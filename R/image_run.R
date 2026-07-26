#' Download and summarize temporal satellite data over polygon regions
#'
#' Downloads bands (and, optionally, vegetation indices) from a Google Earth
#' Engine (GEE) \code{ee$ImageCollection} or single \code{ee$Image} for one or
#' more polygon regions, and returns per-plot summary statistics (mean,
#' median, sd, max, min) over each region/plot/phase combination.
#'
#' @details
#' \strong{Two independent aggregation stages}
#'
#' There are always two distinct kinds of aggregation happening in this
#' function, and it is important not to confuse them:
#' \enumerate{
#'   \item \strong{Temporal aggregation} — collapsing several images/dates
#'   inside a phase (or the whole interval, if \code{phases_df = NULL}) down
#'   to a single representative value per pixel. Controlled by
#'   \code{mean_or_median} together with \code{summarize_raster}.
#'   \item \strong{Spatial aggregation} — collapsing all pixels that fall
#'   inside a given plot polygon down to summary statistics (mean, median,
#'   sd, max, min). This always happens, regardless of \code{summarize_raster},
#'   and is not controlled by \code{mean_or_median}.
#' }
#'
#' \strong{Why \code{summarize_raster} exists}
#'
#' When vegetation indices are requested (\code{vis_df}), the order in which
#' the temporal aggregation happens relative to the index calculation
#' matters:
#' \itemize{
#'   \item If the raster is temporally summarized (mean/median across images)
#'   \emph{before} the index is calculated, the index is computed from
#'   already-averaged band values. Since vegetation indices are frequently
#'   non-linear functions of the bands, this can push the resulting index
#'   values outside their expected parametric range.
#'   \item If the index is instead calculated on \emph{every individual
#'   image} first, and only afterwards summarized (mean/median) across
#'   images, each index value stays within its valid range, and the temporal
#'   summary is a summary of valid index values.
#' }
#'
#' Setting \code{summarize_raster = FALSE} switches to the second behavior.
#' It is more expensive (every image in the interval must be downloaded
#' individually, instead of a single pre-reduced image per phase), so it
#' should be used mainly when \code{vis_df} is supplied and index accuracy
#' matters more than download cost.
#'
#' \strong{Native scale vs. output scale}
#'
#' The download from Earth Engine \emph{always} happens at the asset's
#' native resolution (auto-detected via \code{\link{get_native_scale}}),
#' regardless of \code{use_native_scale} or \code{target_scale_m} — this
#' keeps server-side work to a minimum. \code{use_native_scale} only
#' controls what \emph{local} resampling scale is applied afterward, via
#' \code{terra::project()}: the native scale itself (\code{TRUE}) or
#' \code{target_scale_m} (\code{FALSE}).
#'
#' \strong{Download buffer vs. region buffer}
#'
#' \code{region_buffer_m} (or the value derived automatically when
#' \code{buffer_from_native_scale = TRUE}) is used to build the extent
#' requested from Earth Engine. If that buffer is smaller than one native
#' pixel, it is internally raised to the native pixel size just for that
#' download extent, so that \code{clip()} does not mask away an entire
#' native pixel and return an all-NA raster. This safety adjustment never
#' changes the buffer value the user supplied for anything other than
#' building the download extent.
#'
#' \strong{Memory usage with many images}
#'
#' When \code{summarize_raster = FALSE}, every individual image in a phase
#' contributes its own set of bands to the combined download image (via
#' \code{toBands()}), so both the Earth Engine request and the raster that
#' comes back into R grow with images x bands. Over a long period with many
#' images, this can exhaust memory or cause Earth Engine request failures.
#' \code{batch_size} addresses this directly: instead of asking Earth
#' Engine for one single combined image per phase, images are grouped into
#' smaller batches, each downloaded separately and then stitched back
#' together locally with \code{terra}. This keeps each individual Earth
#' Engine request small. The function also calls \code{gc()} between
#' batches and at the end of each region to release memory as soon as
#' possible. If memory problems persist even with batching, consider
#' shortening \code{phases_df} intervals (fewer images per phase), lowering
#' \code{max_pixels}, or processing regions in smaller groups across
#' separate calls to the function.
#'
#' @param shapefiles A named list of \code{sf} objects, one per region.
#' @param asset_bands_ic An \code{ee$ImageCollection} or a single
#'   \code{ee$Image} to download bands from.
#' @param vis_df Optional \code{data.frame} with columns \code{Index} and
#'   \code{Equation} describing vegetation indices to compute (see
#'   \code{\link{calc_vis_4}}). If \code{NULL}, only the raw bands are
#'   returned.
#' @param phases_df Optional \code{data.frame} with columns \code{Year},
#'   \code{Phase}, \code{start_date}, \code{end_date} defining phenological
#'   phases/intervals to summarize separately. If \code{NULL}, the whole
#'   collection is treated as a single interval.
#' @param mean_or_median Either \code{"mean"} or \code{"median"}. Defines how
#'   multiple images within a phase are collapsed into one representative
#'   value per pixel (temporal aggregation only — see Details).
#' @param summarize_raster Logical, default \code{TRUE}. If \code{TRUE}, each
#'   phase's image collection is reduced (via \code{mean_or_median}) to a
#'   single image \emph{before} download and before any vegetation index is
#'   calculated. If \code{FALSE}, every individual image in the phase is
#'   downloaded and vegetation indices are calculated per image; the
#'   temporal aggregation (\code{mean_or_median}) is then applied afterward,
#'   per pixel, across those per-image index values. See Details.
#' @param batch_size Optional integer. Only relevant when
#'   \code{summarize_raster = FALSE} (individual images are downloaded
#'   rather than a single temporally-reduced image per phase). When a
#'   phase's image count exceeds \code{batch_size}, images are grouped into
#'   smaller Earth Engine download requests instead of one large combined
#'   request, and the results are stitched back together locally. See
#'   Details ("Memory usage with many images").
#' @param target_scale_m Numeric, desired output pixel size in meters for the
#'   final local product. Ignored when \code{use_native_scale = TRUE}.
#' @param use_native_scale Logical, default \code{TRUE}. If \code{TRUE}, the
#'   asset's native resolution (auto-detected via \code{get_native_scale()})
#'   is used as the final local output scale instead of \code{target_scale_m}.
#'   Download from Earth Engine always happens at the native resolution
#'   regardless of this argument; this only affects local resampling.
#' @param min_output_cells Integer, default 5. Minimum number of local output
#'   cells required across the smallest dimension of each region, used to
#'   automatically refine \code{target_scale_m}/native scale locally when the
#'   region is too small for the requested scale.
#' @param region_buffer_m Numeric buffer, in meters, applied to each region
#'   polygon when building the Earth Engine download extent. If smaller than
#'   one native pixel, it is automatically raised to the native pixel size so
#'   that \code{clip()} does not mask out the entire native pixel before
#'   download.
#' @param buffer_from_native_scale Logical, default \code{FALSE}. If
#'   \code{TRUE}, \code{region_buffer_m} is derived automatically from the
#'   native pixel size instead of using a fixed value.
#' @param crs Target coordinate reference system, e.g. \code{"EPSG:32723"}.
#' @param projection_method Resampling method passed to \code{terra::project()}.
#' @param division_scale Optional numeric scalar or vector used to divide the
#'   downloaded raster values (e.g. to convert digital numbers to
#'   reflectance).
#' @param addition_scale Optional numeric scalar or vector added to the
#'   downloaded raster values after \code{division_scale} is applied.
#' @param interpolate_bands Logical. If \code{TRUE}, small pockets of NA
#'   cells are filled with a 3x3 focal mean.
#' @param valid_values_threshold Numeric in \code{[0, 1]}. Minimum proportion of
#'   non-NA cells required per plot/phase for that combination to be flagged
#'   \code{Valid = TRUE} in the returned \code{Validation} table.
#' @param mask Optional \code{ee$Image} used to mask out pixels before
#'   download. Build it however you like outside the function -- e.g. chain
#'   \code{$neq()}/\code{$And()} conditions on a land-cover collection such
#'   as MapBiomas to exclude classes you are not interested in (water,
#'   urban, forest, etc.), or derive it from any other Earth Engine
#'   collection you have access to. It is clipped to each region's download
#'   extent internally and applied via \code{updateMask()}, so a single
#'   mask is automatically adapted to every region in \code{shapefiles}. If
#'   \code{NULL} (default), no mask is applied.
#' @param download_route Passed to \code{rgee::ee_as_rast()} as \code{via},
#'   e.g. \code{"drive"} or \code{"gcs"}.
#'   \itemize{
#'     \item \code{"drive"} (default): the image is exported to a folder in
#'     your Google Drive and downloaded from there. Works out of the box with
#'     the same OAuth authentication used by \code{ee_Initialize()}.
#'     \item \code{"gcs"}: the image is exported to a Google Cloud Storage
#'     bucket instead. Requires billing enabled on the GCP project and an
#'     existing bucket -- see \code{gcs_bucket}. Authenticate either with
#'     \code{ee_Initialize(gcs = TRUE)} or a Service Account Key registered
#'     via \code{rgee::ee_utils_sak_copy()}/\code{rgee::ee_utils_sak_validate()}.
#'   }
#' @param gcs_bucket Character, name of an existing Google Cloud Storage
#'   bucket to use as the export container. Required when
#'   \code{download_route == "gcs"}; ignored otherwise (the Drive route
#'   always uses the \code{"rgee_backup"} container name). Passed to
#'   \code{rgee::ee_as_rast()} as \code{container}.
#' @param max_pixels Passed to \code{rgee::ee_as_rast()} as \code{maxPixels}.
#' @param clean_container Logical, default \code{TRUE}. If \code{TRUE},
#'   calls \code{rgee::ee_clean_container()} after each region's download,
#'   targeting the Drive folder or GCS bucket matching \code{download_route}
#'   (\code{"rgee_backup"} for \code{"drive"}, \code{gcs_bucket} for
#'   \code{"gcs"}).
#' @param save_raster Logical, default \code{FALSE}. If \code{TRUE}, the
#'   final processed raster for each region -- the same object returned in
#'   \code{$Raster}, i.e. the version right before it is turned into a
#'   data.frame -- is written to disk as a GeoTIFF.
#' @param raster_output_path Optional character path to a folder where
#'   rasters are saved when \code{save_raster = TRUE}. If \code{NULL}
#'   (default), a \code{"raster_outputs"} folder is created inside the
#'   current working directory. Each file is named with the region, a date
#'   and a random suffix -- the same suffix shared with \code{Data},
#'   \code{ImageCount} and \code{Validation} for that region/run, see
#'   \code{save_data} -- so repeated runs, or multiple regions in the same
#'   run, never overwrite each other.
#' @param save_data Character vector, default \code{"none"}. Controls
#'   whether -- and in which format(s) -- the final per-region \code{Data},
#'   \code{ImageCount} and \code{Validation} tibbles (the same objects
#'   returned in \code{$Data}, \code{$ImageCount} and \code{$Validation})
#'   are written to disk. One or more of:
#'   \itemize{
#'     \item \code{"none"}: nothing is saved. Ignored (with a warning) if
#'     combined with any other format.
#'     \item \code{"qs"}: saved with \code{qs2::qs_save()}.
#'     \item \code{"parquet"}: saved with \code{arrow::write_parquet()}.
#'     \item \code{"csv"}: saved with \code{readr::write_csv()}.
#'     \item \code{"xlsx"}: saved with \code{writexl::write_xlsx()}.
#'   }
#'   Multiple formats can be requested at once, e.g.
#'   \code{save_data = c("xlsx", "qs")}, and all of them are written. Each
#'   tibble gets its own subfolder inside \code{data_output_path}, and each
#'   format its own subfolder inside that (e.g. \code{"Data/QS"},
#'   \code{"ImageCount/CSV"}, \code{"Validation/XLSX"}), created
#'   automatically as needed.
#' @param data_output_path Optional character path to the base folder used
#'   when \code{save_data} requests at least one format. If \code{NULL}
#'   (default), the current working directory is used as the base -- e.g.
#'   \code{save_data = "qs"} creates \code{"Data/QS"}, \code{"ImageCount/QS"}
#'   and \code{"Validation/QS"} inside it. If set to a path, those same
#'   per-tibble/per-format subfolders are created inside that path instead.
#'
#' @return A named list, one entry per region in \code{shapefiles}, each
#'   containing:
#'   \describe{
#'     \item{Raster}{The downloaded and processed \code{SpatRaster}.}
#'     \item{Data}{A \code{tibble} with per-plot (and, if \code{phases_df} is
#'     supplied, per-phase) summary statistics for every numeric band and
#'     vegetation index.}
#'     \item{ImageCount}{A \code{tibble} with the number of images used per
#'     plot/phase.}
#'     \item{Validation}{A \code{tibble} flagging plot/phase combinations
#'     with too many NA cells, per \code{valid_values_threshold}.}
#'   }
#'
#' @examples
#' \dontrun{
#'   # rgee must already be initialized in this session with your own
#'   # Google Earth Engine credentials and Cloud project, e.g.:
#'   # rgee::ee_Initialize(project = "your-gee-project-id")
#'
#'   # -- 1. Build a mask to exclude land-cover classes you're not
#'   #       interested in (here: urban, water, forest, savanna, forest
#'   #       plantation, restinga), using MapBiomas collection 9 --
#'   mapbiomas <-
#'     rgee::ee$Image(
#'       paste0(
#'         "projects/mapbiomas-public/assets/brazil/lulc/collection9/",
#'         "mapbiomas_collection90_integration_v1"
#'       )
#'     )$select("classification_2023")
#'
#'   classes_to_exclude <- c(24, 33, 3, 4, 9, 62)
#'
#'   exclusion_mask <-
#'     Reduce(
#'       function(m, class_id) m$And(mapbiomas$neq(class_id)),
#'       classes_to_exclude[-1],
#'       mapbiomas$neq(classes_to_exclude[1])
#'     )
#'
#'   # -- 2. A small Sentinel-2 SR collection, filtered to one growing
#'   #       season, so there's more than one image per phase (useful to
#'   #       also exercise batch_size below) --
#'   s2_ic <-
#'     rgee::ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")$
#'     filterDate("2023-10-01", "2024-03-31")$
#'     filter(rgee::ee$Filter$lt("CLOUDY_PIXEL_PERCENTAGE", 20))
#'
#'   # -- 3. Vegetation indices and phenological phases --
#'   vis_df <- data.frame(
#'     Index = c("NDVI", "GNDVI"),
#'     Equation = c("(NIR - R) / (NIR + R)", "(NIR - G) / (NIR + G)")
#'   )
#'
#'   phases_df <- data.frame(
#'     Year = 2023,
#'     Phase = c("Vegetative", "Flowering"),
#'     start_date = c("2023-10-01", "2023-12-01"),
#'     end_date = c("2023-11-30", "2024-01-31")
#'   )
#'
#'   # `shapefiles` is a named list of sf polygons, one per region/farm,
#'   # each with an "id" column identifying individual plots.
#'   # shapefiles <- list(FarmA = sf_farm_a, FarmB = sf_farm_b)
#'
#'   result <-
#'     get_gee_data(
#'       shapefiles       = shapefiles,
#'       asset_bands_ic   = s2_ic,
#'       vis_df           = vis_df,
#'       phases_df        = phases_df,
#'       summarize_raster = FALSE,   # per-image indices -> needed for batch_size
#'       batch_size       = 5,       # download 5 images per EE request at a time
#'       mask             = exclusion_mask,
#'       save_raster      = TRUE,
#'       raster_output_path = "outputs/s2_2023_2024/rasters",
#'       save_data        = c("xlsx", "qs"), # write both formats at once
#'       data_output_path = "outputs/s2_2023_2024/data"
#'     )
#'
#'   # Per-plot/per-phase summary statistics
#'   result$FarmA$Data
#'
#'   # Rasters, Data, ImageCount, and Validation were also written to disk,
#'   # one file per region, sharing a single random suffix per region so
#'   # files from the same run can be matched up, e.g.
#'   # "outputs/s2_2023_2024/rasters/raster_FarmA_2026_07_20_0417.tif"
#'   # "outputs/s2_2023_2024/data/Data/XLSX/data_FarmA_2026_07_20_0417.xlsx"
#'   # "outputs/s2_2023_2024/data/Data/QS/data_FarmA_2026_07_20_0417.qs"
#'   list.files("outputs/s2_2023_2024", recursive = TRUE)
#' }
#'
#' @export
get_gee_data <-
  function(
    shapefiles,
    asset_bands_ic,
    vis_df = NULL,
    phases_df = NULL,
    mean_or_median = "mean",
    summarize_raster = TRUE,
    batch_size = NULL,
    target_scale_m = 5L,
    use_native_scale = TRUE,
    min_output_cells = 5,
    region_buffer_m = NULL,
    buffer_from_native_scale = FALSE,
    crs = "EPSG:32723",
    projection_method = "bilinear",
    division_scale = NULL,
    addition_scale = NULL,
    interpolate_bands = FALSE,
    valid_values_threshold = 0.1,
    mask = NULL,
    download_route = "drive",
    gcs_bucket = NULL,
    max_pixels = 1e12,
    clean_container = TRUE,
    save_raster = FALSE,
    raster_output_path = NULL,
    save_data = "none",
    data_output_path = NULL
  ) {


    valid_save_formats <- c("none", "qs", "parquet", "csv", "xlsx")

    if (!all(save_data %in% valid_save_formats)) {

      stop(
        "save_data must be one or more of: ",
        paste(valid_save_formats, collapse = ", "), "."
      )

    }

    if (length(save_data) > 1 && "none" %in% save_data) {

      save_data <- setdiff(save_data, "none")

      warning(
        "\"none\" was included in save_data together with other formats; ",
        "it will be ignored. Data will be saved as: ",
        paste(save_data, collapse = ", "), "."
      )

    }

    save_formats <- unique(save_data[save_data != "none"])


    if (download_route == "gcs" && is.null(gcs_bucket)) {

      stop(
        "download_route = \"gcs\" requires gcs_bucket (the name of an ",
        "existing Google Cloud Storage bucket). Create one and pass its ",
        "name, e.g. gcs_bucket = \"my-bucket\". See rgee::ee_utils_sak_copy() ",
        "/ rgee::ee_utils_sak_validate() for GCS authentication setup."
      )

    }

    # The container rgee exports into: a Drive folder name for "drive", or a
    # GCS bucket name for "gcs". Shared between the ee_as_rast() calls below
    # and the matching ee_clean_container() cleanup, so both always target
    # the same place.
    container_name <- if (download_route == "gcs") gcs_bucket else "rgee_backup"


    is_image_collection <-
      inherits(asset_bands_ic, "ee.imagecollection.ImageCollection")


    # Raw per-image processing only makes sense for an ImageCollection: a
    # single ee$Image has no temporal dimension to summarize.
    use_raw_images <- !summarize_raster && is_image_collection


    if (!is.null(phases_df)) {

      phase_tb <- phases_df |>
        dplyr::select(Year, Phase, start_date, end_date)

    }


    # Extract band names from the asset (collection or single image)
    if (is_image_collection) {
      ee_bands_object <- asset_bands_ic$first()$bandNames()
    } else {
      ee_bands_object <- asset_bands_ic$bandNames()
    }

    # Bring the band names from the Earth Engine server to R
    available_bands <- ee_bands_object$getInfo()


    sat_bands <-
      tryCatch(

        get_bands(asset_bands_ic),

        error = function(e) {

          message(
            "Band extraction error caused by unresolved image or collection ",
            "provenance. Switching to manual extraction."
          )

          manual_bands <-
            if (is_image_collection) {

              asset_bands_ic$first()$bandNames()$getInfo()

            } else {

              asset_bands_ic$bandNames()$getInfo()

            }

          manual_bands |>
            setNames(manual_bands)

        }
      )


    # Two explicit scales are used throughout this function:
    #   - download_scale: ALWAYS the asset's native resolution, obtained via
    #     get_native_scale(). This is what is actually requested from Earth
    #     Engine — the minimum amount of server-side work possible.
    #   - output_scale: the resolution the user actually wants in the final
    #     product (= target_scale_m, or = download_scale if
    #     use_native_scale = TRUE). Resampling to output_scale is done
    #     locally afterward, via terra::project(), which is orders of
    #     magnitude cheaper than server-side reprojection on Earth Engine.
    download_scale <-
      tryCatch(
        get_native_scale(asset_bands_ic),
        error = function(e) NULL
      )

    if (is.null(download_scale)) {

      message(
        "Could not determine native scale automatically. ",
        "Falling back to target_scale_m (", target_scale_m,
        ") for the Earth Engine download."
      )

      download_scale <- target_scale_m

    }

    output_scale <- if (use_native_scale) download_scale else target_scale_m


    region_fc_list <-
      shapefiles |>
      purrr::map(~ ee$FeatureCollection(rgee::sf_as_ee(.x)))


    total_regions <- length(region_fc_list)

    results <-
      region_fc_list |>
      purrr::imap(\(region_fc, region) {

        region_idx <- match(region, names(region_fc_list))

        message(
          sprintf(
            "Processing region %d of %d: %s",
            region_idx, total_regions, region
          )
        )


        region_sf <- shapefiles[[region]]


        region_sf <- region_sf[!duplicated(sf::st_geometry(region_sf)), ]


        region_sf_reproj <-
          tryCatch(

            sf::st_transform(region_sf, crs),

            error = function(e) {

              message(
                "Error reprojecting shapefile for ", region, ": ",
                e$message, "."
              )

              NULL

            }
          )


        if (is.null(region_sf_reproj)) {

          message("Shapefile reprojection failed for ", region, ".")

          return(list(
            Raster = NULL,
            Data = NULL,
            ImageCount = NULL,
            Validation = NULL
          ))

        }


        region_fc <- ee$FeatureCollection(rgee::sf_as_ee(region_sf_reproj))


        if (!"id" %in% names(region_sf_reproj)) {

          message("No 'id' column found for ", region, ". Assigning default IDs.")

          region_sf_reproj$id <- seq_len(nrow(region_sf_reproj))

          region_fc <- ee$FeatureCollection(rgee::sf_as_ee(region_sf_reproj))

        }


        plot_ids <- region_fc$aggregate_array("id")$distinct()$getInfo()


        # Automatic, generic local refinement (not asset-specific).
        # download_scale already guarantees a cheap native-resolution
        # download from GEE. Here we make sure the scale used LOCALLY to
        # build the template/plot_ids_base is fine enough to distinguish
        # the plots in this particular region — regardless of which asset
        # is used (TerraClimate, MODIS, Sentinel, etc.), and regardless of
        # use_native_scale/target_scale_m. If output_scale is coarser than
        # what this region's extent supports, we refine it downward — always
        # with a local terra::project() step, never by reprocessing anything
        # in the cloud.
        region_output_scale <- output_scale

        region_ext_vec <- as.vector(terra::ext(region_sf_reproj))

        region_extent_x <- region_ext_vec["xmax"] - region_ext_vec["xmin"]
        region_extent_y <- region_ext_vec["ymax"] - region_ext_vec["ymin"]

        region_smallest_extent <- min(region_extent_x, region_extent_y)

        max_allowed_output_scale <- region_smallest_extent / min_output_cells

        if (region_output_scale > max_allowed_output_scale) {

          message(
            "Region '", region, "': output scale (", region_output_scale,
            " m) is coarser than this region's extent supports -- ",
            "refining locally to ", round(max_allowed_output_scale, 3),
            " m so individual plots stay distinguishable (at least ",
            min_output_cells, " cells across the smallest dimension). ",
            "Download still happens at the native scale (", download_scale,
            " m); this refinement is a local terra::project() step only, ",
            "at no extra Earth Engine cost."
          )

          region_output_scale <- max_allowed_output_scale

        }


        if (buffer_from_native_scale) {

          region_buffer_m <- download_scale

          message(
            "Using resolution-based buffer of ", region_buffer_m,
            " units for ", region
          )

        }


        # The buffer used to build the DOWNLOAD region can never be smaller
        # than one native pixel (download_scale). This guarantees that
        # clip() always preserves at least one whole pixel: if the buffer
        # requested for local output were used directly for the download
        # region too, and that buffer is smaller than the native pixel,
        # clip() would mask away the entire native pixel and the download
        # would come back completely empty (all-NA), no matter how well the
        # result is refined locally afterward. The user's original
        # region_buffer_m is still respected whenever it is already
        # >= download_scale.
        download_buffer_dist <-
          if (is.null(region_buffer_m)) {

            download_scale

          } else if (region_buffer_m < download_scale) {

            message(
              "Region '", region, "': region_buffer_m (", region_buffer_m,
              " m) is smaller than the native pixel size (", download_scale,
              " m) -- using ", download_scale, " m to build the Earth ",
              "Engine download region, so clip() doesn't mask away the ",
              "whole native pixel. This only affects the region sent to ",
              "Earth Engine for download; local refinement afterward still ",
              "targets the resolution you requested."
            )

            download_scale

          } else {

            region_buffer_m

          }


        download_region <-
          region_fc$geometry()$buffer(download_buffer_dist)


        template_rast <-
          terra::rast(
            extent = terra::ext(region_sf_reproj),
            resolution = region_output_scale / 2,
            crs = crs
          )


        plot_ids_base <-
          terra::rasterize(
            region_sf_reproj,
            template_rast,
            field = "id",
            fun = "min",
            background = NA
          )


        # Residual safety net: the automatic refinement block above already
        # guarantees min_output_cells cells across the smallest dimension,
        # so this case should no longer occur under normal conditions. Kept
        # only so the function doesn't fail opaquely (the original error was
        # "[focal] nrow(w) > 2 * nrow(x)") in very atypical situations (e.g.
        # a degenerate geometry, or min_output_cells set too low by the
        # user). Skip just this region and move on.
        if (all(is.na(terra::values(plot_ids_base)))) {

          message(
            "Region '", region, "': skipping -- could not rasterize plot ",
            "IDs even after local refinement to ", region_output_scale,
            " m. The template raster has only ", terra::nrow(template_rast),
            " x ", terra::ncol(template_rast), " cell(s), and no pixel ",
            "center falls inside any polygon. This is unexpected given ",
            "min_output_cells = ", min_output_cells, "; check for ",
            "degenerate/very small geometries in this region."
          )

          return(list(
            Raster = NULL,
            Data = NULL,
            ImageCount = NULL,
            Validation = NULL
          ))

        }


        # Additional protection: even with valid data, a template_rast with
        # too few rows/columns may not fit a 3x3 focal window. Rather than
        # letting terra throw a low-level error, we skip smoothing in that
        # specific case.
        min_dim <- min(terra::nrow(plot_ids_base), terra::ncol(plot_ids_base))

        if (min_dim < 2) {

          message(
            "Region '", region, "': template raster too small (",
            terra::nrow(plot_ids_base), " x ", terra::ncol(plot_ids_base),
            " cell(s)) for a 3x3 focal window; skipping focal smoothing ",
            "for plot_ids_base."
          )

        } else {

          plot_ids_base <-
            terra::focal(
              plot_ids_base,
              w = 3,
              fun = "min",
              na.rm = TRUE,
              na.policy = "only"
            )

        }


        names(plot_ids_base) <- "Plot"


        if (is_image_collection) {

          ic <- asset_bands_ic$filterBounds(download_region)

          ic_size <- ic$size()$getInfo()

          if (ic_size == 0) {

            message("No images found for ", region, ".")

            return(list(
              Raster = NULL,
              Data = NULL,
              ImageCount = NULL,
              Validation = NULL
            ))

          }

          if (is.null(phases_df)) {

            if (summarize_raster) {

              image <- if (mean_or_median == "mean") ic$mean() else ic$median()

              phase_images <-
                list(list(Image = image, ImageCount = ic_size, Phase = "All"))

            } else {

              # Download every individual image, tagging each one's bands
              # with its index within the collection so they can be told
              # apart after the download and pivoted back into a per-image,
              # per-pixel long table.
              img_list <- ic$toList(ic_size)

              images <-
                purrr::map(seq_len(ic_size) - 1L, \(i) {

                  img <- ee$Image(img_list$get(i))

                  new_bands <-
                    paste0(sprintf("%05d", i), "_", img$bandNames()$getInfo())

                  img$rename(new_bands)

                })

              phase_images <-
                list(list(Images = images, ImageCount = ic_size, Phase = "All"))

            }

          } else {

            if (summarize_raster) {

              phase_images <-
                phases_df |>
                dplyr::select(Phase, start_date, end_date) |>
                purrr::pmap(\(Phase, start_date, end_date) {

                  ic_phase <-
                    ic$filterDate(as.character(start_date), as.character(end_date))

                  phase_size <- ic_phase$size()$getInfo()

                  if (phase_size == 0)
                    return(list(
                      Image = NULL,
                      ImageCount = 0,
                      Phase = Phase
                    ))

                  image <-
                    if (mean_or_median == "mean") {

                      ic_phase$mean()

                    } else {

                      ic_phase$median()

                    }

                  new_bands <- paste0(Phase, "_", image$bandNames()$getInfo())

                  list(
                    Image = image$rename(new_bands),
                    ImageCount = phase_size,
                    Phase = Phase
                  )

                })

            } else {

              phase_images <-
                phases_df |>
                dplyr::select(Phase, start_date, end_date) |>
                purrr::pmap(\(Phase, start_date, end_date) {

                  ic_phase <-
                    ic$filterDate(as.character(start_date), as.character(end_date))

                  phase_size <- ic_phase$size()$getInfo()

                  if (phase_size == 0)
                    return(list(
                      Images = NULL,
                      ImageCount = 0,
                      Phase = Phase
                    ))

                  img_list <- ic_phase$toList(phase_size)

                  images <-
                    purrr::map(seq_len(phase_size) - 1L, \(i) {

                      img <- ee$Image(img_list$get(i))

                      new_bands <-
                        paste0(
                          Phase, "_", sprintf("%05d", i), "_",
                          img$bandNames()$getInfo()
                        )

                      img$rename(new_bands)

                    })

                  list(
                    Images = images,
                    ImageCount = phase_size,
                    Phase = Phase
                  )

                })

            }

          }

        } else {

          phase_images <-
            list(list(Image = asset_bands_ic, ImageCount = 1, Phase = "All"))

        }


        if (use_raw_images) {

          valid_images <-
            phase_images |>
            purrr::map("Images") |>
            purrr::compact() |>
            purrr::flatten()

        } else {

          valid_images <-
            phase_images |>
            purrr::map("Image") |>
            purrr::compact()

        }


        image_counts <-
          phase_images |>
          purrr::map("ImageCount")


        phase_names <-
          phase_images |>
          purrr::map_chr(\(x) as.character(x[["Phase"]]))


        if (length(valid_images) == 0) {

          message("No valid images for ", region, ".")

          return(list(
            Raster = NULL,
            Data = NULL,
            ImageCount = NULL,
            Validation = NULL
          ))

        }


        n_valid_images <- length(valid_images)

        # When summarize_raster = FALSE, every individual image contributes
        # its own bands to the combined download image, so the request
        # sent to Earth Engine (and the raster that comes back) grows with
        # images x bands. use_batches splits that single large request into
        # several smaller ones -- each downloaded and combined locally --
        # which keeps peak memory and per-request Earth Engine load down.
        # See Details ("Memory usage with many images").
        use_batches <-
          use_raw_images && !is.null(batch_size) && n_valid_images > batch_size

        if (use_batches) {

          image_batches <-
            split(
              seq_len(n_valid_images),
              ceiling(seq_len(n_valid_images) / batch_size)
            )

          n_batches <- length(image_batches)

          raster_list <-
            purrr::imap(image_batches, \(idx_batch, batch_i) {

              message(
                "  Region '", region, "': downloading batch ", batch_i,
                " of ", n_batches, " (", length(idx_batch), " image(s))."
              )

              batch_images <- valid_images[idx_batch]

              batch_band_names <-
                batch_images |>
                purrr::map(~ .x$bandNames()$getInfo()) |> unlist()

              batch_final_image <-
                ee$ImageCollection(batch_images)$
                toBands()$rename(batch_band_names)$clip(download_region)

              if (!is.null(mask)) {

                batch_final_image <-
                  batch_final_image$updateMask(mask$clip(download_region))

              }

              batch_raster <-
                tryCatch(
                  rgee::ee_as_rast(
                    image = batch_final_image,
                    region = download_region,
                    scale = download_scale,
                    via = download_route,
                    container = container_name,
                    crs = crs,
                    maxPixels = max_pixels
                  ),
                  error = function(e) {
                    message(
                      "Error downloading batch ", batch_i, " for ", region,
                      ": ", e$message
                    )
                    NULL
                  }
                )

              if (clean_container && download_route %in% c("drive", "gcs")) {

                rgee::ee_clean_container(name = container_name, type = download_route)

              }

              gc()

              batch_raster

            })

          raster_list <- purrr::compact(raster_list)

          raster <-
            if (length(raster_list) == 0) {

              NULL

            } else {

              tryCatch(
                do.call(c, raster_list),
                error = function(e) {
                  message(
                    "Error combining batches into one raster for ", region,
                    ": ", e$message
                  )
                  NULL
                }
              )

            }

          rm(raster_list)
          gc()

        } else {

          band_names <-
            valid_images |>
            purrr::map(~ .x$bandNames()$getInfo()) |> unlist()


          final_image <-
            ee$ImageCollection(valid_images)$
            toBands()$rename(band_names)$clip(download_region)

          if (!is.null(mask)) {

            final_image <- final_image$updateMask(mask$clip(download_region))

          }


          raster <-
            tryCatch(
              rgee::ee_as_rast(
                image = final_image,
                region = download_region,
                scale = download_scale,
                via = download_route,
                container = container_name,
                crs = crs,
                maxPixels = max_pixels
              ),
              error = function(e) {
                message("Error downloading raster for ", region, ": ", e$message)
                NULL
              }
            )


          if (clean_container && download_route %in% c("drive", "gcs")) {

            rgee::ee_clean_container(name = container_name, type = download_route)

          } else if (download_route %in% c("drive", "gcs")) {

            message(
              "clean_container = FALSE: keeping files on ",
              if (download_route == "gcs") "Google Cloud Storage" else "Google Drive",
              " for ", region, "."
            )

          }

        }


        if (is.null(raster) || !inherits(raster, "SpatRaster")) {

          message("Raster download failed for ", region, ".")

          return(list(
            Raster = NULL,
            Data = NULL,
            ImageCount = NULL,
            Validation = NULL
          ))

        }


        raster <-
          if (!is.null(division_scale) && all(division_scale > 0)) {

            raster <-
              if (length(division_scale) == 1 ||
                  length(division_scale) == length(available_bands)) {

                raster / division_scale

              } else {

                warning(
                  paste0(
                    "The number of elements in the division_scale vector must be 1 ",
                    "or equal to the number of bands available in a single image.\n",
                    "The process will continue without using the division_scale argument."
                  )
                )

                raster

              }

            raster

          } else {

            raster

          }


        raster <-
          if (!is.null(addition_scale) && all(is.numeric(addition_scale))) {

            raster <-
              if (length(addition_scale) == 1 ||
                  length(addition_scale) == length(available_bands)) {

                raster + addition_scale

              } else {

                warning(
                  paste0(
                    "The number of elements in the addition_scale vector must be 1 ",
                    "or equal to the number of bands available in a single image.\n",
                    "The process will continue without using the addition_scale argument."
                  )
                )

                raster

              }

            raster

          } else {

            raster

          }


        # Local resampling to the final desired scale (output_scale). The
        # raster arrives from GEE already at the (cheap) native scale, and
        # is resampled here, locally, with terra.
        raster <-
          terra::project(raster,
                         template_rast,
                         method = projection_method)


        raster <-
          terra::crop(raster, region_sf_reproj)


        raster <-
          terra::mask(raster, region_sf_reproj)


        if (interpolate_bands) {

          raster <-
            terra::focal(
              raster,
              w = 3,
              fun = "mean",
              na.rm = TRUE,
              na.policy = "only"
            )

          raster <- terra::mask(raster, plot_ids_base)

        }


        raster <- c(plot_ids_base, raster)


        # One random suffix per region/iteration, shared by the raster and
        # by Data/ImageCount/Validation below, so every file produced in
        # this iteration carries a matching identity while still never
        # overwriting files from other regions or other runs.
        run_suffix <- sprintf("%04d", sample.int(9999, 1))


        if (save_raster) {

          output_dir <-
            if (is.null(raster_output_path)) {

              file.path(getwd(), "raster_outputs")

            } else {

              raster_output_path

            }

          if (!dir.exists(output_dir)) {

            dir.create(output_dir, recursive = TRUE)

          }

          raster_filename <-
            file.path(
              output_dir,
              paste0(
                "raster_", region, "_",
                format(Sys.Date(), "%Y_%m_%d"), "_",
                run_suffix,
                ".tif"
              )
            )

          tryCatch(
            {
              terra::writeRaster(raster, raster_filename, overwrite = FALSE)
              message("Region '", region, "': raster saved to ", raster_filename)
            },
            error = function(e) {
              message(
                "Error saving raster for ", region, ": ", e$message
              )
            }
          )

        }


        Data <-
          raster |>
          terra::as.data.frame(xy = TRUE) |>
          tibble::as_tibble() |>
          dplyr::filter_at(
            dplyr::vars(tidyselect::matches("Plot")), ~ !is.na(.))


        if (!interpolate_bands) {

          Data <- Data |>
            dplyr::filter(
              dplyr::if_all(!dplyr::any_of(c("x", "y", "Plot")), ~ !is.na(.))
            )

        }


        if (!is.null(phases_df) && nrow(phases_df) > 0) {

          bands_escaped <- stringr::str_escape(available_bands) |> paste(collapse = "|")
          phases_escaped <- stringr::str_escape(phase_names) |> paste(collapse = "|")

          if (use_raw_images) {

            # (phase)_(image index)_(band), e.g. "Flowering_00003_B4"
            bands_regex <-
              paste0("^(", phases_escaped, ")[_\\.]+(\\d+)[_\\.]+(", bands_escaped, ")$")

            # A phase with zero images (e.g. no images fell inside its
            # date range) never appears in Data at all -- it contributed
            # no bands, hence no pivoted rows. A plain right_join(phase_tb)
            # would still add that phase back in, but since it has no
            # match on the left side, EVERY left-side column -- including
            # Region and Plot -- would be filled with NA, producing a
            # single phantom row with Region = NA, Plot = NA instead of
            # one row per real plot. Building the full Region x Plot x
            # Phase grid up front and left-joining the real data onto it
            # keeps Region/Plot always populated with real identifiers,
            # with NA appearing only in the measurement columns for
            # phases that truly had no data.
            phase_plot_grid <-
              tidyr::expand_grid(
                Region = forcats::as_factor(region),
                Plot = forcats::as_factor(plot_ids),
                phase_tb |> dplyr::mutate(Phase = as.character(Phase))
              )

            Data <- Data |>
              tidyr::pivot_longer(
                cols = !dplyr::any_of(c("x", "y", "Plot")),
                names_to = c("Phase", "ImageIndex", ".value"),
                names_pattern = bands_regex
              ) |>
              dplyr::mutate(
                Plot = forcats::as_factor(Plot),
                ImageIndex = as.integer(ImageIndex)
              ) |>
              tibble::add_column(
                Region = forcats::as_factor(region), .before = "x"
              ) |>
              dplyr::mutate(Phase = as.character(Phase)) |>
              dplyr::full_join(
                phase_plot_grid,
                by = c("Region", "Plot", "Phase")
              ) |>
              dplyr::mutate(Phase = forcats::as_factor(Phase)) |>
              dplyr::relocate("start_date", "end_date", .after = "Phase") |>
              dplyr::relocate("Year", .before = "Phase")

          } else {

            # (phase)_(band), e.g. "Flowering_B4" -- this image is already a
            # per-phase temporal mean/median, computed on the GEE side.
            bands_regex <- paste0("^(", phases_escaped, ")[_\\.]+(", bands_escaped, ")$")

            # See comment above (use_raw_images branch) for why a grid +
            # left_join is used instead of right_join(phase_tb).
            phase_plot_grid <-
              tidyr::expand_grid(
                Region = forcats::as_factor(region),
                Plot = forcats::as_factor(plot_ids),
                phase_tb |> dplyr::mutate(Phase = as.character(Phase))
              )

            Data <- Data |>
              tidyr::pivot_longer(
                cols = !dplyr::any_of(c("x", "y", "Plot")),
                names_to = c("Phase", ".value"),
                names_pattern = bands_regex
              ) |>
              dplyr::mutate(
                Plot = forcats::as_factor(Plot)
              ) |>
              tibble::add_column(
                Region = forcats::as_factor(region), .before = "x"
              ) |>
              dplyr::mutate(Phase = as.character(Phase)) |>
              dplyr::full_join(
                phase_plot_grid,
                by = c("Region", "Plot", "Phase")
              ) |>
              dplyr::mutate(Phase = forcats::as_factor(Phase)) |>
              dplyr::relocate("start_date", "end_date", .after = "Phase") |>
              dplyr::relocate("Year", .before = "Phase")

          }

        } else {

          if (use_raw_images) {

            # (image index)_(band), e.g. "00003_B4"
            bands_escaped <- stringr::str_escape(available_bands) |> paste(collapse = "|")
            bands_regex <- paste0("^(\\d+)[_\\.]+(", bands_escaped, ")$")

            Data <- Data |>
              tidyr::pivot_longer(
                cols = !dplyr::any_of(c("x", "y", "Plot")),
                names_to = c("ImageIndex", ".value"),
                names_pattern = bands_regex
              ) |>
              dplyr::mutate(
                Plot = forcats::as_factor(Plot),
                ImageIndex = as.integer(ImageIndex)
              ) |>
              tibble::add_column(
                Region = forcats::as_factor(region), .before = "x"
              )

          } else {

            Data <-
              Data |>
              dplyr::mutate(Plot = forcats::as_factor(Plot)) |>
              tibble::add_column(
                Region = forcats::as_factor(region), .before = "x"
              )

          }

        }


        if (!is.null(vis_df) && nrow(vis_df) > 0) {

          required_bands <- extract_required_bands(vis_df$Equation)

          message(
            "Required bands: ", paste(required_bands, collapse = ", "), "."
          )

          if (is.null(available_bands)) {

            stop("Could not retrieve band names from asset.")

          }

          required_sat_bands <- sat_bands[names(sat_bands) %in% required_bands]

          missing_bands <- required_bands[!required_bands %in% names(sat_bands)]

          if (length(missing_bands) > 0) {

            message(
              "Warning: Missing bands for VIs: ",
              paste(missing_bands, collapse = ", ")
            )

            lines_new_vis_df <-
              !grepl(paste(missing_bands, collapse = "|"), vis_df$Equation)

            message(
              "Excluding indices requiring missing bands: ",
              paste(vis_df$Index[!lines_new_vis_df], collapse = ", "), "."
            )

            vis_df <-
              vis_df[lines_new_vis_df, ]

          }

          # calc_vis_4() is applied row-wise, so at this point it computes
          # each vegetation index on whatever a row represents: a single raw
          # image (when use_raw_images = TRUE) or an already
          # temporally-summarized pixel (when use_raw_images = FALSE). This
          # is exactly what makes summarize_raster = FALSE keep index values
          # within their parametric space -- indices are always computed
          # from a real, individual image, never from a pixel that is
          # itself a mean/median of several images.
          Data <- Data |>
            dplyr::rename(!!!required_sat_bands) |>
            calc_vis_4(vis_df)

        } else {

          Data <- Data |>
            dplyr::rename(!!!sat_bands)

        }


        # When individual images were downloaded (use_raw_images = TRUE),
        # collapse them now into one representative value per pixel per
        # phase/plot, using mean_or_median. This is the temporal aggregation
        # step requested by the user, applied AFTER vegetation indices were
        # computed per image, so index values themselves are never averaged
        # pre-emptively at the raw-band level.
        if (use_raw_images) {

          temporal_group_cols <-
            if (!is.null(phases_df) && nrow(phases_df) > 0) {

              c("Region", "Plot", "Year", "Phase", "start_date", "end_date", "x", "y")

            } else {

              c("Region", "Plot", "x", "y")

            }

          temporal_fn <- if (mean_or_median == "mean") mean else stats::median

          Data <- Data |>
            dplyr::group_by(dplyr::across(dplyr::any_of(temporal_group_cols))) |>
            dplyr::summarise(
              dplyr::across(
                .cols = tidyselect::where(is.numeric) & -dplyr::any_of(c("ImageIndex")),
                .fns = ~ temporal_fn(.x, na.rm = TRUE)
              ),
              .groups = "drop"
            )

        }


        if (is_image_collection && !is.null(phases_df)) {

          ImageCount <-
            tibble::tibble(
              Region = region,
              Year = phases_df$Year,
              Phase = phases_df$Phase,
              start_date = phases_df$start_date,
              end_date = phases_df$end_date,
              Plot = list(plot_ids),
              ImageCount = unlist(image_counts)
            ) |> tidyr::unnest(Plot)


          Validation <-
            Data |>
            dplyr::group_by(Region, Plot, Year, Phase, start_date, end_date) |>
            dplyr::summarise(
              NA_count = sum(is.na(dplyr::across(dplyr::where(is.numeric) & -c("x", "y")))),
              total_cells = n() * length(names(dplyr::pick(dplyr::where(is.numeric) & -c("x", "y")))),
              valid_ratio = (total_cells - NA_count) / total_cells,
              Valid = valid_ratio >= valid_values_threshold,
              .groups = "drop"
            )


        } else {


          ImageCount <-
            tibble::tibble(
              Region = region,
              Plot = plot_ids,
              ImageCount = unlist(image_counts)
            )


          Validation <-
            Data |>
            dplyr::group_by(Region, Plot) |>
            dplyr::summarise(
              NA_count = sum(is.na(dplyr::across(dplyr::where(is.numeric) & -c("x", "y")))),
              total_cells = n() * length(names(dplyr::pick(dplyr::where(is.numeric) & -c("x", "y")))),
              valid_ratio = (total_cells - NA_count) / total_cells,
              Valid = valid_ratio >= valid_values_threshold,
              .groups = "drop"
            )

        }


        # Spatial aggregation: collapse all pixels within each plot into
        # summary statistics. This always happens, is independent of
        # summarize_raster, and does not use mean_or_median (all five
        # statistics are always computed).
        if (is_image_collection && nrow(phases_df) > 0) {

          Data <- Data |>
            dplyr::group_by(Region, Plot, Year, Phase, start_date, end_date) |>
            dplyr::summarise(
              dplyr::across(
                .cols = dplyr::where(is.numeric) & -c(x, y),
                .fns = list(
                  mean = ~ mean(., na.rm = TRUE),
                  median = ~ median(., na.rm = TRUE),
                  sd = ~ sd(., na.rm = TRUE),
                  max = ~ max(., na.rm = TRUE),
                  min = ~ min(., na.rm = TRUE)
                ),
                .names = "{.col}_{.fn}"
              ),
              .groups = "drop"
            )


        } else {


          Data <- Data |>
            dplyr::group_by(Region, Plot) |>
            dplyr::summarise(
              dplyr::across(
                .cols = dplyr::where(is.numeric) & -c(x, y),
                .fns = list(
                  mean = ~ mean(., na.rm = TRUE),
                  median = ~ median(., na.rm = TRUE),
                  sd = ~ sd(., na.rm = TRUE),
                  max = ~ max(., na.rm = TRUE),
                  min = ~ min(., na.rm = TRUE)
                ),
                .names = "{.col}_{.fn}"
              ),
              .groups = "drop"
            )

        }

        gc()


        if (length(save_formats) > 0) {

          data_base_dir <- if (is.null(data_output_path)) getwd() else data_output_path

          save_tibble_formats(Data, "Data", data_base_dir, region, save_formats, run_suffix)
          save_tibble_formats(ImageCount, "ImageCount", data_base_dir, region, save_formats, run_suffix)
          save_tibble_formats(Validation, "Validation", data_base_dir, region, save_formats, run_suffix)

        }


        list(
          Raster = raster,
          Data = Data,
          ImageCount = ImageCount,
          Validation = Validation
        )
      }

      )

    return(results)


  }


# Writes a single tibble (Data, ImageCount or Validation) to disk in one or
# more formats. Each format gets its own subfolder (e.g. "Data/QS",
# "Data/CSV") inside base_dir, created on demand. `suffix` is the per-region
# random tag shared with the raster and the other two tibbles saved in the
# same iteration, so all files from one region/run can be matched up.
save_tibble_formats <- function(obj, obj_label, base_dir, region, formats, suffix) {

  ext_map <- c(qs = "qs", parquet = "parquet", csv = "csv", xlsx = "xlsx")

  for (fmt in formats) {

    format_dir <- file.path(base_dir, obj_label, toupper(fmt))

    if (!dir.exists(format_dir)) {

      dir.create(format_dir, recursive = TRUE)

    }

    filename <-
      file.path(
        format_dir,
        paste0(
          tolower(obj_label), "_", region, "_",
          format(Sys.Date(), "%Y_%m_%d"), "_", suffix, ".", ext_map[[fmt]]
        )
      )

    tryCatch(
      {
        switch(
          fmt,
          qs = {
            rlang::check_installed("qs2")
            qs2::qs_save(obj, filename)
          },
          parquet = {
            rlang::check_installed("arrow")
            arrow::write_parquet(obj, filename)
          },
          csv = {
            rlang::check_installed("readr")
            readr::write_csv(obj, filename)
          },
          xlsx = {
            rlang::check_installed("writexl")
            writexl::write_xlsx(obj, filename)
          }
        )

        message(
          "Region '", region, "': ", obj_label, " saved to ", filename
        )
      },
      error = function(e) {
        message(
          "Error saving ", obj_label, " (", fmt, ") for ", region, ": ",
          e$message
        )
      }
    )

  }

}



# Get local raster data (drones, satellite scenes, saved GEE rasters) -----


#' Extract bands and vegetation indices from local raster files over polygon
#' regions
#'
#' Reads one or more raster files (GeoTIFF or anything \code{terra::rast()}
#' accepts) per region from subfolders of \code{base_dir}, and returns
#' per-plot summary statistics (mean, median, sd, max, min) for their bands
#' and, optionally, vegetation indices computed from those bands.
#'
#' @details
#' \strong{Folder layout}
#'
#' \code{base_dir} must contain one subfolder per region, and each
#' subfolder's name must contain the corresponding name from
#' \code{names(shapefiles)} (matched with \code{grepl()}, same convention
#' \code{\link{get_gee_data}} uses for \code{names(shapefiles)}). Every
#' \code{.tif} file directly inside a region's subfolder is treated as one
#' raster "snapshot" for that region -- this is what originally supported
#' drone surveys split across several flights (several files, one per
#' flight/date, in the same region folder), but a folder with a single file
#' (a satellite scene, a Planet composite, or a raster saved by
#' \code{\link{get_gee_data}} via \code{save_raster = TRUE}) works exactly
#' the same way, just with one snapshot instead of several.
#'
#' \strong{Matching \code{bands} to the raster's actual layers}
#'
#' \code{bands} is a named character vector: names are the generic band
#' symbols used in \code{vis_df$Equation} and recognized by
#' \code{\link{extract_required_bands}} (\code{R}, \code{G}, \code{B},
#' \code{NIR}, \code{SWIR}, \code{SWIR1}, \code{SWIR2}, \code{RE},
#' \code{RE1}-\code{RE4}), values are however that band is identified in the
#' source file. \code{band_match} controls how that mapping is applied to
#' each file:
#' \itemize{
#'   \item \code{"name"}: the file's layers already carry real names (a
#'   satellite scene, a Planet product, or a raster saved by
#'   \code{get_gee_data}) -- layers whose current name equals one of
#'   \code{bands}' values are renamed to the matching generic symbol; every
#'   other layer is left untouched.
#'   \item \code{"position"}: the file's layers carry no meaningful names
#'   (a typical drone GeoTIFF) -- the first \code{length(bands)} layers are
#'   assigned \code{names(bands)}, in order. If the file has fewer layers
#'   than \code{bands}, only that many are assigned (with a warning); if it
#'   has more, the extra layers keep whatever name \code{terra} gave them.
#'   \item \code{"auto"} (default): tries \code{"name"} first (checking
#'   whether any of \code{bands}' values already appears among the file's
#'   layer names); if none match, falls back to \code{"position"}. This
#'   preserves the original drone behavior (unnamed layers) while taking
#'   advantage of real names when the file already has them.
#' }
#'
#' \strong{Bands vs. auxiliary layers}
#'
#' Only layers that end up matched to one of \code{bands}' generic symbols
#' are treated as "computable": only those are divided/added via
#' \code{division_scale}/\code{addition_scale}, and only those are checked
#' against \code{vis_df} to decide which indices can be computed. Every
#' other layer in the file (a land-cover classification, an already-computed
#' index, an \code{id}/\code{Plot} layer coming from a raster saved by
#' \code{get_gee_data}, etc.) is carried through untouched and simply
#' appears in the final table as its own column -- there is no need to
#' declare it separately.
#'
#' \strong{Extracting bands without computing indices}
#'
#' \code{vis_df} is optional. If \code{NULL} (the default), no vegetation
#' index is computed -- the function only extracts/renames bands (and
#' passes through auxiliary layers) and summarizes them per plot. This is
#' useful for files that don't carry every band a given index needs, or
#' when only the raw bands are wanted.
#'
#' \strong{Buffer: positive (dilate) or negative (erode)}
#'
#' \code{buffer_dist} accepts any real number: positive values dilate each
#' plot polygon outward (the original behavior, e.g. to make sure a template
#' raster covers slightly beyond the plot), negative values erode it inward
#' (e.g. to move sample pixels away from a plot's edge and avoid mixed
#' pixels from a neighboring plot). \code{0}/\code{NULL} applies no buffer.
#' When \code{resolution_based_buffer = TRUE}, the buffer magnitude is
#' derived from the raster's own pixel size, but its sign still follows
#' \code{buffer_dist} if one was supplied (e.g. \code{buffer_dist = -1} with
#' \code{resolution_based_buffer = TRUE} eats inward by one pixel), and
#' defaults to dilating outward otherwise. Whichever direction is used, any
#' geometry that becomes empty or invalid after buffering is dropped (with a
#' warning naming its \code{id_column} value) rather than silently breaking
#' rasterization/cropping downstream -- this is the "clear rule" negative
#' buffers need: erosion is allowed, but it can never make a plot vanish
#' without telling you.
#'
#' @param base_dir Path to the base directory containing one subfolder per
#'   region.
#' @param shapefiles A named list of \code{sf} objects, one per region;
#'   names must match (via \code{grepl()}) a subfolder of \code{base_dir}.
#' @param vis_df Optional \code{data.frame} with columns \code{Index} and
#'   \code{Equation} describing vegetation indices to compute (see
#'   \code{\link{calc_vis_4}}). If \code{NULL} (default), only bands and any
#'   auxiliary layers are extracted and summarized.
#' @param bands Named character vector mapping generic band symbols (names)
#'   to how that band is identified in the source raster (values) -- see
#'   Details.
#' @param band_match One of \code{"auto"} (default), \code{"position"} or
#'   \code{"name"} -- see Details.
#' @param crs Target coordinate reference system, e.g. \code{"EPSG:32723"}.
#' @param projection_method Resampling method passed to \code{terra::project()}.
#' @param division_scale Optional numeric scalar or vector (length 1 or
#'   \code{length(available bands)}) used to divide the computable bands
#'   (e.g. to convert digital numbers to reflectance).
#' @param addition_scale Optional numeric scalar or vector (same length
#'   rules as \code{division_scale}) added to the computable bands after
#'   \code{division_scale} is applied.
#' @param buffer_dist Optional numeric buffer distance, in the units of
#'   \code{crs}. Positive dilates, negative erodes -- see Details.
#' @param resolution_based_buffer Logical, default \code{FALSE}. If
#'   \code{TRUE}, the buffer magnitude is derived from the raster's pixel
#'   size instead of a fixed \code{buffer_dist} -- see Details.
#' @param interpolate_bands Logical. If \code{TRUE}, small pockets of NA
#'   cells are filled with a 3x3 focal mean.
#' @param filename_pattern Regular expression with two capture groups (date,
#'   phase) used to parse each file's name via
#'   \code{\link{extract_date_and_phase}}. Defaults to the drone convention
#'   \code{"Date_DD_MM_YY_Phase_..."}. Files whose name doesn't match get
#'   \code{Date = NA} / \code{Phase = "All"}, with a warning, instead of
#'   failing -- this is what lets a single satellite scene (with an
#'   arbitrary filename) be processed like any other file.
#' @param export_geotiff Logical, default \code{TRUE}. If \code{TRUE}, the
#'   processed raster for each file (bands renamed/scaled, reprojected,
#'   cropped, masked, with the rasterized plot IDs attached) is written to
#'   \code{base_dir/GeoTIFF/<region>/}.
#' @param id_column Character, name of the plot ID column in each element of
#'   \code{shapefiles}. Created automatically (as sequential integers) if
#'   missing.
#' @param invalid_value Numeric, value treated as invalid data (besides NA)
#'   when checking raster coverage -- passed to
#'   \code{\link{find_non_covered_areas}}.
#' @param threshold Numeric in \code{[0, 1]}, maximum proportion of
#'   invalid/NA cells a plot may have before it is dropped for a given file
#'   -- passed to \code{\link{find_non_covered_areas}}.
#' @param per_layer Logical, passed to \code{\link{find_non_covered_areas}}.
#' @param verbose Logical, whether to print progress/diagnostic messages.
#'
#' @return A named list, one entry per region in \code{shapefiles}, each a
#'   \code{tibble} with per-plot, per-date, per-phase summary statistics for
#'   every numeric band, auxiliary layer, and vegetation index.
#'
#' @examples
#' \dontrun{
#'   vis_df <- data.frame(
#'     Index = c("NDVI", "GNDVI"),
#'     Equation = c("(NIR - R) / (NIR + R)", "(NIR - G) / (NIR + G)")
#'   )
#'
#'   # shapefiles is a named list of sf polygons, one per region/farm, each
#'   # with an "id" column identifying individual plots. base_dir has one
#'   # subfolder per region name, each containing one or more .tif files.
#'   # shapefiles <- list(FarmA = sf_farm_a, FarmB = sf_farm_b)
#'
#'   result <-
#'     get_raster_data(
#'       base_dir = "drone_flights",
#'       shapefiles = shapefiles,
#'       vis_df = vis_df,
#'       bands = c(R = "R", G = "G", B = "B", RE = "RE", NIR = "NIR"),
#'       buffer_dist = -0.5 # erode 0.5 m inward, away from plot edges
#'     )
#'
#'   result$FarmA
#' }
#'
#' @export
get_raster_data <- function(base_dir,
                             shapefiles,
                             vis_df = NULL,
                             bands = c(
                               R = "R",
                               G = "G",
                               B = "B",
                               RE = "RE",
                               NIR = "NIR",
                               Panchromatic = "Panchromatic"
                             ),
                             band_match = c("auto", "position", "name"),
                             crs = "EPSG:32723",
                             projection_method = "bilinear",
                             division_scale = NULL,
                             addition_scale = NULL,
                             buffer_dist = NULL,
                             resolution_based_buffer = FALSE,
                             interpolate_bands = FALSE,
                             filename_pattern = "Date_(\\d{2}_\\d{2}_\\d{2})_([A-Za-z0-9]+)_",
                             export_geotiff = TRUE,
                             id_column = "id",
                             invalid_value = 0,
                             threshold = 0.1,
                             per_layer = TRUE,
                             verbose = TRUE) {


  band_match <- match.arg(band_match)


  log_message <- function(..., level = "info") {
    if (verbose && (level == "info" || level == "warning"))
      message(...)
  }


  # Which generic bands are required by vis_df (if any), and which of the
  # declared `bands` can satisfy them -- mirrors get_gee_data's handling of
  # sat_bands/required_bands, but driven by the user-supplied `bands` param
  # instead of a fixed per-satellite lookup table.
  if (!is.null(vis_df) && nrow(vis_df) > 0) {

    required_bands_for_vis <- extract_required_bands(vis_df$Equation)

    log_message(
      "Required bands to compute VIs: ",
      paste(required_bands_for_vis, collapse = ", "),
      "."
    )

    available_bands <- bands[names(bands) %in% required_bands_for_vis]

    missing_bands <- required_bands_for_vis[!required_bands_for_vis %in% names(bands)]

    if (length(missing_bands) > 0) {

      log_message(
        "Warning: the following bands required by vis_df are not declared in `bands`: ",
        paste(missing_bands, collapse = ", "),
        ".",
        level = "warning"
      )

      lines_new_vis_df <- !grepl(paste(missing_bands, collapse = "|"), vis_df$Equation)

      log_message(
        "Excluding indices requiring missing bands: ",
        paste(vis_df$Index[!lines_new_vis_df], collapse = ", "),
        "."
      )

      vis_df <- vis_df[lines_new_vis_df, ]

    }

  } else {

    # No vis_df: every declared band is "computable" (division_scale /
    # addition_scale eligible), there just aren't any indices to compute.
    available_bands <- bands

  }


  # List all folders in base_dir
  folders <- list.dirs(base_dir, recursive = FALSE, full.names = TRUE) |>
    stringr::str_subset("GeoTIFF$", negate = TRUE)


  folders_basenames <- basename(folders)


  split_folders_basenames <- folders_basenames |>
    stringr::str_split("-", simplify = FALSE) |>
    unlist()


  shapefile_names <- names(shapefiles)


  valid_shapefile_names <- base::intersect(shapefile_names, split_folders_basenames)


  if (length(valid_shapefile_names) < length(shapefile_names)) {


    log_message(
      "A reduced set of shapefiles corresponds to the subfolders within the base directory.\n",
      "Only the shapefiles that match these subfolders are included in the process.\n",
      paste0("Valid shapefiles are: ", paste(valid_shapefile_names, collapse = ", "), ".")
    )


    shapefiles <- shapefiles[valid_shapefile_names]


  }


  shapefiles <- purrr::map(shapefiles, \(shp) {
    if (any(duplicated(sf::st_geometry(shp)))) {

      log_message("Removed ",
                  sum(duplicated(sf::st_geometry(shp))),
                  " duplicate geometries in shapefile for ",
                  paste(names(shp), collapse = ", "), ".")

      shp[!duplicated(sf::st_geometry(shp)), ]

    } else {

      shp

    }
  })



  # Process each region
  results <- shapefiles |>
    purrr::imap(\(shp, region_name) {


      folder_path <- folders[grepl(region_name, basename(folders))]


      if (length(folder_path) == 0) {

        log_message("No folder found for region: ", region_name, ".", level = "warning")

        return(NULL)

      }


      log_message("Processing folder: ", folder_path, ".")


      # Reproject shapefile
      region_sf_reproj <- tryCatch(

        sf::st_transform(shp, crs),

        error = function(e) {

          log_message("Shapefile reprojection failed for ",
                      region_name,
                      ": ",
                      e$message,
                      level = "warning")

          return(NULL)

        }

      )


      if (is.null(region_sf_reproj))  {

        return(NULL)

      }


      # Add ID column if missing
      if (!id_column %in% names(region_sf_reproj)) {

        log_message(
          "No '",
          id_column,
          "' column found in shapefile for ",
          region_name,
          ". Assigning default IDs."
        )

        region_sf_reproj[[id_column]] <- seq_len(nrow(region_sf_reproj))

      }


      # List raster files
      tif_files <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE)


      if (length(tif_files) == 0) {

        log_message("No TIFF files found for region: ", region_name, ".", level = "warning")

        return(NULL)

      }


      # Process each raster file (one flight/scene/snapshot each)
      date_rasters <-
        tif_files |>
        purrr::map(
          \(tif_file) {


            tif <- terra::rast(tif_file)


            non_covered_areas <- find_non_covered_areas(
              shp = region_sf_reproj,
              tif = tif,
              id_column = id_column,
              invalid_value = invalid_value,
              threshold = threshold,
              verbose = verbose,
              per_layer = per_layer
            )


            if (length(non_covered_areas) > 0) {

              log_message(
                "Removing ",
                length(non_covered_areas),
                paste0(
                  " geometries not fully covered by TIFF: ",
                  paste(non_covered_areas, collapse = ", "), "."
                )
              )

              region_sf_reproj <- region_sf_reproj |>
                dplyr::filter(!(!!rlang::sym(id_column)) %in% non_covered_areas)

            }


            # Create a template raster with same resolution as input
            input_res <- terra::res(tif)


            template_rast <-
              terra::rast(
                extent = terra::ext(region_sf_reproj),
                resolution = input_res,
                crs = crs
              )


            log_message(
              "Template raster extent for region ",
              region_name,
              ": ",
              paste(terra::ext(template_rast)[], collapse = ", "),
              "."
            )


            log_message(
              "Template raster dimensions for region ",
              region_name,
              ": ",
              paste(dim(template_rast), collapse = " x "),
              "."
            )


            # Apply buffer -- sign comes from buffer_dist (positive dilates,
            # negative erodes); resolution_based_buffer only controls the
            # magnitude, not the direction. See Details for the full rule.
            effective_buffer <-
              if (resolution_based_buffer) {

                buffer_sign <- if (!is.null(buffer_dist) && buffer_dist < 0) -1 else 1

                if (input_res[1] < 0.1) 0 else buffer_sign * input_res[1]

              } else {

                buffer_dist %||% 0

              }


            region_sf_buffered <-
              if (effective_buffer != 0) {

                buffered <- sf::st_buffer(region_sf_reproj, dist = effective_buffer)

                empty_or_invalid <- sf::st_is_empty(buffered) | !sf::st_is_valid(buffered)

                if (any(empty_or_invalid)) {

                  log_message(
                    sum(empty_or_invalid),
                    " geometries became empty or invalid after a buffer of ",
                    effective_buffer,
                    " units and were dropped for region ",
                    region_name,
                    ": ",
                    paste(buffered[[id_column]][empty_or_invalid], collapse = ", "),
                    ".",
                    level = "warning"
                  )

                  buffered <- buffered[!empty_or_invalid, ]

                }

                buffered

              } else {

                region_sf_reproj

              }


            log_message(if (effective_buffer > 0) {

              paste0("Applied a dilating buffer of ",
                     effective_buffer,
                     " units for region: ",
                     region_name, ".")

            } else if (effective_buffer < 0) {

              paste0("Applied an eroding buffer of ",
                     effective_buffer,
                     " units for region: ",
                     region_name, ".")

            } else {

              paste0("No buffer applied for region: ", region_name, ".")

            })


            # Rasterize plot IDs
            plot_ids_base <- terra::rasterize(
              region_sf_buffered,
              template_rast,
              field = id_column,
              fun = "min",
              background = NA
            )


            names(plot_ids_base) <- "plot_id"


            na_proportion <- mean(is.na(terra::values(plot_ids_base)))


            if (na_proportion > 0.05) {

              plot_ids_base <- terra::focal(
                plot_ids_base,
                w = 3,
                fun = "min",
                na.rm = TRUE,
                na.policy = "only"
              )

              log_message(
                "Applied focal to fill ",
                round(na_proportion * 100, 2),
                "% NA pixels in plot_ids_base."
              )

            } else {

              log_message(
                "Skipped focal operation; NA proportion (",
                round(na_proportion * 100, 2),
                "%) is low."
              )

            }


            # Match `bands` to this file's actual layers (see Details for
            # the "auto"/"position"/"name" rule), renaming only the layers
            # that are identified -- everything else keeps its original
            # name and rides along as an auxiliary/passthrough layer.
            raster_names <- terra::names(tif)

            match_mode <-
              if (band_match == "auto") {
                if (any(bands %in% raster_names)) "name" else "position"
              } else {
                band_match
              }

            if (match_mode == "name") {

              matched_bands <- bands[bands %in% raster_names]

              if (length(matched_bands) > 0) {

                idx <- match(matched_bands, raster_names)
                raster_names[idx] <- names(matched_bands)
                names(tif) <- raster_names

              }

              if (length(matched_bands) < length(bands)) {

                log_message(
                  "Warning: could not match by name the following declared bands in ",
                  basename(tif_file),
                  ": ",
                  paste(names(bands)[!names(bands) %in% names(matched_bands)], collapse = ", "),
                  ".",
                  level = "warning"
                )

              }

            } else {

              num_bands <- terra::nlyr(tif)

              if (num_bands < length(bands)) {

                log_message(
                  "Warning: ",
                  basename(tif_file),
                  " has ",
                  num_bands,
                  " bands; expected ",
                  length(bands),
                  " (",
                  paste(names(bands), collapse = ", "),
                  ").",
                  level = "warning"
                )

                raster_names[seq_len(num_bands)] <- names(bands)[seq_len(num_bands)]

              } else {

                raster_names[seq_len(length(bands))] <- names(bands)

              }

              names(tif) <- raster_names

            }


            # Apply division_scale / addition_scale -- computable bands
            # only (auxiliary layers are left untouched).
            avail_syms <- intersect(names(available_bands), terra::names(tif))

            if (length(avail_syms) > 0) {

              if (!is.null(division_scale) && all(division_scale > 0)) {

                if (length(division_scale) == 1 ||
                    length(division_scale) == length(avail_syms)) {

                  tif[[avail_syms]] <- tif[[avail_syms]] / division_scale

                } else {

                  log_message(
                    "division_scale must have length 1 or ",
                    length(avail_syms),
                    "; skipping it for ",
                    basename(tif_file),
                    ".",
                    level = "warning"
                  )

                }

              }

              if (!is.null(addition_scale)) {

                if (length(addition_scale) == 1 ||
                    length(addition_scale) == length(avail_syms)) {

                  tif[[avail_syms]] <- tif[[avail_syms]] + addition_scale

                } else {

                  log_message(
                    "addition_scale must have length 1 or ",
                    length(avail_syms),
                    "; skipping it for ",
                    basename(tif_file),
                    ".",
                    level = "warning"
                  )

                }

              }

            }


            # Recover date/phase from the filename; fall back instead of
            # failing when it doesn't match filename_pattern (e.g. an
            # arbitrarily-named satellite scene).
            parsed_date_phase <- extract_date_and_phase(basename(tif_file), pattern = filename_pattern)

            if (is.na(parsed_date_phase$date) || is.na(parsed_date_phase$phase)) {

              log_message(
                "Filename '",
                basename(tif_file),
                "' did not match filename_pattern; using Date = NA / Phase = 'All' for this file.",
                level = "warning"
              )

            }

            date_str <- parsed_date_phase$date %||% "NA"
            phase_str <- parsed_date_phase$phase %||% "All"


            # Every layer (bands and auxiliary alike) is tagged with the
            # same date/phase suffix so the pivot below can recover both,
            # regardless of layer name -- "@@@" is used instead of "_" as
            # the separator so layer names and phase labels may freely
            # contain underscores without corrupting the split.
            names(tif) <- paste(terra::names(tif), date_str, phase_str, sep = "@@@")


            tif <- tryCatch(

              terra::project(tif, template_rast, method = projection_method),

              error = function(e) {
                log_message("Reprojection failed for ",
                            basename(tif_file),
                            ": ",
                            e$message,
                            level = "warning")
                tif

              }

            )


            # Crop and mask
            tif <- terra::crop(tif, region_sf_buffered) |>
              terra::mask(region_sf_buffered)


            # Interpolate bands if requested
            if (interpolate_bands) {

              na_proportion <- mean(is.na(terra::values(tif)))

              if (na_proportion > 0.01) {

                tif <- terra::focal(
                  tif,
                  w = 3,
                  fun = "mean",
                  na.rm = TRUE,
                  na.policy = "only"
                )

                log_message(
                  "Interpolated NA values for ",
                  paste(date_str, phase_str, sep = "@@@"),
                  " (NA proportion: ",
                  round(na_proportion, 4),
                  ")."
                )
              }

              tif <- terra::mask(tif, plot_ids_base)

            }


            # Combine with plot IDs
            plot_ids <- plot_ids_base


            names(plot_ids) <- paste("plot_id", date_str, phase_str, sep = "@@@")


            rast_with_ids <- c(tif, plot_ids)


            rast_with_ids


          })


      if (export_geotiff) {

        dir_path <- fs::path_norm(file.path(base_dir, "GeoTIFF", region_name))

        fs::dir_create(dir_path, recurse = TRUE)

        file_paths <- paste0(dir_path, "/", basename(tif_files))

        date_rasters |>
          purrr::walk2(file_paths, \(save_file, save_path) {
            terra::writeRaster(save_file, filename = save_path, overwrite = TRUE)
          }, .progress = verbose)

      }


      # Convert to data frame and compute VIs
      Data_list <- date_rasters |>
        purrr::map(\(d) {

          df <- d |>
            terra::as.data.frame(xy = TRUE) |>
            tibble::as_tibble() |>
            dplyr::filter_at(dplyr::vars(tidyselect::starts_with("plot_id")), ~ !is.na(.))


          if (!interpolate_bands && length(available_bands) > 0) {

            avail_prefix_regex <-
              paste0("^(", paste(stringr::str_escape(names(available_bands)), collapse = "|"), ")@@@")

            avail_cols <- names(df)[stringr::str_detect(names(df), avail_prefix_regex)]

            if (length(avail_cols) > 0) {

              df <- df |>
                dplyr::filter(dplyr::if_all(tidyselect::all_of(avail_cols), ~ !is.na(.)))

            }

          }

          df

        }) |>
        purrr::map(\(d) {

          out <- d |>
            tidyr::pivot_longer(
              cols = !c("x", "y"),
              names_to = c(".value", "Date", "Phase"),
              names_sep = "@@@"
            ) |>
            dplyr::mutate(
              Date = dplyr::na_if(Date, "NA"),
              Date = lubridate::mdy(Date),
              Phase = forcats::as_factor(Phase),
              Plot = forcats::as_factor(plot_id)
            ) |>
            dplyr::select(-plot_id) |>
            tibble::add_column(Region = forcats::as_factor(region_name),
                               .before = "x")

          if (!is.null(vis_df) && nrow(vis_df) > 0) {

            out <- calc_vis_4(out, vis_df)

          }

          out |>
            dplyr::group_by(Region, Phase, Date, Plot) |>
            dplyr::summarise(
              dplyr::across(
                .cols = tidyselect::where(is.numeric) & -c(x, y),
                .fns = list(
                  mean = ~ mean(., na.rm = TRUE),
                  median = ~ median(., na.rm = TRUE),
                  sd = ~ sd(., na.rm = TRUE),
                  max = ~ max(., na.rm = TRUE),
                  min = ~ min(., na.rm = TRUE)
                ),
                .names = "{.col}_{.fn}"
              ),
              .groups = "drop"
            ) |>
            dplyr::mutate_if(is.numeric, ~ replace(.x, is.infinite(.x) |
                                                     is.nan(.x), NA))
        })

      dplyr::bind_rows(Data_list)

    })

  return(results)

}
