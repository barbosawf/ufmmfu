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
#' @param target_scale_m Numeric, desired output pixel size in meters for the
#'   final local product. Ignored when \code{use_native_scale = TRUE}.
#' @param use_native_scale Logical, default \code{TRUE}. If \code{TRUE}, the
#'   asset's native resolution (auto-detected via \code{get_native_scale()})
#'   is used as the final local output scale instead of \code{target_scale_m}.
#'   Download from Earth Engine always happens at the native resolution
#'   regardless of this argument; this only affects local resampling.
#' @param valid_values_threshold Numeric in \code{[0, 1]}. Minimum proportion of
#'   non-NA cells required per plot/phase for that combination to be flagged
#'   \code{Valid = TRUE} in the returned \code{Validation} table.
#' @param download_route Passed to \code{rgee::ee_as_rast()}, e.g.
#'   \code{"drive"} or \code{"gcs"}.
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
#' @param max_pixels Passed to \code{rgee::ee_as_rast()} as \code{maxPixels}.
#' @param clean_drive Logical, default \code{TRUE}. If \code{TRUE} and
#'   \code{download_route == "drive"}, calls
#'   \code{rgee::ee_clean_container()} after each region's download.
#' @param min_output_cells Integer, default 5. Minimum number of local output
#'   cells required across the smallest dimension of each region, used to
#'   automatically refine \code{target_scale_m}/native scale locally when the
#'   region is too small for the requested scale.
#' @param mask Optional \code{ee$Image} used to mask out pixels before
#'   download. Build it however you like outside the function -- e.g. chain
#'   \code{$neq()}/\code{$And()} conditions on a land-cover collection such
#'   as MapBiomas to exclude classes you are not interested in (water,
#'   urban, forest, etc.), or derive it from any other Earth Engine
#'   collection you have access to. It is clipped to each region's download
#'   extent internally and applied via \code{updateMask()}, so a single
#'   mask is automatically adapted to every region in \code{shapefiles}. If
#'   \code{NULL} (default), no mask is applied.
#' @param save_raster Logical, default \code{FALSE}. If \code{TRUE}, the
#'   final processed raster for each region -- the same object returned in
#'   \code{$Raster}, i.e. the version right before it is turned into a
#'   data.frame -- is written to disk as a GeoTIFF.
#' @param raster_output_path Optional character path to a folder where
#'   rasters are saved when \code{save_raster = TRUE}. If \code{NULL}
#'   (default), a \code{"raster_outputs"} folder is created inside the
#'   current working directory. Each file is named with the region and a
#'   timestamp so repeated runs, or multiple regions in the same run, never
#'   overwrite each other.
#' @param batch_size Optional integer. Only relevant when
#'   \code{summarize_raster = FALSE} (individual images are downloaded
#'   rather than a single temporally-reduced image per phase). When a
#'   phase's image count exceeds \code{batch_size}, images are grouped into
#'   smaller Earth Engine download requests instead of one large combined
#'   request, and the results are stitched back together locally. See
#'   Details ("Memory usage with many images").
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
#'     get_temporal_vi_data(
#'       shapefiles       = shapefiles,
#'       asset_bands_ic   = s2_ic,
#'       vis_df           = vis_df,
#'       phases_df        = phases_df,
#'       summarize_raster = FALSE,   # per-image indices -> needed for batch_size
#'       mask             = exclusion_mask,
#'       save_raster      = TRUE,
#'       raster_output_path = "rasters/s2_2023_2024",
#'       batch_size       = 5        # download 5 images per EE request at a time
#'     )
#'
#'   # Per-plot/per-phase summary statistics
#'   result$FarmA$Data
#'
#'   # Rasters were also written to rasters/s2_2023_2024/, one GeoTIFF per
#'   # region, named e.g. "raster_FarmA_20260718_223045_0417.tif"
#'   list.files("rasters/s2_2023_2024")
#' }
#'
#' @export
get_temporal_vi_data <-
  function(
    shapefiles,
    asset_bands_ic,
    vis_df = NULL,
    phases_df = NULL,
    mean_or_median = "mean",
    summarize_raster = TRUE,
    target_scale_m = 5L,
    use_native_scale = TRUE,
    valid_values_threshold = 0.1,
    download_route = "drive",
    region_buffer_m = NULL,
    buffer_from_native_scale = FALSE,
    crs = "EPSG:32723",
    projection_method = "bilinear",
    division_scale = NULL,
    addition_scale = NULL,
    interpolate_bands = FALSE,
    max_pixels = 1e12,
    clean_drive = TRUE,
    min_output_cells = 5,
    mask = NULL,
    save_raster = FALSE,
    raster_output_path = NULL,
    batch_size = NULL
  ) {


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

              if (clean_drive && download_route == "drive") {

                rgee::ee_clean_container(name = "rgee_backup", type = "drive")

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
                crs = crs,
                maxPixels = max_pixels
              ),
              error = function(e) {
                message("Error downloading raster for ", region, ": ", e$message)
                NULL
              }
            )


          if (clean_drive && download_route == "drive") {

            rgee::ee_clean_container(name = "rgee_backup", type = "drive")

          } else if (download_route == "drive") {

            message(
              "clean_drive = FALSE: keeping files on Google Drive for ", region, "."
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

          # Region name + timestamp + a random suffix guarantee a unique
          # filename across regions and across repeated runs of the
          # function, so earlier saved rasters are never overwritten.
          raster_filename <-
            file.path(
              output_dir,
              paste0(
                "raster_", region, "_",
                format(Sys.time(), "%Y%m%d_%H%M%S"), "_",
                sprintf("%04d", sample.int(9999, 1)),
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

            Data <- Data |>
              tidyr::pivot_longer(
                cols = !dplyr::any_of(c("x", "y", "Plot")),
                names_to = c("Phase", "ImageIndex", ".value"),
                names_pattern = bands_regex
              ) |>
              dplyr::mutate(
                Phase = forcats::as_factor(Phase),
                Plot = forcats::as_factor(Plot),
                ImageIndex = as.integer(ImageIndex)
              ) |>
              tibble::add_column(
                Region = forcats::as_factor(region), .before = "x"
              ) |>
              dplyr::right_join(phase_tb) |>
              dplyr::relocate("start_date", "end_date", .after = "Phase") |>
              dplyr::relocate("Year", .before = "Phase")

          } else {

            # (phase)_(band), e.g. "Flowering_B4" -- this image is already a
            # per-phase temporal mean/median, computed on the GEE side.
            bands_regex <- paste0("^(", phases_escaped, ")[_\\.]+(", bands_escaped, ")$")

            Data <- Data |>
              tidyr::pivot_longer(
                cols = !dplyr::any_of(c("x", "y", "Plot")),
                names_to = c("Phase", ".value"),
                names_pattern = bands_regex
              ) |>
              dplyr::mutate(
                Phase = forcats::as_factor(Phase),
                Plot = forcats::as_factor(Plot)
              ) |>
              tibble::add_column(
                Region = forcats::as_factor(region), .before = "x"
              ) |>
              dplyr::right_join(phase_tb) |>
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
