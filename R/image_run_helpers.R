#' Extract required generic band names from vegetation index equations
#'
#' Scans one or more vegetation index equations and returns which generic
#' band symbols (\code{R}, \code{G}, \code{B}, \code{NIR}, \code{SWIR},
#' \code{SWIR1}, \code{SWIR2}, \code{RE}, \code{RE1}-\code{RE4}) are used in
#' them. Used to check, before download, whether a given asset actually
#' provides every band a requested index needs.
#'
#' @param equations A character vector of vegetation index equations (as
#'   found in \code{vis_df$Equation}).
#'
#' @return A character vector with the unique generic band names found
#'   across \code{equations}.
#'
#' @export
extract_required_bands <- function(equations) {

  bands <- paste0(
    "\\b",
    c(
      "R",
      "G",
      "B",
      "NIR",
      "SWIR",
      "SWIR1",
      "SWIR2",
      "RE",
      paste0("RE", 1:4)
    ),
    "\\b"
  )

  bands |>
    purrr::map(\(b) {
      stringr::str_extract_all(equations, b) |>
        purrr::compact() |>
        unlist() |>
        unique()
    }) |>
    purrr::compact() |>
    unlist()

}



#' Get the Earth Engine system ID of an asset
#'
#' Retrieves the \code{"system:id"} property of an \code{ee$ImageCollection}
#' (using its first image) or of a single \code{ee$Image}. Used internally
#' to identify which known satellite/product an asset belongs to (see
#' \code{\link{get_bands}} and \code{\link{get_native_scale}}).
#'
#' @param asset An \code{ee$ImageCollection} or a single \code{ee$Image}.
#'
#' @return A character scalar with the asset's system ID, or \code{NULL} if
#'   it could not be retrieved.
#'
#' @export
get_asset_id <- function(asset) {

  tryCatch({

    if (inherits(asset, "ee.imagecollection.ImageCollection")) {

      ee$ImageCollection(asset)$first()$get("system:id")$getInfo()

    } else {

      asset$get("system:id")$getInfo()

    }

  }, error = function(e) NULL)

}



#' Get the named band mapping for a known Earth Engine asset
#'
#' Looks up \code{asset} against a small internal table of known Earth
#' Engine collections (Landsat 8/9 SR, Sentinel-2 SR, TerraClimate,
#' OpenLandMap soil pH/organic carbon, MODIS LST, CHIRPS) and returns a named
#' character vector mapping generic band names (e.g. \code{"R"},
#' \code{"NIR"}, \code{"RE1"}) to that asset's actual band names (e.g.
#' \code{"SR_B4"}, \code{"B8"}).
#'
#' @param asset An \code{ee$ImageCollection} or a single \code{ee$Image}
#'   belonging to one of the known assets.
#'
#' @return A named character vector: names are generic band symbols, values
#'   are the asset's actual band names.
#'
#' @details Raises an error via \code{stop()} if \code{asset}'s system ID
#'   does not match any of the known assets. Callers that want to fall back
#'   to manual band extraction (e.g. \code{asset_ic$bandNames()$getInfo()})
#'   should wrap the call in \code{tryCatch()}.
#'
#' @export
get_bands <- function(asset) {

  lbands <-
    c(
      R = "SR_B4",
      G = "SR_B3",
      B = "SR_B2",
      NIR = "SR_B5",
      SWIR1 = "SR_B6",
      SWIR2 = "SR_B7"
    )

  sbands <-
    c(
      R = "B4",
      G = "B3",
      B = "B2",
      NIR = "B8",
      SWIR1 = "B11",
      SWIR2 = "B12",
      RE1 = "B5",
      RE2 = "B6",
      RE3 = "B7",
      RE4 = "B8A"
    )

  tbands <-
    c(
      "aet",
      "def",
      "pdsi",
      "pet",
      "pr",
      "ro",
      "soil",
      "srad",
      "swe",
      "tmmn",
      "tmmx",
      "vap",
      "vpd",
      "vs"
    )

  tbands <- setNames(tbands, tbands)

  solbands <- c("0", "10", "30", "60", "100", "200")

  pbands <- setNames(paste0("b", solbands), paste0("pH", solbands))

  cbands <- setNames(paste0("b", solbands), paste0("C", solbands))

  mbands <-
    c(
      "LST_Day_1km",
      "QC_Day",
      "Day_view_time",
      "Day_view_angle",
      "LST_Night_1km",
      "QC_Night",
      "Night_view_time",
      "Night_view_angle",
      "Emis_31",
      "Emis_32",
      "Clear_day_cov",
      "Clear_night_cov"
    )

  mbands <- setNames(mbands, mbands)

  chbands <- c(precipitation = "precipitation")

  known_satellite_bands <- list(
    "LANDSAT/LC08/C02/T1_L2" = lbands,
    "LANDSAT/LC09/C02/T1_L2" = lbands,
    "COPERNICUS/S2_SR" = sbands,
    "COPERNICUS/S2_SR_HARMONIZED" = sbands,
    "IDAHO_EPSCOR/TERRACLIMATE" = tbands,
    "OpenLandMap/SOL/SOL_PH-H2O_USDA-4C1A2A_M/v02" = pbands,
    "OpenLandMap/SOL/SOL_ORGANIC-CARBON_USDA-6A1C_M/v02" = cbands,
    "MODIS/061/MOD11A1" = mbands,
    "UCSB-CHG/CHIRPS/DAILY" = chbands
  )

  known_satellites <- stringr::str_c("\\b", names(known_satellite_bands), "\\b")

  asset_id <- get_asset_id(asset = asset)

  detected_satellite <- stringr::str_detect(asset_id, known_satellites)

  if (!is.null(asset_id) && any(detected_satellite)) {

    sat <- names(known_satellite_bands)[detected_satellite]

    detected_bands <- known_satellite_bands[[sat]]

    message(
      "Bands [", paste(detected_bands, collapse = "|"), "] available in ", sat, "."
    )

    return(detected_bands)

  } else {

    stop("Unable to detect which known asset this Image/ImageCollection comes from.")

  }

}



#' Get the native resolution (in meters) of a known Earth Engine asset
#'
#' Determines the pixel size Earth Engine natively stores an asset at. First
#' checks a small internal table of known assets (Landsat 8/9 SR at 30 m,
#' Sentinel-2 SR at 10 m, TerraClimate at 4638 m, OpenLandMap soil pH at
#' 250 m, MODIS MOD09GQ at 250 m); if the asset isn't recognized, falls back
#' to querying each band's projection directly on the Earth Engine server
#' and returns the smallest (finest) nominal scale found across bands.
#'
#' @param asset An \code{ee$ImageCollection} or a single \code{ee$Image}.
#'
#' @return An integer (or numeric) native scale in meters, or \code{NULL} if
#'   it could not be determined (e.g. all per-band projection queries
#'   failed).
#'
#' @export
get_native_scale <- function(asset) {

  known_scales <- list(
    "LANDSAT/LC08/C02/T1_L2" = 30L,
    "LANDSAT/LC09/C02/T1_L2" = 30L,
    "COPERNICUS/S2_SR" = 10L,
    "COPERNICUS/S2_SR_HARMONIZED" = 10L,
    "IDAHO_EPSCOR/TERRACLIMATE" = 4638L,
    "OpenLandMap/SOL/SOL_PH-H2O_USDA-4C1A2A_M/v02" = 250L,
    "MODIS/006/MOD09GQ" = 250L
  )

  known_scales_names <- stringr::str_c("\\b", names(known_scales), "\\b")

  asset_id <- tryCatch({

    if (inherits(asset, "ee.imagecollection.ImageCollection")) {

      ee$ImageCollection(asset)$first()$get("system:id")$getInfo()

    } else {

      asset$get("system:id")$getInfo()

    }
  }, error = function(e) NULL)

  known_scales_detect <- stringr::str_detect(asset_id, known_scales_names)

  if (!is.null(asset_id) && any(known_scales_detect)) {

    detected_name <- names(known_scales)[known_scales_detect]

    message("Native scale found: ", known_scales[[detected_name]], " meters.")

    return(known_scales[[detected_name]])

  }

  first_image <- if (inherits(asset, "ee.imagecollection.ImageCollection")) {

    asset$first()

  } else {

    asset

  }

  scales <-
    first_image$bandNames()$getInfo() |>
    sapply(function(band) {

      tryCatch(

        as.integer(first_image$select(band)$projection()$nominalScale()$getInfo()),

        error = function(e) NA_integer_

      )
    })

  min_scale <- min(scales, na.rm = TRUE)

  if (is.infinite(min_scale)) return(NULL)

  message("Native scale from projection: ", min_scale, " meters.")

  return(min_scale)

}



#' Get the band names available in an Earth Engine asset
#'
#' Convenience wrapper that returns the band names of an
#' \code{ee$ImageCollection} (using its first image) or of a single
#' \code{ee$Image}, without raising an error if the query fails.
#'
#' @param asset_ic An \code{ee$ImageCollection} or a single \code{ee$Image}.
#'
#' @return A character vector of band names, or \code{NULL} if the query
#'   failed.
#'
#' @export
get_available_bands <- function(asset_ic) {

  tryCatch(

    if (inherits(asset_ic, "ee.imagecollection.ImageCollection")) {
      asset_ic$first()$bandNames()$getInfo()

    } else {
      asset_ic$bandNames()$getInfo()

    },
    error = function(e) NULL
  )

}



#' Extract a date and a phase label from a filename
#'
#' Matches \code{filename} against \code{pattern} and returns the first two
#' capture groups as \code{date} and \code{phase}. Used internally by
#' \code{\link{get_raster_data}} to recover the acquisition date and
#' phenological phase encoded in each raster file's name.
#'
#' @param filename A character scalar, typically \code{basename(tif_file)}.
#' @param pattern A regular expression with exactly two capture groups: the
#'   first must match the date portion, the second the phase portion.
#'   Defaults to the convention \code{"Date_DD_MM_YY_Phase_..."}, e.g.
#'   \code{"Date_23_10_24_Vegetative_..."}.
#'
#' @return A list with elements \code{date} and \code{phase}, both character
#'   scalars. If \code{filename} does not match \code{pattern}, both elements
#'   are \code{NA_character_} -- callers decide how to handle that case (see
#'   \code{\link{get_raster_data}}, which falls back to \code{Date = NA} /
#'   \code{Phase = "All"} with a warning instead of failing).
#'
#' @export
extract_date_and_phase <-
  function(filename,
           pattern = "Date_(\\d{2}_\\d{2}_\\d{2})_([A-Za-z0-9]+)_") {

    m <- stringr::str_match(filename, pattern)

    list(
      date = if (is.na(m[1, 1])) NA_character_ else m[1, 2],
      phase = if (is.na(m[1, 1])) NA_character_ else m[1, 3]
    )

  }



# Function to find areas in shapefile not covered by TIFF -----------------


#' Find shapefile geometries not (fully) covered by a raster
#'
#' Identifies which geometries in \code{shp} fall completely outside
#' \code{tif}'s extent, partially outside it, or intersect it but contain too
#' high a proportion of invalid/NA cells. Used internally by
#' \code{\link{get_raster_data}} to drop plots that a given flight/scene does
#' not usably cover before rasterizing plot IDs and extracting bands.
#'
#' @param shp An \code{sf} object with one geometry per plot.
#' @param tif A \code{SpatRaster} (from \code{terra}) covering the same area
#'   (partially or fully) as \code{shp}.
#' @param id_column Character, name of the column in \code{shp} identifying
#'   each geometry. If missing, row indices are used instead (with a
#'   warning).
#' @param invalid_value Numeric, the value treated as invalid data in
#'   \code{tif} (in addition to \code{NA}), e.g. \code{0} for a raster where
#'   background/no-data pixels are coded as zero.
#' @param threshold Numeric in \code{[0, 1]}. Geometries with a proportion of
#'   invalid/NA cells above this threshold are flagged as not covered.
#' @param verbose Logical, whether to print progress/diagnostic messages.
#' @param per_layer Logical. If \code{TRUE}, the invalid-proportion check is
#'   done independently per raster layer (a geometry is flagged if any single
#'   layer exceeds \code{threshold}). If \code{FALSE}, invalid cells are
#'   pooled across all layers before comparing against \code{threshold}.
#'
#' @return An integer/character vector (matching the type of
#'   \code{shp[[id_column]]}) with the IDs of geometries that are either
#'   completely outside \code{tif}'s extent, partially outside it, or
#'   intersect it with too many invalid/NA cells.
#'
#' @export
find_non_covered_areas <-
  function(shp,
           tif,
           id_column = "id",
           invalid_value = 0,
           threshold = 0.1,
           verbose = TRUE,
           per_layer = TRUE) {

    # Step 1: Validate inputs
    if (!inherits(shp, "sf")) {

      stop("The 'shp' argument must be an 'sf' object.")

    }

    if (!inherits(tif, "SpatRaster")) {

      stop("The 'tif' argument must be a 'SpatRaster' object from the 'terra' package.")

    }

    if (nrow(shp) == 0) {

      stop("Shapefile is empty.")

    }

    if (terra::ncell(tif) == 0) {

      stop("TIFF raster is empty.")

    }

    if (!is.numeric(threshold) || threshold < 0 || threshold > 1) {

      stop("Threshold must be a numeric value between 0 and 1.")

    }

    # Step 2: Validate ID column
    shp_names <- names(shp)

    if (is.null(shp_names) || length(shp_names) == 0) {

      if (verbose) {
        warning("Shapefile has no attribute table. Using row indices as IDs.")
      }

      shp[[id_column]] <- seq_len(nrow(shp))

    } else if (!id_column %in% shp_names) {

      stop(
        "ID column '",
        id_column,
        "' not found in shapefile. Available columns: ",
        paste(shp_names, collapse = ", ")
      )

    }

    # Step 3: Ensure CRS match
    tif_crs <- terra::crs(tif)

    if (sf::st_crs(shp) != sf::st_crs(tif_crs)) {

      if (verbose) {

        cat("Reprojecting shapefile to match TIFF CRS...\n")
      }

      shp <- sf::st_transform(shp, tif_crs)

    }

    # Step 4: Get TIFF extent as an sf polygon
    tif_extent <- sf::st_as_sfc(sf::st_bbox(terra::ext(tif), crs = tif_crs))

    # Step 5: Identify geometries outside and partially outside TIFF extent
    intersects_extent <- sf::st_intersects(shp, tif_extent, sparse = FALSE)[, 1]

    outside_extent <- shp[!intersects_extent, ]

    # Identify partially overlapping geometries
    intersects_full <- sf::st_within(shp, tif_extent, sparse = FALSE)[, 1]

    partially_outside <- shp[intersects_extent & !intersects_full, ]


    outside_ids <- if (nrow(outside_extent) > 0) {

      if (verbose) {
        cat(
          nrow(outside_extent),
          "geometries are completely outside the TIFF extent:",
          outside_extent[[id_column]],
          "\n"
        )
      }

      outside_extent[[id_column]]

    } else {

      numeric(0)

    }

    partially_outside_ids <- if (nrow(partially_outside) > 0) {

      if (verbose) {

        cat(
          nrow(partially_outside),
          "geometries are partially outside the TIFF extent:",
          partially_outside[[id_column]],
          "\n"
        )
      }

      partially_outside[[id_column]]

    } else {

      numeric(0)

    }

    inside_extent <- shp[intersects_extent, ] |>  # Start with all intersecting
      dplyr::filter(!(!!rlang::sym(id_column) %in% c(outside_ids, partially_outside_ids)))  # Exclude fully and partially outside
    # Note: Filtering inside_extent to exclude partially_outside_ids assumes these are not checked for invalid data

    # Step 6: Check for invalid data in intersecting geometries
    invalid_ids <- numeric(0)

    if (nrow(inside_extent) > 0 && threshold < 1 && threshold > 0) {

      extracted <- terra::extract(
        tif,
        terra::vect(inside_extent),
        fun = NULL,
        touches = TRUE,
        ID = TRUE
      )

      if (per_layer) {

        invalid_summary <- extracted |>
          dplyr::group_by(ID) |>
          dplyr::summarise(
            n_values = dplyr::n(),
            dplyr::across(
              .cols = -dplyr::any_of(c("ID")),
              .fns = ~ sum(is.na(.x) |
                             .x == invalid_value, na.rm = TRUE),
              .names = "na_or_invalid_{.col}"
            )
          ) |>
          dplyr::mutate(
            dplyr::across(
              tidyselect::starts_with("na_or_invalid_"),
              ~ .x / n_values,
              .names = "prop_invalid_{.col}"
            )
          ) |>
          dplyr::filter(dplyr::if_any(
            tidyselect::starts_with("prop_invalid_"),
            ~ .x > threshold
          ))

      } else {

        invalid_summary <- extracted |>
          dplyr::group_by(ID) |>
          dplyr::summarise(n_values = dplyr::n(),
                           na_or_invalid = sum(rowSums(
                             dplyr::across(
                               .cols = -dplyr::any_of(c("ID")),
                               .fns = ~ is.na(.x) |
                                 .x == invalid_value
                             ),
                             na.rm = TRUE
                           ))) |>
          dplyr::mutate(prop_invalid = na_or_invalid / n_values) |>
          dplyr::filter(prop_invalid > threshold)

      }

      invalid_ids <- if (nrow(invalid_summary) > 0) {

        inside_extent[[id_column]][invalid_summary$ID]

      } else {

        numeric(0)

      }
      if (length(invalid_ids) > 0 && verbose) {

        cat(
          length(invalid_ids),
          "geometries intersect the TIFF but contain invalid data above the threshold:",
          invalid_ids,
          "\n"
        )

      }

    }

    # Step 7: Combine IDs of non-covered areas
    all_non_covered_ids <- sort(unique(c(
      outside_ids, partially_outside_ids, invalid_ids
    )))

    # Step 8: Return results
    if (length(all_non_covered_ids) == 0 && verbose) {

      cat("All shapefile areas are covered by valid TIFF data.\n")

    } else if (verbose) {

      cat(length(all_non_covered_ids),
          "total geometries not fully covered by TIFF.\n")

    }

    return(all_non_covered_ids)

  }



#' Calculate vegetation indices from band columns in a data frame
#'
#' Evaluates each vegetation index equation in \code{vis_df} against the
#' columns of \code{df} (band values), adding one new column per index.
#' Infinite and \code{NaN} results are replaced with \code{NA}. This is the
#' preferred (fourth) implementation of this calculation in the package: it
#' relies on \code{dplyr::mutate()} with quoted expressions built via
#' \code{rlang::parse_expr()}, which is both fast and keeps the original row
#' order and grouping intact.
#'
#' @param df A data frame (or tibble) whose columns include every band
#'   referenced in \code{vis_df$Equation} (e.g. \code{R}, \code{G}, \code{B},
#'   \code{NIR}, \code{SWIR}, \code{RE}, ...).
#' @param vis_df A data frame with (at least) columns \code{Index} (the new
#'   column name for each vegetation index) and \code{Equation} (an R
#'   expression, as a string, referencing band column names in \code{df}).
#'
#' @return \code{df} with one additional column per row of \code{vis_df},
#'   named after \code{vis_df$Index}.
#'
#' @export
calc_vis_4 <- function(df, vis_df) {

  exprs <-
    vis_df$Equation |>
    purrr::map(~ rlang::parse_expr(.x)) |>
    setNames(vis_df$Index)

  suppressWarnings(
    df |>
      dplyr::mutate(!!!exprs) |>
      dplyr::mutate(
        dplyr::across(
          .cols = tidyselect::all_of(vis_df$Index),
          .fns = ~ replace(.x, is.infinite(.x) | is.nan(.x), NA)
        )
      )
  )

}
