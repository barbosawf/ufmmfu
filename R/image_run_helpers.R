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
