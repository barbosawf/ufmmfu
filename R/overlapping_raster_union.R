# Unify overlapping rasters ------------------------------------------------


#' Merge overlapping raster files that jointly cover split plots
#'
#' When more than one raster file is needed to cover a region (several drone
#' flights, adjacent satellite tiles, ...), a plot that sits near a file's
#' edge can end up split: incomplete in every individual file, even though
#' the files together cover it fully. This function detects which input
#' rasters overlap in extent, mosaics each overlapping group into a single
#' raster, and writes one merged \code{.tif} per group -- so plots that were
#' partial in isolation become whole once their covering files are merged.
#' Rasters that don't overlap any other input are written back out
#' unchanged (still going through the same mosaic/writing code path, so the
#' output is consistent either way).
#'
#' Run this \emph{before} \code{\link{get_raster_data}} whenever a region's
#' plots may be split across more than one file: point it at that region's
#' raw files, then point \code{get_raster_data()} at the folder containing
#' its output (the \code{.tif} extension matches what
#' \code{get_raster_data()} looks for).
#'
#' @details
#' \strong{How groups are detected}
#'
#' Overlap is checked purely on raster extent (via \code{terra::intersect()}
#' on each pair of \code{SpatExtent}s), using a union-find so that A-overlaps-B
#' and B-overlaps-C group A, B and C together even if A and C don't overlap
#' directly. This assumes \code{terra::intersect()} returns \code{NULL} for
#' a non-overlapping pair (true in the terra versions this was developed
#' against) -- if a newer/older terra version changes that, re-check this
#' function's grouping before trusting its output.
#'
#' \strong{Resolution and alignment inside a group}
#'
#' Every raster in an overlapping group is resampled (via
#' \code{terra::resample()}, method \code{projection_method}) onto a common
#' grid before mosaicking: the finest resolution among the group's rasters,
#' over the union of their extents. This guarantees the rasters are
#' perfectly aligned (not just same resolution) before
#' \code{terra::mosaic()} combines them cell by cell with \code{mosaic_method}.
#'
#' @param raster_paths Character vector of paths to the raster files to
#'   check for overlaps and merge.
#' @param band_names Optional character vector of band names to assign to
#'   each group's merged raster (only applied when its length matches the
#'   merged raster's layer count). \code{NULL} (default) leaves whatever
#'   names the source rasters/mosaic already carry.
#' @param output_name Character, base name used to build each group's output
#'   filename: \code{"<output_name>_<group index>.tif"}.
#' @param output_dir Optional directory to write merged rasters into. If
#'   \code{NULL} (default), uses the directory of \code{raster_paths[1]} --
#'   i.e. writes alongside the inputs, as the original version of this
#'   function did.
#' @param crs_setting Target coordinate reference system, e.g.
#'   \code{"EPSG:32723"}. Every input raster is reprojected to it before
#'   overlap detection.
#' @param projection_method Resampling method passed to
#'   \code{terra::resample()} when aligning rasters within a group.
#' @param mosaic_method Passed to \code{terra::mosaic()} as \code{fun} --
#'   how overlapping cells are combined, e.g. \code{"mean"}, \code{"first"},
#'   \code{"max"}.
#' @param on_disc Logical, default \code{FALSE}. If \code{TRUE}, intermediate
#'   resampled rasters and each group's mosaic are written to temporary
#'   files instead of held in memory -- use for rasters too large to
#'   process in RAM.
#' @param remove_input_files Logical, default \code{FALSE}. If \code{TRUE},
#'   deletes every file in \code{raster_paths} \strong{after} all groups
#'   have been written -- this is irreversible, only enable it once you've
#'   confirmed the merged outputs are correct.
#' @param verbose Logical, whether to print progress/diagnostic messages.
#'
#' @return A character vector with the path of each file written (one per
#'   overlap group), invisibly.
#'
#' @seealso \code{\link{get_raster_data}}, the usual next step once
#'   overlapping files have been merged.
#'
#' @examples
#' \dontrun{
#'   # Two drone flights over the same farm, each missing part of a few
#'   # plots near the flight boundary -- merge them into one file per
#'   # overlap group before running get_raster_data() on the result.
#'   overlapping_raster_union(
#'     raster_paths = list.files("drone_flights/FarmA", pattern = "\\.tif$",
#'                               full.names = TRUE),
#'     band_names = c("R", "G", "B", "RE", "NIR"),
#'     output_name = "FarmA_merged"
#'   )
#' }
#'
#' @export
overlapping_raster_union <- function(raster_paths,
                                     band_names = NULL,
                                     output_name = "raster",
                                     output_dir = NULL,
                                     crs_setting = "EPSG:32723",
                                     projection_method = "bilinear",
                                     mosaic_method = "mean",
                                     on_disc = FALSE,
                                     remove_input_files = FALSE,
                                     verbose = FALSE) {


  n_rasters <- length(raster_paths)

  if (n_rasters == 0) stop("No raster provided.")


  output_dir <- output_dir %||% dirname(raster_paths[1])

  fs::dir_create(output_dir, recurse = TRUE)


  log_message <- function(..., level = "info") {
    if (verbose && (level %in% c("info", "warning"))) message(...)
  }


  # Load every raster and reproject it to the target CRS up front, so
  # extent comparisons below are all in the same units.
  rasters <- lapply(raster_paths, terra::rast)

  rasters <- lapply(rasters, function(r) {
    if (paste0("EPSG:", sf::st_crs(r)$epsg) != crs_setting) {
      terra::project(r, crs_setting)
    } else {
      r
    }
  })

  names(rasters) <- basename(raster_paths)


  # Detect which rasters overlap in extent, using a union-find so
  # transitively-connected rasters (A-B and B-C overlap, even if A-C don't)
  # end up in the same group.
  extents <- lapply(rasters, terra::ext)

  parent <- seq_len(n_rasters)

  find_root <- function(x) {
    if (parent[x] != x) parent[x] <<- find_root(parent[x])
    parent[x]
  }

  union_groups <- function(x, y) {
    root_x <- find_root(x)
    root_y <- find_root(y)
    if (root_x != root_y) parent[root_x] <<- root_y
  }

  if (n_rasters > 1) {

    for (i in seq_len(n_rasters - 1)) {
      for (j in (i + 1):n_rasters) {
        # NOTE: assumes terra::intersect() returns NULL for a
        # non-overlapping extent pair -- see Details.
        if (!is.null(terra::intersect(extents[[i]], extents[[j]]))) {
          union_groups(i, j)
        }
      }
    }

  }

  groups <- split(seq_len(n_rasters), sapply(seq_len(n_rasters), find_root))

  log_message(length(groups), " group(s) of overlapping rasters detected.")

  if (verbose) {
    for (g in seq_along(groups)) {
      log_message("Group ", g, ": ", paste(names(rasters)[groups[[g]]], collapse = ", "))
    }
  }


  # Zero-pad group indices to the width of the largest one, so filenames
  # sort the same alphabetically and numerically (e.g. "01".."12", not
  # "1", "10", "11", "12", "2", ...).
  index_width <- nchar(as.character(length(groups)))


  written_paths <- character(0)

  for (grp_idx in seq_along(groups)) {

    grp <- groups[[grp_idx]]

    if (length(grp) > 1) {

      log_message(
        "Merging ", length(grp), " raster(s) from group ", grp_idx,
        " (method: ", mosaic_method, ")."
      )

      finest_res <- min(sapply(rasters[grp], function(r) terra::res(r)[1]))

      log_message("Finest resolution in group: ", round(finest_res, 5), " -- resampling to it.")

      group_exts <- lapply(rasters[grp], terra::ext)

      union_ext <- Reduce(terra::union, group_exts)

      alignment_template <-
        terra::rast(ext = union_ext, resolution = finest_res, crs = crs_setting)

      resampled <- lapply(rasters[grp], function(r) {
        if (on_disc) {
          terra::resample(
            r, alignment_template,
            method = projection_method,
            filename = tempfile(fileext = ".tif"),
            overwrite = TRUE
          )
        } else {
          terra::resample(r, alignment_template, method = projection_method)
        }
      })

      collection <- terra::sprc(resampled)

      merged <-
        if (on_disc) {
          terra::mosaic(
            collection,
            fun = mosaic_method,
            filename = tempfile(fileext = ".tif"),
            overwrite = TRUE
          )
        } else {
          terra::mosaic(collection, fun = mosaic_method)
        }

      gc()

    } else {

      merged <- rasters[[grp]]

    }

    if (!is.null(band_names) && length(band_names) == terra::nlyr(merged)) {
      names(merged) <- band_names
    }

    group_tag <- formatC(grp_idx, width = index_width, flag = "0")

    # ".tif", not ".tiff", so get_raster_data()'s `list.files(..., pattern
    # = "\\.tif$")` picks these up automatically.
    out_path <- fs::path(output_dir, paste0(output_name, "_", group_tag, ".tif"))

    terra::writeRaster(merged, out_path, overwrite = TRUE)

    written_paths <- c(written_paths, out_path)

    gc()

  }


  if (remove_input_files) {

    log_message(
      "remove_input_files = TRUE: deleting ", n_rasters, " original input file(s).",
      level = "warning"
    )

    file.remove(raster_paths)

  }


  invisible(written_paths)

}
