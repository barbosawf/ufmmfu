#' Resolve Earth Engine credential file paths
#'
#' @param ee_user Character. The Earth Engine account email.
#' @param is_windows Logical. Whether the current OS is Windows.
#'
#' @return A named list with resolved credential paths for the current OS.
#' @export
get_ee_credential_paths <-
  function(ee_user, is_windows = .Platform$OS.type == "windows") {
    if (is_windows) {
      home_dir <- chartr("\\", "/", Sys.getenv("USERPROFILE"))
      adc_path <- paste0(home_dir,
                         "/AppData/Roaming/gcloud/legacy_credentials/",
                         ee_user,
                         "/adc.json")
    } else {
      home_dir <- Sys.getenv("HOME")
      adc_path <- NULL
    }

    user_ee_dir   <- paste0(home_dir, "/.config/earthengine/", ee_user)
    global_ee_dir <- paste0(home_dir, "/.config/earthengine")

    list(
      is_windows = is_windows,
      user_ee_dir = user_ee_dir,
      global_ee_dir = global_ee_dir,
      user_credentials_file = paste0(user_ee_dir, "/credentials"),
      global_credentials_file = paste0(global_ee_dir, "/credentials"),
      adc_path = adc_path
    )
  }


#' Check whether a cached Earth Engine token already exists
#'
#' @param paths A list returned by \code{\link{get_ee_credential_paths}}.
#'
#' @return Logical. `TRUE` if a cached token file was found.
#' @export
has_cached_ee_token <-
  function(paths) {
    candidate_paths <- c(paths$user_credentials_file, paths$adc_path)
    any(file.exists(candidate_paths))
  }


#' Sync Earth Engine credentials to the global cache location
#'
#' Copies the user-level credentials file over the global cache file,
#' before any Python-side Earth Engine module is loaded.
#'
#' @param paths A list returned by \code{\link{get_ee_credential_paths}}.
#'
#' @return Invisibly, `TRUE` if a copy was performed, `FALSE` otherwise.
#' @export
sync_ee_credentials <- function(paths) {
  if (!file.exists(paths$user_credentials_file)) {
    return(invisible(FALSE))
  }

  if (paths$is_windows) {
    shell(sprintf(
      'copy "%s" "%s" /Y',
      chartr("/", "\\", paths$user_credentials_file),
      chartr("/", "\\", paths$global_credentials_file)
    ), intern = TRUE)

  } else {
    file.copy(
      from = paths$user_credentials_file,
      to = paths$global_credentials_file,
      overwrite = TRUE
    )
  }

  invisible(TRUE)
}


#' Reset the Python-side 'ee' module in memory
#'
#' Clears the `ee` module from Python's memory to remove any footprint
#' left by a previously authenticated account in the same session.
#'
#' @return Invisibly, `NULL`.
#' @export
reset_ee_python_module <- function() {
  if (reticulate::py_module_available("ee")) {
    message("Resetting internal Python active memory to clear previous account footprints...")
    reticulate::py_run_string("import sys; sys.modules.pop('ee', None)")
  }
  invisible(NULL)
}
