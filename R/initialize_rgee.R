#' Initialize an Earth Engine session via rgee
#'
#' Handles both the "cached token" and "no cached token" cases, with
#' automatic retry via forced re-authentication if the cached-token path fails.
#'
#' @param ee_user Character. The Earth Engine account email.
#' @param ee_project Character. The Earth Engine / Cloud project ID.
#' @param has_token Logical. Whether a cached token was found
#'   (see \code{\link{has_cached_ee_token}}).
#'
#' @return The result of \code{rgee::ee_Initialize()}, invisibly.
#' @export
initialize_rgee <-
  function(ee_user, ee_project, has_token) {

    container_path <- paste0("projects/", ee_project, "/assets")

    run_ee_initialize <- function() {
      rgee::ee_Initialize(
        user = ee_user,
        project = ee_project,
        drive = TRUE,
        container = container_path
      )
    }

    reset_ee_python_module()

    if (has_token) {
      message("Cached token footprint found for user: ", ee_user)
      message("Initializing Earth Engine session...")

      tryCatch({
        run_ee_initialize()

      }, error = function(e) {
        message("Initialization stalled. Forcing native Python credential synchronization...")
        rgee::ee_clean_user_credentials()
        rgee::ee_Authenticate(user = ee_user, drive = TRUE)
        run_ee_initialize()

      })

    } else {
      message("No local cached token found for account: ", ee_user)
      message("Launching OAuth2 browser authentication interface...")

      rgee::ee_clean_user_credentials()
      rgee::ee_Authenticate(user = ee_user, drive = TRUE)
      suppressWarnings(run_ee_initialize())

    }

    invisible(NULL)
  }
