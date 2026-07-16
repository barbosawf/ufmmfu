#' Run the full rgee setup: Python environment + Earth Engine authentication
#'
#' Convenience wrapper that runs the entire \pkg{rgee} initialization pipeline
#' in the correct order: sets up (or reuses) the dedicated Python virtual
#' environment, syncs cached Earth Engine credentials, and initializes the
#' Earth Engine session.
#'
#' @param ee_user Character. The Earth Engine account email.
#' @param ee_project Character. The Earth Engine / Cloud project ID.
#' @param venv_name Character. Name of the Python virtual environment used by
#'   rgee. Default `"rgee"`.
#' @param path_env Character. Folder where the virtual environment will be
#'   created/looked up. Should be OUTSIDE any cloud-synced folder (e.g.
#'   OneDrive). Default `"C:/venvs"`.
#' @param py_version Character. Python version to use when creating the
#'   virtual environment. Default `"3.11"`.
#' @param required_packages Character vector of Python packages required by
#'   rgee. Default covers `numpy`, `pandas`, `matplotlib`, `scipy`,
#'   `earthengine-api`, `ipykernel`.
#' @param interactive_restart Logical. If `TRUE` (default) and the Python
#'   virtual environment needs to be created, the function will attempt to
#'   automatically restart the R session via \code{rstudioapi::restartSession()}
#'   (available in RStudio and Positron) and re-run \code{run_rgee_setup()}
#'   with the same arguments right after the restart. If \pkg{rstudioapi} is
#'   not installed, or the IDE doesn't support this, the function falls back
#'   to asking the user to restart manually. Set to `FALSE` to always require
#'   a manual restart.
#' @param pip_cert Character or `NULL`. Path to a CA certificate bundle that
#'   `pip` should trust. See [setup_rgee_python_env()] for details -- this
#'   is a passthrough. Useful on networks with SSL inspection (corporate
#'   proxies/firewalls) that otherwise cause `CERTIFICATE_VERIFY_FAILED`
#'   errors when installing the Python packages required by rgee. Once
#'   passed explicitly, it's remembered in `~/.Renviron` and reused
#'   automatically in later calls.
#' @param pip_trusted_host Character vector, `TRUE`, or `NULL`. Host(s) for
#'   which `pip` should skip SSL verification entirely. See
#'   [setup_rgee_python_env()]. Prefer `pip_cert` when available.
#'
#' @details
#' If the Python virtual environment does not exist yet, this function will
#' create it and install the required packages. Binding reticulate to a
#' virtual environment created in the same session it was created in is
#' unreliable due to a known reticulate/uv conflict, so a session restart is
#' required before Earth Engine authentication can proceed. With
#' `interactive_restart = TRUE`, this restart-and-continue step is automated
#' whenever possible.
#'
#' @return Invisibly, a list with:
#'   \item{venv_python_path}{Path to the virtual environment's Python executable.}
#'   \item{initialized}{Logical. Whether Earth Engine was successfully initialized
#'     in this call.}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' run_rgee_setup(
#'   ee_user    = "tyrosine@gmail.com",
#'   ee_project = "rgee-tyrosine"
#' )
#' }
run_rgee_setup <-
  function(ee_user,
           ee_project,
           venv_name = "rgee",
           path_env = NULL,
           py_version = "3.11",
           required_packages = c("numpy",
                                 "pandas",
                                 "matplotlib",
                                 "scipy",
                                 "earthengine-api",
                                 "ipykernel"),
           interactive_restart = TRUE,
           pip_cert = NULL,
           pip_trusted_host = NULL) {
    # ---------------------------------------------------------------
    # STEP 1: Set up (or reuse) the Python virtual environment.
    # If it was just created, setup_rgee_python_env() returns NULL and
    # a session restart is required before proceeding.
    # ---------------------------------------------------------------
    venv_python_path <- setup_rgee_python_env(
      venv_name = venv_name,
      path_env = path_env,
      py_version = py_version,
      required_packages = required_packages,
      pip_cert = pip_cert,
      pip_trusted_host = pip_trusted_host
    )

    if (is.null(venv_python_path)) {
      if (isTRUE(interactive_restart)) {
        restarted <- .try_auto_restart_and_resume(
          ee_user = ee_user,
          ee_project = ee_project,
          venv_name = venv_name,
          path_env = path_env,
          py_version = py_version,
          required_packages = required_packages,
          pip_cert = pip_cert,
          pip_trusted_host = pip_trusted_host
        )

        if (!restarted) {
          message(
            "\n>>> Could not restart the session automatically. ",
            "Please restart the R session manually, then call run_rgee_setup() ",
            "again to continue with Earth Engine authentication. <<<\n"
          )
        }

      } else {
        message(
          "\n>>> run_rgee_setup() stopped after creating the Python environment. ",
          "Restart the R session and call run_rgee_setup() again to continue ",
          "with Earth Engine authentication. <<<\n"
        )
      }

      return(invisible(list(
        venv_python_path = NULL, initialized = FALSE
      )))
    }

    # ---------------------------------------------------------------
    # STEP 2: Resolve Earth Engine credential paths and sync cached tokens.
    # ---------------------------------------------------------------
    ee_paths  <- get_ee_credential_paths(ee_user)
    has_token <- has_cached_ee_token(ee_paths)

    sync_ee_credentials(ee_paths)

    # ---------------------------------------------------------------
    # STEP 3: Initialize the Earth Engine session.
    # ---------------------------------------------------------------
    initialize_rgee(ee_user = ee_user,
                    ee_project = ee_project,
                    has_token = has_token)

    invisible(list(venv_python_path = venv_python_path, initialized = TRUE))
  }


#' Attempt to automatically restart the R session and resume setup
#'
#' Internal helper. Uses \pkg{rstudioapi} (if available) to restart the R
#' session and automatically re-run \code{run_rgee_setup()} with the same
#' arguments right after the restart.
#'
#' @inheritParams run_rgee_setup
#' @return Logical. `TRUE` if an automatic restart was successfully triggered,
#'   `FALSE` otherwise (caller should fall back to asking for a manual restart).
#' @keywords internal
#' @noRd
.try_auto_restart_and_resume <- function(ee_user,
                                         ee_project,
                                         venv_name,
                                         path_env,
                                         py_version,
                                         required_packages,
                                         pip_cert = NULL,
                                         pip_trusted_host = NULL) {
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    return(FALSE)
  }

  if (!isTRUE(tryCatch(
    rstudioapi::isAvailable(),
    error = function(e)
      FALSE
  ))) {
    return(FALSE)
  }

  has_restart_fn <- tryCatch(
    isTRUE(rstudioapi::hasFun("restartSession")),
    error = function(e)
      FALSE
  )

  if (!has_restart_fn) {
    return(FALSE)
  }

  # Build the command to run automatically right after the session restarts.
  resume_call <- sprintf(
    "ufmmfu::run_rgee_setup(ee_user = %s, ee_project = %s, venv_name = %s, path_env = %s, py_version = %s, required_packages = %s, interactive_restart = TRUE, pip_cert = %s, pip_trusted_host = %s)",
    deparse(ee_user),
    deparse(ee_project),
    deparse(venv_name),
    deparse(path_env),
    deparse(py_version),
    deparse(required_packages),
    paste(deparse(pip_cert), collapse = " "),
    paste(deparse(pip_trusted_host), collapse = " ")
  )

  message("\n>>> Restarting R session automatically to bind the new Python environment... <<<\n")

  tryCatch({
    rstudioapi::restartSession(command = resume_call)
    TRUE
  }, error = function(e) {
    FALSE
  })
}
