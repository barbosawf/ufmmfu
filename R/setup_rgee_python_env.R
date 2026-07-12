#' Set up the Python virtual environment used by rgee
#'
#' Creates (or reuses) a dedicated Python virtual environment for
#' \pkg{rgee}, outside of cloud-synced folders such as OneDrive, and
#' ensures all required Python packages are installed.
#'
#' @param venv_name Character. Name of the virtual environment. Default `"rgee"`.
#' @param path_env Character. Folder where the virtual environment will be
#'   created/looked up. Should be OUTSIDE any cloud-synced folder (e.g. OneDrive).
#'   Default `"C:/venvs"`.
#' @param py_version Character. Python version to use. Default `"3.11"`.
#' @param required_packages Character vector of Python packages to install.
#'
#' @details
#' If the environment does not exist yet, this function creates it and
#' installs the required packages, then stops and asks the user to restart
#' the R session before calling the function again. This avoids a known
#' reticulate/uv conflict where creating a virtualenv and binding to it in
#' the same session causes an initialization error.
#'
#' @return Invisibly, the path to the virtual environment's Python executable.
#' @export
#'
#' @examples
#' \dontrun{
#' venv_rgee_path <- setup_rgee_python_env()
#' }
setup_rgee_python_env <-
  function(venv_name = "rgee",
           path_env = "C:/venvs",
           py_version = "3.11",
           required_packages = c("numpy",
                                 "pandas",
                                 "matplotlib",
                                 "scipy",
                                 "earthengine-api",
                                 "ipykernel")) {

    is_windows <- .Platform$OS.type == "windows"

    if (!dir.exists(path_env)) {
      message("Creating virtualenv root folder at: ", path_env)
      dir.create(path_env, recursive = TRUE, showWarnings = FALSE)
    }

    Sys.setenv(WORKON_HOME = path_env)
    message("Virtualenvs will be created/looked up under: ", path_env)

    expected_python_path <- if (is_windows) {
      file.path(path_env, venv_name, "Scripts", "python.exe")
    } else {
      file.path(path_env, venv_name, "bin", "python")
    }

    if (file.exists(expected_python_path)) {
      Sys.setenv(RETICULATE_PYTHON = expected_python_path)
      message("Pre-pinned RETICULATE_PYTHON to: ", expected_python_path)
    }

    venv_list_obj <- reticulate::virtualenv_list()

    if (!(venv_name %in% venv_list_obj)) {
      message("Environment '",
              venv_name,
              "' not found under '",
              path_env,
              "'. Creating...")

      if (is_windows) {
        available_pythons <- tryCatch(
          reticulate::py_versions_windows(),
          error = function(e)
            NULL
        )
        if (length(available_pythons$version) == 0) {
          message("Installing isolated Python ",
                  py_version,
                  " via pyenv-win...")
          reticulate::install_python(version = py_version, force = FALSE)
        }
      }

      reticulate::virtualenv_create(venv_name, version = py_version)
      reticulate::virtualenv_install(envname = venv_name, packages = required_packages)

      message(
        "\n>>> Environment created and packages installed. ",
        "Please RESTART the R session now, then call setup_rgee_python_env() again ",
        "to bind this environment safely. <<<\n"
      )
      return(invisible(NULL))

    } else {
      message(
        "Environment '",
        venv_name,
        "' already exists under '",
        path_env,
        "'. Checking installed packages..."
      )
    }

    venv_python_path <- reticulate::virtualenv_python(venv_name)
    message("Resolved venv Python path: ", venv_python_path)

    reticulate::use_virtualenv(venv_name, required = TRUE)

    installed <- tryCatch(
      reticulate::py_list_packages(envname = venv_name)$package,
      error = function(e)
        character(0)
    )

    missing_pkgs <- setdiff(required_packages, installed)

    if (length(missing_pkgs) > 0) {
      message("Missing packages detected: ",
              paste(missing_pkgs, collapse = ", "))
      message("Installing missing packages into '", venv_name, "'...")
      reticulate::virtualenv_install(envname = venv_name, packages = missing_pkgs)
    } else {
      message("All required packages are already installed.")
    }

    message("Active Python: ", reticulate::py_config()$python)
    message("Is 'ee' module available? ",
            reticulate::py_module_available("ee"))

    current_env_py <- Sys.getenv("EARTHENGINE_PYTHON")
    if (current_env_py == "" || current_env_py != venv_python_path) {
      rgee::ee_install_set_pyenv(py_path = venv_python_path, py_env = venv_name)
    }

    invisible(venv_python_path)
  }
