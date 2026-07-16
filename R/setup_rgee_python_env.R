#' Set up the Python virtual environment used by rgee
#'
#' Creates (or reuses) a dedicated Python virtual environment for
#' \pkg{rgee}, outside of cloud-synced folders such as OneDrive, and
#' ensures all required Python packages are installed.
#'
#' @param venv_name Character. Name of the virtual environment. Default `"rgee"`.
#' @param path_env Character or `NULL`. Folder where the virtual environment
#'   will be created/looked up. Should be OUTSIDE any cloud-synced folder
#'   (e.g. OneDrive). If `NULL` (default), an OS-appropriate default is used:
#'   `"C:/venvs"` on Windows, or `"~/.venvs"` on Linux/macOS.
#' @param py_version Character. Python version to use. Default `"3.11"`.
#' @param required_packages Character vector of Python packages to install.
#' @param pip_cert Character or `NULL`. Path to a CA certificate bundle
#'   (`.pem`/`.crt`) that `pip` should trust when downloading packages.
#'   Useful on corporate networks that perform SSL inspection (self-signed
#'   certificate injected in the chain), which otherwise causes
#'   `CERTIFICATE_VERIFY_FAILED` errors. If `NULL` (default), the function
#'   looks for a previously saved value in `PIP_CERT` (session env var or
#'   `~/.Renviron`). If provided explicitly, the value is saved to
#'   `~/.Renviron` so future calls don't need to repeat it.
#' @param pip_trusted_host Character vector, `TRUE`, or `NULL`. Host(s) for
#'   which `pip` should skip SSL certificate verification entirely (e.g.
#'   `"pypi.org"`, `"files.pythonhosted.org"`). Use this only when
#'   `pip_cert` is not available/practical -- it disables certificate
#'   verification for those hosts, which is less secure. Pass `TRUE` to use
#'   the default hosts `c("pypi.org", "files.pythonhosted.org")`. Like
#'   `pip_cert`, an explicit value is persisted to `~/.Renviron`. Ignored if
#'   `pip_cert` resolves to a non-`NULL` value.
#'
#' @details
#' If the environment does not exist yet, this function creates it and
#' installs the required packages, then stops and asks the user to restart
#' the R session before calling the function again. This avoids a known
#' reticulate/uv conflict where creating a virtualenv and binding to it in
#' the same session causes an initialization error.
#'
#' On networks that perform SSL/TLS inspection (common on some corporate
#' networks), `pip` may fail with `CERTIFICATE_VERIFY_FAILED` errors when
#' downloading packages from PyPI. Use `pip_cert` (preferred) or
#' `pip_trusted_host` to work around this. Once set explicitly, the choice
#' is remembered in `~/.Renviron` and applied automatically on subsequent
#' calls -- even from `run_rgee_setup()` -- without passing the argument
#' again.
#'
#' @return Invisibly, the path to the virtual environment's Python executable.
#' @export
#'
#' @examples
#' \dontrun{
#' venv_rgee_path <- setup_rgee_python_env()
#'
#' # On a network with SSL inspection (e.g. corporate firewall):
#' venv_rgee_path <- setup_rgee_python_env(
#'   pip_cert = "C:/certs/empresa-ca-bundle.pem"
#' )
#' }
setup_rgee_python_env <-
  function(venv_name = "rgee",
           path_env = NULL,
           py_version = "3.11",
           required_packages = c("numpy",
                                 "pandas",
                                 "matplotlib",
                                 "scipy",
                                 "earthengine-api",
                                 "ipykernel"),
           pip_cert = NULL,
           pip_trusted_host = NULL) {
    is_windows <- .Platform$OS.type == "windows"

    # ---------------------------------------------------------------
    # Resolve (and, if provided explicitly, persist) pip SSL configuration.
    # Must happen before any reticulate call that may shell out to pip
    # (virtualenv_create()'s internal pip/wheel/setuptools bootstrap,
    # virtualenv_install(), etc.).
    # ---------------------------------------------------------------
    .resolve_and_apply_pip_ssl_config(pip_cert = pip_cert, pip_trusted_host = pip_trusted_host)

    # ---------------------------------------------------------------
    # Resolve an OS-appropriate default for path_env if the user didn't
    # provide one explicitly.
    # ---------------------------------------------------------------
    if (is.null(path_env)) {
      path_env <- if (is_windows)
        "C:/venvs"
      else
        "~/.venvs"
    }

    path_env <- path.expand(path_env)

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

      # ---------------------------------------------------------------
      # Explicitly resolve (or install) a Python interpreter to use as the
      # base for the new virtualenv. Works identically on Windows and
      # Linux/macOS via reticulate's uv-backed Python management.
      # ---------------------------------------------------------------
      python_starter <- tryCatch(
        reticulate::virtualenv_starter(version = py_version),
        error = function(e)
          NULL
      )

      if (is.null(python_starter) ||
          length(python_starter) == 0 || anyNA(python_starter)) {
        message(
          "No suitable Python ",
          py_version,
          " found. Installing via reticulate::install_python()..."
        )
        reticulate::install_python(version = py_version)
        python_starter <- tryCatch(
          reticulate::virtualenv_starter(version = py_version),
          error = function(e)
            NULL
        )
      }

      if (is.null(python_starter) ||
          length(python_starter) == 0 || anyNA(python_starter)) {
        stop(
          "Could not locate or install a Python ",
          py_version,
          " interpreter to create the '",
          venv_name,
          "' virtual environment."
        )
      }

      python_starter <- python_starter[1]
      message("Using Python interpreter: ", python_starter)

      reticulate::virtualenv_create(venv_name, python = python_starter)
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

    # ---------------------------------------------------------------
    # STEP 4: Final diagnostics
    # ---------------------------------------------------------------
    message("Active Python: ", reticulate::py_config()$python)
    message("Is 'ee' module available? ",
            reticulate::py_module_available("ee"))

    # ---------------------------------------------------------------
    # Persist EARTHENGINE_PYTHON/EARTHENGINE_ENV to .Renviron ourselves,
    # quietly, WITHOUT calling rgee::ee_install_set_pyenv() -- that function
    # prompts an interactive "restart R session?" question, which is
    # unnecessary here because Sys.setenv() below already makes the
    # variable available in the CURRENT session. .Renviron just ensures
    # future sessions pick it up automatically on startup.
    # (THIS REPLACES the old call to rgee::ee_install_set_pyenv())
    # ---------------------------------------------------------------
    current_env_py <- Sys.getenv("EARTHENGINE_PYTHON")
    if (current_env_py == "" ||
        current_env_py != venv_python_path) {
      .set_earthengine_pyenv_quiet(py_path = venv_python_path, py_env = venv_name)
    }

    invisible(venv_python_path)
  }


#' Persist EARTHENGINE_PYTHON/EARTHENGINE_ENV to .Renviron without prompting
#'
#' Internal helper. Writes (or updates) EARTHENGINE_PYTHON and EARTHENGINE_ENV
#' in the user's .Renviron file, and sets them in the current session via
#' Sys.setenv(), avoiding rgee::ee_install_set_pyenv()'s interactive restart
#' prompt.
#'
#' @param py_path Character. Path to the virtual environment's Python executable.
#' @param py_env Character. Name of the virtual environment.
#' @return Invisibly, `NULL`.
#' @keywords internal
#' @noRd
.set_earthengine_pyenv_quiet <- function(py_path, py_env) {
  renviron_path <- path.expand("~/.Renviron")

  if (!file.exists(renviron_path)) {
    file.create(renviron_path)
  }

  lines <- readLines(renviron_path, warn = FALSE)
  lines <- lines[!grepl("^EARTHENGINE_PYTHON=", lines)]
  lines <- lines[!grepl("^EARTHENGINE_ENV=", lines)]

  lines <- c(
    lines,
    sprintf('EARTHENGINE_PYTHON="%s"', py_path),
    sprintf('EARTHENGINE_ENV="%s"', py_env)
  )

  writeLines(lines, renviron_path)

  Sys.setenv(EARTHENGINE_PYTHON = py_path, EARTHENGINE_ENV = py_env)

  message(
    "Saved EARTHENGINE_PYTHON and EARTHENGINE_ENV to ",
    renviron_path,
    " (no restart needed -- applied to current session too)."
  )

  invisible(NULL)
}


#' Resolve and apply pip SSL configuration (cert bundle or trusted hosts)
#'
#' Internal helper. Handles the common corporate-network scenario where
#' `pip` fails with `CERTIFICATE_VERIFY_FAILED` because a firewall/proxy
#' performs SSL inspection and injects a self-signed certificate into the
#' chain. Resolves `pip_cert`/`pip_trusted_host` (explicit argument > cached
#' session env var > `~/.Renviron`), applies the corresponding environment
#' variables for the current session (so `reticulate::virtualenv_create()`,
#' `virtualenv_install()`, and any subprocess `pip` calls pick them up), and
#' -- only when the caller passed an explicit, non-`NULL` value -- persists
#' it to `~/.Renviron` for future sessions.
#'
#' `pip_cert` takes precedence over `pip_trusted_host` when both resolve to
#' a non-`NULL` value, since trusting a CA bundle is safer than disabling
#' verification entirely.
#'
#' @param pip_cert Character or `NULL`. See [setup_rgee_python_env()].
#' @param pip_trusted_host Character vector, `TRUE`, or `NULL`. See
#'   [setup_rgee_python_env()].
#' @return Invisibly, `NULL`.
#' @keywords internal
#' @noRd
.resolve_and_apply_pip_ssl_config <- function(pip_cert = NULL,
                                              pip_trusted_host = NULL) {
  renviron_path <- path.expand("~/.Renviron")

  # -----------------------------------------------------------------
  # Resolve pip_cert: explicit argument > session env var > .Renviron
  # (the .Renviron file is only consulted if the session env var isn't
  # already set, since sourcing .Renviron at startup already populates it
  # in the common case).
  # -----------------------------------------------------------------
  explicit_cert <- !is.null(pip_cert) && !identical(pip_cert, "")
  if (!explicit_cert) {
    cached_cert <- Sys.getenv("PIP_CERT", unset = "")
    if (nzchar(cached_cert)) {
      pip_cert <- cached_cert
    }
  }

  explicit_host <- !is.null(pip_trusted_host) &&
    !(is.logical(pip_trusted_host) && !isTRUE(pip_trusted_host))
  if (!explicit_host && !explicit_cert) {
    cached_host <- Sys.getenv("PIP_TRUSTED_HOST", unset = "")
    if (nzchar(cached_host)) {
      pip_trusted_host <- strsplit(cached_host, "\\s+")[[1]]
    }
  }

  if (isTRUE(pip_trusted_host)) {
    pip_trusted_host <- c("pypi.org", "files.pythonhosted.org")
  }

  if (is.null(pip_cert) && is.null(pip_trusted_host)) {
    # Nothing configured and nothing cached -- normal network, do nothing.
    return(invisible(NULL))
  }

  # -----------------------------------------------------------------
  # Apply to the current session. pip_cert takes precedence: if set, make
  # sure no trusted-host bypass lingers from a previous .Renviron value.
  # -----------------------------------------------------------------
  if (!is.null(pip_cert)) {
    pip_cert <- path.expand(pip_cert)

    if (!file.exists(pip_cert)) {
      warning("pip_cert points to a file that does not exist: ",
              pip_cert,
              ". Ignoring it.")
      pip_cert <- NULL
    } else {
      Sys.setenv(
        PIP_CERT = pip_cert,
        SSL_CERT_FILE = pip_cert,
        REQUESTS_CA_BUNDLE = pip_cert
      )
      Sys.unsetenv("PIP_TRUSTED_HOST")
      message("Using pip CA certificate bundle: ", pip_cert)
    }
  }

  if (is.null(pip_cert) && !is.null(pip_trusted_host)) {
    trusted_host_str <- paste(pip_trusted_host, collapse = " ")
    Sys.setenv(PIP_TRUSTED_HOST = trusted_host_str)
    message(
      "Skipping SSL certificate verification for pip host(s): ",
      trusted_host_str,
      " (less secure -- prefer pip_cert when possible)."
    )
  }

  # -----------------------------------------------------------------
  # Persist to .Renviron, but only for values the caller explicitly passed
  # in THIS call -- we don't want to re-write values that were merely read
  # back from .Renviron/env vars on every call.
  # -----------------------------------------------------------------
  if (explicit_cert || explicit_host) {
    if (!file.exists(renviron_path)) {
      file.create(renviron_path)
    }

    lines <- readLines(renviron_path, warn = FALSE)
    lines <- lines[!grepl("^PIP_CERT=", lines)]
    lines <- lines[!grepl("^PIP_TRUSTED_HOST=", lines)]
    lines <- lines[!grepl("^SSL_CERT_FILE=", lines)]
    lines <- lines[!grepl("^REQUESTS_CA_BUNDLE=", lines)]

    if (!is.null(pip_cert)) {
      lines <- c(
        lines,
        sprintf('PIP_CERT="%s"', pip_cert),
        sprintf('SSL_CERT_FILE="%s"', pip_cert),
        sprintf('REQUESTS_CA_BUNDLE="%s"', pip_cert)
      )
    } else if (!is.null(pip_trusted_host)) {
      lines <- c(lines, sprintf(
        'PIP_TRUSTED_HOST="%s"',
        paste(pip_trusted_host, collapse = " ")
      ))
    }

    writeLines(lines, renviron_path)
    message(
      "Saved pip SSL configuration to ",
      renviron_path,
      " (will be applied automatically in future sessions)."
    )
  }

  invisible(NULL)
}
