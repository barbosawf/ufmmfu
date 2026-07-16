#' Resolve the True Physical Home Directory for SSH Purposes
#'
#' On Windows, R's `~` (via `normalizePath("~")`) may resolve to a
#' OneDrive-redirected "Documents" folder instead of the physical user
#' profile directory (`C:/Users/<user>`), which is what Git Bash / OpenSSH
#' actually use as `$HOME`. This mismatch causes SSH to look for keys and
#' the `config` file in the wrong place, producing errors such as
#' "no such identity" even though the keys exist on disk. This function
#' always returns the physical, non-redirected home directory.
#'
#' @return Character. A normalized path (forward slashes) to the user's
#'   real, physical home directory.
#' @export
get_ssh_home <- function() {
  if (.Platform$OS.type == "windows") {
    home <- Sys.getenv("USERPROFILE", unset = NA)
    if (is.na(home) || !nzchar(home) || !dir.exists(home)) {
      drive <- Sys.getenv("HOMEDRIVE", unset = "C:")
      path  <- Sys.getenv("HOMEPATH", unset = "")
      home  <- paste0(drive, path)
    }
    home <- gsub("\\\\", "/", home)
  } else {
    home <- normalizePath("~", mustWork = FALSE)
  }
  home
}

#' Diagnose SSH Home Resolution and Configuration Status
#'
#' Compares R's default `~` resolution against the physical home directory
#' returned by \code{\link{get_ssh_home}}, warning if they diverge (a sign
#' of OneDrive folder redirection on Windows). Also reports whether an SSH
#' \code{config} file and known key files are present in the physical
#' \code{.ssh} directory.
#'
#' @param key_names Character vector. Base names (without extension) of
#'   private keys to look for inside the physical \code{.ssh} directory.
#'   Defaults to \code{c("id_rsa_barbosawf", "id_rsa_epamigsafra")}.
#' @return Invisibly returns a list with \code{r_home}, \code{physical_home},
#'   \code{ssh_dir}, \code{config_exists}, and \code{keys_found}.
#' @export
check_ssh_setup <- function(key_names = c("id_rsa_barbosawf", "id_rsa_epamigsafra")) {
  r_home        <- tryCatch(
    normalizePath("~", mustWork = FALSE),
    error = function(e)
      NA
  )
  physical_home <- get_ssh_home()
  ssh_dir       <- file.path(physical_home, ".ssh")
  config_file   <- file.path(ssh_dir, "config")

  message(sprintf("- R's '~' resolves to:      %s", r_home))
  message(sprintf("- Physical SSH home is:     %s", physical_home))

  if (!identical(gsub("\\\\", "/", r_home), gsub("\\\\", "/", physical_home))) {
    warning(
      "R's '~' and the physical home directory differ. This usually means ",
      "OneDrive (or another sync tool) has redirected your Documents/user ",
      "profile folder. Use get_ssh_home() instead of '~' for anything ",
      "SSH-related."
    )
  } else {
    message("[OK] No divergence detected between R's '~' and the physical home.")
  }

  config_exists <- file.exists(config_file)
  message(sprintf(
    "- SSH config file %s at: %s",
    if (config_exists)
      "found"
    else
      "NOT found",
    config_file
  ))

  keys_found <- stats::setNames(logical(length(key_names)), key_names)
  for (k in key_names) {
    key_path <- file.path(ssh_dir, k)
    exists   <- file.exists(key_path)
    keys_found[[k]] <- exists
    message(sprintf("- Key '%s' %s at: %s", k, if (exists)
      "found"
      else
        "NOT found", key_path))
  }

  invisible(
    list(
      r_home = r_home,
      physical_home = physical_home,
      ssh_dir = ssh_dir,
      config_exists = config_exists,
      keys_found = keys_found
    )
  )
}

#' Generate Multi-Account SSH Config in the Physical Home Directory
#'
#' Writes an SSH \code{config} file with host-alias routing for multiple
#' GitHub accounts, always targeting the physical, non-redirected home
#' directory returned by \code{\link{get_ssh_home}} (avoiding the OneDrive
#' path-mismatch issue). Also binds \code{core.sshCommand} globally in Git
#' so native Git calls (outside R) find the same config file without
#' needing \code{-F} on every command.
#'
#' Existing private keys are not generated or moved by this function; it
#' only writes routing configuration. Use your platform's \code{ssh-keygen}
#' (or an equivalent helper) to create keys directly inside the directory
#' printed by this function if they don't already exist there.
#'
#' @param accounts A named character vector mapping account alias to email,
#'   e.g. \code{c(barbosawf = "wagner.barbosa@ufv.br", epamigsafra = "epamigsafra01@outlook.com")}.
#' @param bind_global Logical. If \code{TRUE} (default), also sets
#'   \code{git config --global core.sshCommand} to point at this config file.
#' @return Invisibly returns the path to the written config file.
#' @export
git_setup_ssh_config <- function(accounts, bind_global = TRUE) {
  if (is.null(names(accounts)) || any(!nzchar(names(accounts)))) {
    stop("'accounts' must be a named character vector: alias = email.")
  }

  ssh_dir     <- file.path(get_ssh_home(), ".ssh")
  config_file <- file.path(ssh_dir, "config")

  if (!dir.exists(ssh_dir)) {
    dir.create(ssh_dir, recursive = TRUE, showWarnings = FALSE)
  }

  known_hosts_file <- file.path(ssh_dir, "known_hosts")

  lines <- c(
    "# Multi-Account GitHub Routing Configurations",
    sprintf(
      "# Generated automatically by ufmmfu::git_setup_ssh_config() on %s",
      Sys.time()
    ),
    "",
    "# Force known_hosts to live in the physical .ssh directory. Without this,",
    "# ssh resolves known_hosts from the process's $HOME, which on some Windows",
    "# machines is redirected into OneDrive and can fail with 'Permission denied'",
    "# when ssh tries to update it (OneDrive locks/syncs the file).",
    "Host *",
    sprintf("    UserKnownHostsFile %s", known_hosts_file),
    ""
  )

  for (alias in names(accounts)) {
    key_path <- file.path(ssh_dir, sprintf("id_rsa_%s", alias))
    lines <- c(
      lines,
      sprintf("Host github.com-%s", alias),
      "    HostName github.com",
      "    User git",
      sprintf("    IdentityFile %s", key_path),
      "    IdentitiesOnly yes",
      ""
    )
  }

  writeLines(lines, con = config_file, sep = "\n")
  message(sprintf("[SUCCESS] SSH config written to physical path: %s", config_file))

  if (!file.exists(known_hosts_file)) {
    file.create(known_hosts_file)
    message(sprintf(
      "[SUCCESS] Created empty known_hosts file at: %s",
      known_hosts_file
    ))
  }

  missing_keys <- character(0)
  for (alias in names(accounts)) {
    key_path <- file.path(ssh_dir, sprintf("id_rsa_%s", alias))
    if (!file.exists(key_path)) {
      missing_keys <- c(missing_keys, key_path)
    }
  }
  if (length(missing_keys) > 0) {
    warning(
      "The following private key files were not found (config points to ",
      "them, but they don't exist yet):\n",
      paste(sprintf("  - %s", missing_keys), collapse = "\n")
    )
  }

  if (isTRUE(bind_global)) {
    ssh_command <- sprintf("ssh -F %s", config_file)
    result <- system(sprintf(
      'git config --global core.sshCommand %s',
      shQuote(ssh_command)
    ))
    if (result == 0) {
      message("[SUCCESS] Git global core.sshCommand bound to the physical config file.")
    } else {
      warning("[WARNING] Failed to set git config --global core.sshCommand.")
    }
  }

  invisible(config_file)
}

#' Check Git Remote Status and Identify Account Routing
#'
#' @return Invisibly returns the remote origin URL string.
#' @export
git_status_check <- function() {
  url <- tryCatch(
    system("git config --get remote.origin.url", intern = TRUE),
    error = function(e)
      NULL
  )
  if (length(url) == 0 || identical(url, "")) {
    stop("Could not retrieve 'origin' remote URL. Are you inside a Git repository?")
  }
  message(sprintf("- Remote Origin URL: %s", url))
  if (grepl("^git@", url)) {
    host_alias <- sub("^git@([^:]+):.*$", "\\1", url)
    message(sprintf("- SSH connection active via Host Alias: %s", host_alias))
  } else {
    warning("This repository is using HTTPS. Switch to SSH using git_set_ssh_account().")
  }
  invisible(url)
}

#' Pull Remote Changes Safely via Active SSH Routing
#'
#' @param branch Character. The target branch name. If NULL, automatically detects the current branch.
#' @return Invisibly returns the system execution result code.
#' @export
git_pull_with_local_id <- function(branch = NULL) {
  if (is.null(branch)) {
    branch <- tryCatch(
      system("git rev-parse --abbrev-ref HEAD", intern = TRUE),
      error = function(e)
        stop("Could not detect active branch.")
    )
  }
  message("- Verifying upstream tracking branch configuration...")
  system(
    sprintf("git branch --set-upstream-to=origin/%s %s", branch, branch),
    ignore.stderr = TRUE
  )
  message(sprintf("- Executing SSH Pull from 'origin/%s'...", branch))
  result <- system(sprintf("git pull origin %s", shQuote(branch)))
  if (result == 0) {
    message("[SUCCESS] Pull completed successfully.")
  } else {
    stop(
      "[ERROR] Git pull failed. Verify your system SSH keys and config file ",
      "paths. Run check_ssh_setup() to diagnose home-directory or missing-key issues."
    )
  }
  invisible(result)
}

#' Push Local Changes Safely via Active SSH Routing
#'
#' @param branch Character. The target branch name. If NULL, automatically detects the current branch.
#' @return Invisibly returns the system execution result code.
#' @export
git_push_with_local_id <- function(branch = NULL) {
  if (is.null(branch)) {
    branch <- tryCatch(
      system("git rev-parse --abbrev-ref HEAD", intern = TRUE),
      error = function(e)
        stop("Could not detect active branch.")
    )
  }
  message(sprintf("- Executing SSH Push to 'origin/%s'...", branch))
  result <- system(sprintf("git push -u origin %s", shQuote(branch)))
  if (result == 0) {
    message("[SUCCESS] Push completed successfully.")
  } else {
    stop(
      "[ERROR] Git push failed. Verify your system SSH keys and config file ",
      "paths. Run check_ssh_setup() to diagnose home-directory or missing-key issues."
    )
  }
  invisible(result)
}

#' Configure Local Repository Remote to Use a Specific SSH Profile
#'
#' @param account_name Character. The name of the routing profile alias (e.g., 'tyrosine', 'phenylalanine').
#' @param project_name Character. The name of the specific GitHub repository.
#' @param owner Character. The GitHub organization or user account that owns the repo. If NULL, defaults to the account_name.
#' @return Invisibly returns the system execution result code.
#' @export
git_set_ssh_account <- function(account_name, project_name, owner = NULL) {
  if (is.null(owner)) {
    owner <- account_name
  }
  ssh_url <- sprintf("git@github.com-%s:%s/%s.git",
                     account_name,
                     owner,
                     project_name)
  message("- Configuring local repository remote...")
  result <- system(sprintf("git remote set-url origin %s", shQuote(ssh_url)))
  if (result == 0) {
    message(sprintf(
      "[SUCCESS] Remote successfully updated for profile [%s]!",
      account_name
    ))
    message(sprintf("- Project Destination: %s/%s", owner, project_name))
    message(sprintf("- Target Routing URL:  %s", ssh_url))
  } else {
    stop(
      "[ERROR] Failed to update remote origin address. Verify your Git repository initialization."
    )
  }
  invisible(result)
}

#' Force local branch to match remote (hard reset)
#'
#' @description
#' Discards all uncommitted local changes and any local commits not present
#' upstream, resetting the current branch to exactly match the corresponding
#' remote branch (e.g. \code{origin/main}). Unlike \code{\link{git_pull_with_local_id}},
#' which performs a standard \code{git pull} (fetch + merge) and will abort on
#' conflicts, this function is intended for situations where you explicitly want
#' the remote version to fully overwrite local files, with no merging involved.
#'
#' @details
#' This is a destructive operation. Any local modifications that have not been
#' committed or stashed will be permanently lost once the reset is executed.
#' Use this function only when you are certain that the remote state should
#' take precedence over local changes (for example, when local edits were made
#' on a different machine and already pushed, and the current machine's copy
#' is stale or diverged unintentionally).
#'
#' The function performs the following steps:
#' \enumerate{
#'   \item Detects the current branch (if not supplied).
#'   \item Configures the upstream tracking branch.
#'   \item Runs \code{git fetch origin} to update remote-tracking references
#'     without touching local files.
#'   \item If \code{stash_backup = TRUE} and uncommitted changes are present,
#'     runs \code{git stash push} with a timestamped message to back them up
#'     before proceeding.
#'   \item Runs \code{git reset --hard origin/<branch>} to move the local
#'     branch pointer and working directory to match the remote exactly.
#' }
#'
#' If \code{stash_backup = TRUE}, discarded local changes are not permanently
#' lost: they can be recovered later by running \code{git stash list} to find
#' the relevant entry and \code{git stash pop} (or \code{git stash apply}) to
#' restore it. Note that a stash pop/apply performed after the reset may itself
#' produce merge conflicts if the stashed changes overlap with the new remote
#' content; this is expected and should be resolved manually.
#'
#' If a step fails due to SSH authentication issues (e.g. "Permission denied
#' (publickey)" or "no such identity"), run \code{\link{check_ssh_setup}} to
#' diagnose whether the local physical home directory diverges from what R's
#' \code{~} resolves to (a common cause on Windows machines with OneDrive
#' folder redirection).
#'
#' @param branch Character. Name of the branch to reset. If \code{NULL}
#'   (default), the current active branch is detected automatically via
#'   \code{git rev-parse --abbrev-ref HEAD}.
#' @param confirm Logical. If \code{TRUE} (default), the function prompts an
#'   interactive confirmation before discarding local changes. Set to
#'   \code{FALSE} to skip the prompt and run non-interactively (use with
#'   caution, e.g. in automated scripts where the destructive behavior is
#'   already expected).
#' @param stash_backup Logical. If \code{TRUE} (default), any uncommitted
#'   local changes are stashed (via \code{git stash push}) with a timestamped
#'   message before the hard reset is executed. This provides a safety net:
#'   the discarded changes remain recoverable from the stash list (see
#'   \code{Details}) instead of being permanently lost. Set to \code{FALSE}
#'   to skip this safeguard and discard local changes immediately.
#'
#' @return Invisibly returns the integer exit status of the \code{git reset}
#'   command (\code{0} on success), or \code{FALSE} if the operation was
#'   cancelled by the user.
#'
#' @seealso \code{\link{git_pull_with_local_id}} for a non-destructive pull
#'   that merges remote changes into local work. \code{\link{check_ssh_setup}}
#'   for diagnosing SSH authentication and home-directory issues.
#'
#' @examples
#' \dontrun{
#' # Reset the current branch to match origin, with confirmation prompt
#' # and automatic stash backup of local changes
#' git_pull_force_remote()
#'
#' # Reset a specific branch without interactive confirmation
#' git_pull_force_remote(branch = "main", confirm = FALSE)
#'
#' # Reset without keeping a stash backup (changes discarded permanently)
#' git_pull_force_remote(stash_backup = FALSE)
#'
#' # Recovering a stash backup later, if needed:
#' # git stash list
#' # git stash pop stash@{0}
#' }
#'
#' @export
git_pull_force_remote <- function(branch = NULL,
                                  confirm = TRUE,
                                  stash_backup = TRUE) {
  if (is.null(branch)) {
    branch <- tryCatch(
      system("git rev-parse --abbrev-ref HEAD", intern = TRUE),
      error = function(e)
        stop("Could not detect active branch.")
    )
  }

  message(
    sprintf(
      "- WARNING: This will discard ALL uncommitted local changes on branch '%s'.",
      branch
    )
  )
  message(sprintf(
    "- Local changes will be replaced with the content of origin/%s.",
    branch
  ))
  if (stash_backup) {
    message("- A safety backup will be created via 'git stash' before the reset.")
  } else {
    message("- No backup will be created. Discarded changes will be permanently lost.")
  }

  if (confirm) {
    resp <- readline(prompt = "Type 'yes' to confirm the forced reset: ")
    if (!identical(tolower(trimws(resp)), "yes")) {
      message("[CANCELLED] Operation aborted by user.")
      return(invisible(FALSE))
    }
  }

  message("- Verifying upstream tracking branch configuration...")
  system(
    sprintf("git branch --set-upstream-to=origin/%s %s", branch, branch),
    ignore.stderr = TRUE
  )

  message(sprintf("- Fetching latest data from 'origin/%s'...", branch))
  fetch_result <- system("git fetch origin")
  if (fetch_result != 0) {
    stop(
      "[ERROR] Git fetch failed. Verify your system SSH keys and config file ",
      "paths. Run check_ssh_setup() to diagnose home-directory or missing-key issues."
    )
  }

  if (stash_backup) {
    status_output <- tryCatch(
      system("git status --porcelain", intern = TRUE),
      error = function(e)
        character(0)
    )
    if (length(status_output) > 0) {
      timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      stash_message <- sprintf("Backup before force reset on branch '%s' (%s)",
                               branch,
                               timestamp)
      message("- Uncommitted changes detected. Creating stash backup...")
      stash_result <- system(sprintf("git stash push -u -m %s", shQuote(stash_message)))
      if (stash_result == 0) {
        message("- Stash backup created successfully. Use 'git stash list' to view it.")
      } else {
        stop("[ERROR] Failed to create stash backup. Aborting before reset to avoid data loss.")
      }
    } else {
      message("- No uncommitted changes detected. Skipping stash backup.")
    }
  }

  message(sprintf(
    "- Resetting local branch to match 'origin/%s' (hard reset)...",
    branch
  ))
  reset_result <- system(sprintf("git reset --hard origin/%s", branch))
  if (reset_result == 0) {
    message("[SUCCESS] Local files now match origin exactly.")
    if (stash_backup) {
      message("- If needed, recover discarded changes with: git stash list / git stash pop")
    }
  } else {
    stop(
      "[ERROR] Git reset failed. Verify your system SSH keys and config file ",
      "paths. Run check_ssh_setup() to diagnose home-directory or missing-key issues."
    )
  }

  invisible(reset_result)
}
