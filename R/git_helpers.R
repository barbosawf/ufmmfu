#' Detect the Current Operating System
#'
#' Small cross-platform helper used throughout this file to branch logic for
#' Windows, macOS, and Linux (and other Unix-likes) without repeating
#' `.Platform$OS.type` / `Sys.info()` checks in every function.
#'
#' @return Character. One of `"windows"`, `"macos"`, `"linux"`, or `"unix"`
#'   (the last is a catch-all for other Unix-like systems).
#' @export
get_os_type <- function() {
  if (.Platform$OS.type == "windows") {
    return("windows")
  }
  sysname <- tolower(Sys.info()[["sysname"]])
  if (identical(sysname, "darwin")) {
    return("macos")
  }
  if (identical(sysname, "linux")) {
    return("linux")
  }
  "unix"
}

#' Resolve the True Physical Home Directory for SSH Purposes
#'
#' Cloud-sync clients can redirect a user's home or profile folder away from
#' the physical location that Git Bash / OpenSSH actually use as `$HOME`,
#' which causes SSH to look for keys and the `config` file in the wrong
#' place (errors such as "no such identity" even though the keys exist on
#' disk). This is most common on **Windows**, where OneDrive (and, less
#' frequently, Google Drive for desktop) can redirect the "Documents"
#' folder or, in some managed/corporate setups, the user profile itself.
#'
#' On **macOS** and **Linux**, `$HOME` itself is essentially never
#' redirected by cloud-sync tools: iCloud Drive's "Desktop & Documents
#' Sync" only moves `~/Desktop` and `~/Documents`, and Google
#' Drive/Dropbox/OneDrive on these platforms sync a dedicated subfolder
#' rather than replacing `$HOME`. This function still resolves `$HOME`
#' explicitly on every platform (rather than relying on R's `~`, which can
#' behave inconsistently across locales and setups) and pairs with
#' [check_cloud_sync_conflicts()] to flag the rarer cases where `~/.ssh`
#' itself ends up inside a synced folder.
#'
#' @return Character. A normalized path (forward slashes) to the user's
#'   real, physical home directory.
#' @export
get_ssh_home <- function() {
  os <- get_os_type()

  if (os == "windows") {
    home <- Sys.getenv("USERPROFILE", unset = NA)
    if (is.na(home) || !nzchar(home) || !dir.exists(home)) {
      drive <- Sys.getenv("HOMEDRIVE", unset = "C:")
      path  <- Sys.getenv("HOMEPATH", unset = "")
      home  <- paste0(drive, path)
    }
    home <- gsub("\\\\", "/", home)
  } else {
    # macOS, Linux, and other Unix-likes: $HOME is the canonical, reliable
    # source. Falling back to normalizePath("~") only if $HOME is unset,
    # which should not normally happen on these platforms.
    home <- Sys.getenv("HOME", unset = NA)
    if (is.na(home) || !nzchar(home)) {
      home <- normalizePath("~", mustWork = FALSE)
    }
  }
  home
}

#' Warn About Cloud-Sync Folders That May Interfere with SSH
#'
#' Checks whether a given directory (typically `~/.ssh`) appears to live
#' inside a folder managed by a cloud-storage sync client -- OneDrive,
#' Google Drive for desktop, iCloud Drive, Dropbox, or Box are covered by
#' name. These tools can lock files during sync, replace real files with
#' online-only placeholders, or introduce upload delays, any of which can
#' cause intermittent "Permission denied" or "no such identity" SSH errors
#' that have nothing to do with the SSH configuration itself.
#'
#' This is a best-effort, name-based heuristic (it looks for known folder
#' name fragments in the path) rather than a guarantee, since sync clients
#' can be configured with custom folder names.
#'
#' @param path Character. Path to check, e.g. the value returned by
#'   `file.path(get_ssh_home(), ".ssh")`.
#' @return Invisibly returns `TRUE` if a cloud-sync folder name was
#'   detected in `path` (after emitting a `warning()`), or `FALSE`
#'   otherwise.
#' @export
check_cloud_sync_conflicts <- function(path) {
  normalized <- tolower(gsub("\\\\", "/", path))

  providers <- c(
    "onedrive"     = "OneDrive",
    "google drive" = "Google Drive",
    "googledrive"  = "Google Drive",
    "my drive"     = "Google Drive",
    "icloud"       = "iCloud Drive",
    "dropbox"      = "Dropbox",
    "box sync"     = "Box"
  )

  hit <- names(providers)[vapply(names(providers), function(p) grepl(p, normalized, fixed = TRUE), logical(1))]

  if (length(hit) > 0) {
    warning(
      sprintf(
        "The path '%s' appears to be inside a %s-synced folder. Cloud-sync clients can lock, delay, or replace files (including SSH keys and config) during sync, which may cause intermittent authentication errors. If you hit unexplained SSH failures, consider excluding this folder from sync or moving it outside the synced tree.",
        path,
        unname(providers[hit[1]])
      ),
      call. = FALSE
    )
    return(invisible(TRUE))
  }

  invisible(FALSE)
}

#' Diagnose SSH Home Resolution and Configuration Status
#'
#' Compares R's default `~` resolution against the physical home directory
#' returned by \code{\link{get_ssh_home}}, warning if they diverge (a sign
#' of profile-folder redirection, most often seen on Windows). Also reports
#' whether an SSH \code{config} file is present, and lists which private
#' key files exist in the physical \code{.ssh} directory. Additionally runs
#' \code{\link{check_cloud_sync_conflicts}} against the \code{.ssh}
#' directory to flag OneDrive, Google Drive, iCloud Drive, Dropbox, or Box
#' sync folders that could interfere with key files.
#'
#' @param key_names Character vector or \code{NULL}. Base names (without
#'   extension) of private keys to look for inside the physical
#'   \code{.ssh} directory, e.g. \code{c("id_rsa_work", "id_rsa_personal")}.
#'   When \code{NULL} (default), every file in \code{.ssh} that looks like
#'   a private key (\code{id_rsa_*}, \code{id_ed25519_*}, or
#'   \code{id_ecdsa_*}, with no \code{.pub} extension) is auto-discovered
#'   and reported instead, so this function works out of the box for any
#'   user regardless of the account aliases they have set up.
#' @return Invisibly returns a list with \code{r_home}, \code{physical_home},
#'   \code{ssh_dir}, \code{config_exists}, and \code{keys_found}.
#' @export
#'
#' @examples
#' \dontrun{
#' # Auto-discover whatever keys are present
#' check_ssh_setup()
#'
#' # Check specifically for named account keys
#' check_ssh_setup(key_names = c("id_rsa_work", "id_rsa_personal"))
#' }
check_ssh_setup <- function(key_names = NULL) {
  r_home        <- tryCatch(
    normalizePath("~", mustWork = FALSE),
    error = function(e)
      NA
  )
  physical_home <- get_ssh_home()
  ssh_dir       <- file.path(physical_home, ".ssh")
  config_file   <- file.path(ssh_dir, "config")

  message(sprintf("- Operating system:         %s", get_os_type()))
  message(sprintf("- R's '~' resolves to:      %s", r_home))
  message(sprintf("- Physical SSH home is:     %s", physical_home))

  if (!identical(gsub("\\\\", "/", r_home), gsub("\\\\", "/", physical_home))) {
    warning(
      "R's '~' and the physical home directory differ. This usually means ",
      "a cloud-sync tool (OneDrive, Google Drive, etc.) has redirected your ",
      "Documents/user profile folder. Use get_ssh_home() instead of '~' for ",
      "anything SSH-related."
    )
  } else {
    message("[OK] No divergence detected between R's '~' and the physical home.")
  }

  check_cloud_sync_conflicts(ssh_dir)

  config_exists <- file.exists(config_file)
  message(sprintf("- SSH config file %s at: %s", if (config_exists) {
    "found"
  } else {
    "NOT found" }, config_file))

  if (is.null(key_names)) {
    all_files  <- if (dir.exists(ssh_dir)) list.files(ssh_dir) else character(0)
    key_names  <- all_files[grepl("^id_(rsa|ed25519|ecdsa)_", all_files) & !grepl("\\.pub$", all_files)]
    if (length(key_names) == 0) {
      message("- No account-specific private keys (id_rsa_*, id_ed25519_*, id_ecdsa_*) were found to report on.")
    }
  }

  keys_found <- stats::setNames(logical(length(key_names)), key_names)
  for (k in key_names) {
    key_path <- file.path(ssh_dir, k)
    exists   <- file.exists(key_path)
    keys_found[[k]] <- exists
    message(sprintf("- Key '%s' %s at: %s", k, if (exists) {
      "found"
    }
    else{
      "NOT found"
    }, key_path))
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
#'   e.g. \code{c(work = "you@work-email.com", personal = "you@personal-email.com")}.
#' @param bind_global Logical. If \code{TRUE} (default), also sets
#'   \code{git config --global core.sshCommand} to point at this config file.
#' @return Invisibly returns the path to the written config file.
#' @export
#'
#' @examples
#' \dontrun{
#' git_setup_ssh_config(c(work = "you@work-email.com",
#'                        personal = "you@personal-email.com"))
#' }
git_setup_ssh_config <- function(accounts, bind_global = TRUE) {
  if (is.null(names(accounts)) || any(!nzchar(names(accounts)))) {
    stop("'accounts' must be a named character vector: alias = email.")
  }

  ssh_dir     <- file.path(get_ssh_home(), ".ssh")
  config_file <- file.path(ssh_dir, "config")

  if (!dir.exists(ssh_dir)) {
    dir.create(ssh_dir, recursive = TRUE, showWarnings = FALSE)
  }

  check_cloud_sync_conflicts(ssh_dir)

  known_hosts_file <- file.path(ssh_dir, "known_hosts")

  lines <- c(
    "# Multi-Account GitHub Routing Configurations",
    sprintf(
      "# Generated automatically by git_setup_ssh_config() on %s",
      Sys.time()
    ),
    "",
    "# Force known_hosts to live in the physical .ssh directory. Without this,",
    "# ssh resolves known_hosts from the process's $HOME, which on some",
    "# machines is redirected into a cloud-sync folder (OneDrive, Google Drive,",
    "# iCloud Drive, Dropbox, etc.) and can fail with 'Permission denied' when",
    "# ssh tries to update it (the sync client locks/holds the file).",
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
#' @param account_name Character. The name of the routing profile alias, matching a
#'   `Host github.com-<account_name>` entry in `~/.ssh/config` (e.g., created via
#'   [git_setup_ssh_config()]). Example aliases: `"work"`, `"personal"`.
#' @param project_name Character. The name of the specific GitHub repository.
#' @param owner Character. The GitHub organization or user account that owns the repo. If NULL, defaults to the account_name.
#' @return Invisibly returns the system execution result code.
#' @export
#'
#' @examples
#' \dontrun{
#' # Point the current repo's origin at the "work" SSH profile
#' git_set_ssh_account("work", project_name = "my-repo", owner = "my-org")
#' }
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

#' List SSH host aliases configured in ~/.ssh/config
#'
#' Parses the user's SSH config file and returns every `Host` entry defined
#' there. This is used to discover the host aliases created for multi-account
#' Git workflows (e.g. `github.com-work`, `github.com-personal`), so that
#' [git_clone()] can validate an alias before attempting to use it.
#'
#' @param ssh_config_path Character. Path to the SSH config file. Defaults to
#'   `~/.ssh/config`, which is expanded with [path.expand()] so it resolves
#'   correctly on both Windows and Ubuntu.
#'
#' @return A character vector of host aliases (may be empty if the file does
#'   not exist or contains no `Host` entries).
#' @export
#'
#' @examples
#' \dontrun{
#' git_list_ssh_hosts()
#' }
git_list_ssh_hosts <- function(ssh_config_path = "~/.ssh/config") {
  ssh_config_path <- path.expand(ssh_config_path)

  if (!file.exists(ssh_config_path)) {
    return(character(0))
  }

  lines <- readLines(ssh_config_path, warn = FALSE)
  host_lines <- grep("^\\s*Host\\s+", lines, value = TRUE, ignore.case = TRUE)

  # A "Host" line can declare multiple space-separated aliases/patterns.
  aliases <- unlist(strsplit(trimws(sub("^\\s*Host\\s+", "", host_lines,
                                        ignore.case = TRUE)), "\\s+"))

  # Drop wildcard-only entries (e.g. "Host *"), which are not real aliases.
  aliases <- aliases[!grepl("[*?]", aliases)]

  unique(aliases)
}

#' Clone a Git repository using an SSH host alias
#'
#' Generic, cross-platform wrapper around `git clone` that builds the correct
#' SSH remote URL from a repository slug (`"owner/repo"`), an optional SSH
#' host alias, and an optional custom Git host. Credential resolution is fully
#' delegated to the SSH layer (via `~/.ssh/config`), matching the multi-account
#' pattern already used elsewhere in this package (see `git_pull_force_remote()`
#' and the other `git_*` helpers) instead of embedding tokens or passwords in R.
#'
#' Any user can rely on this function regardless of how many GitHub/GitLab
#' accounts they manage: as long as the relevant `Host` alias is set up in
#' `~/.ssh/config`, `git_clone()` takes care of building the right remote URL
#' and calling `git clone`.
#'
#' @param repo Character. Repository identifier. Accepts either:
#'   * a short slug, `"owner/repo"` (most common case), or
#'   * a full SSH URL, `"git@host:owner/repo.git"`, or
#'   * a full HTTPS URL, `"https://host/owner/repo.git"` (automatically
#'     converted to SSH so the configured alias/keys are used).
#' @param dest_dir Character. Destination directory for the clone. Defaults to
#'   the repository name (i.e. what `git clone` would use on its own).
#' @param host Character. Git hosting domain, used only when `repo` is a
#'   short slug. Defaults to `"github.com"`.
#' @param ssh_alias Character or `NULL`. SSH `Host` alias configured in
#'   `~/.ssh/config` for the desired account (e.g. `"github.com-work"`). When
#'   supplied, it replaces `host` in the SSH remote so Git/SSH pick the right
#'   identity file automatically. When `NULL` (default), `host` is used
#'   directly, which works fine for a single-account setup or when the
#'   default SSH identity already has access.
#' @param branch Character or `NULL`. If supplied, passed to `git clone` as
#'   `--branch <branch>`.
#' @param depth Integer or `NULL`. If supplied, passed to `git clone` as
#'   `--depth <depth>` for a shallow clone.
#' @param overwrite Logical. If `TRUE` and `dest_dir` already exists, it is
#'   removed before cloning. Defaults to `FALSE`.
#' @param quiet Logical. If `TRUE`, suppresses `git`'s own progress output
#'   (still returns an error on failure). Defaults to `FALSE`.
#'
#' @return Invisibly, the normalized path to the cloned repository. Stops
#'   with an informative error if `git` is not available, if the alias is not
#'   found in `~/.ssh/config`, or if `git clone` itself fails.
#' @export
#'
#' @examples
#' \dontrun{
#' # Simple clone using the default SSH identity
#' git_clone("tidyverse/dplyr")
#'
#' # Clone using a specific SSH host alias (multi-account setup)
#' git_clone("my-org/private-repo", ssh_alias = "github.com-work")
#'
#' # Shallow clone of a specific branch into a custom directory
#' git_clone("my-account/my-package", dest_dir = "my-package-dev",
#'           branch = "develop", depth = 1)
#' }
git_clone <- function(repo,
                      dest_dir = NULL,
                      host = "github.com",
                      ssh_alias = NULL,
                      branch = NULL,
                      depth = NULL,
                      overwrite = FALSE,
                      quiet = FALSE) {

  if (nchar(Sys.which("git")) == 0) {
    stop("git was not found on PATH. Please install Git and try again.",
         call. = FALSE)
  }

  ssh_url <- git_build_ssh_url(repo, host = host, ssh_alias = ssh_alias)

  # Infer destination directory from the repo name when not supplied,
  # mirroring git clone's own default behavior.
  if (is.null(dest_dir)) {
    repo_name <- sub("\\.git$", "", basename(ssh_url))
    dest_dir <- repo_name
  }

  dest_dir <- path.expand(dest_dir)

  if (dir.exists(dest_dir)) {
    if (!overwrite) {
      stop(sprintf(
        "Destination '%s' already exists. Use overwrite = TRUE to replace it.",
        dest_dir
      ), call. = FALSE)
    }
    unlink(dest_dir, recursive = TRUE, force = TRUE)
  }

  args <- c("clone")
  if (quiet) args <- c(args, "--quiet")
  if (!is.null(branch)) args <- c(args, "--branch", branch)
  if (!is.null(depth)) args <- c(args, "--depth", as.character(depth))
  args <- c(args, ssh_url, dest_dir)

  status <- system2("git", args, stdout = if (quiet) {FALSE} else {""},
                    stderr = if (quiet) {FALSE} else {""})

  if (status != 0) {
    stop(sprintf(
      "git clone failed (exit status %s) for '%s'. Check that the SSH key for '%s' is loaded and has repository access.",
      status, ssh_url, ssh_alias %||% host
    ), call. = FALSE)
  }

  invisible(normalizePath(dest_dir))
}

#' Build an SSH remote URL for a repository
#'
#' Internal helper used by [git_clone()] (and reusable by other `git_*`
#' helpers) to normalize a repository identifier into a proper SSH remote
#' URL, optionally substituting an SSH host alias in place of the real host
#' so that `ssh`/`git` resolve the correct identity file for multi-account
#' setups.
#'
#' @inheritParams git_clone
#' @return Character. A single SSH remote URL, e.g.
#'   `"git@github.com-work:owner/repo.git"`.
#' @keywords internal
#' @export
git_build_ssh_url <- function(repo, host = "github.com", ssh_alias = NULL) {

  if (!is.null(ssh_alias)) {
    configured_hosts <- git_list_ssh_hosts()
    if (length(configured_hosts) > 0 && !(ssh_alias %in% configured_hosts)) {
      warning(sprintf(
        "SSH alias '%s' was not found in ~/.ssh/config. Proceeding anyway; git/ssh will report an error if it cannot resolve the host.",
        ssh_alias
      ), call. = FALSE)
    }
  }

  effective_host <- ssh_alias %||% host

  # Already a full SSH remote: just swap in the alias if one was requested.
  if (grepl("^git@", repo)) {
    slug <- sub("^git@[^:]+:", "", repo)
    return(sprintf("git@%s:%s", effective_host, slug))
  }

  # Full HTTPS remote: convert to SSH so the configured alias/keys apply.
  if (grepl("^https?://", repo)) {
    slug <- sub("^https?://[^/]+/", "", repo)
    slug <- sub("\\.git$", "", slug)
    return(sprintf("git@%s:%s.git", effective_host, slug))
  }

  # Otherwise assume a short "owner/repo" slug.
  slug <- sub("\\.git$", "", repo)
  sprintf("git@%s:%s.git", effective_host, slug)
}

# Small internal null-coalescing helper (kept local so this file has no
# hard dependency on rlang, matching the rest of the package's helpers).
`%||%` <- function(x, y) {if (is.null(x)) {y} else {x}}
