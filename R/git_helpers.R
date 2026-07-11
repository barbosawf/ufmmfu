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
    stop("[ERROR] Git pull failed. Verify your system SSH keys and config file paths.")
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
    stop("[ERROR] Git push failed. Verify your system SSH keys and config file paths.")
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
