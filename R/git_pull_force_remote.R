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
#'   without touching local files.
#'   \item If \code{stash_backup = TRUE} and uncommitted changes are present,
#'   runs \code{git stash push} with a timestamped message to back them up
#'   before proceeding.
#'   \item Runs \code{git reset --hard origin/<branch>} to move the local
#'   branch pointer and working directory to match the remote exactly.
#' }
#'
#' If \code{stash_backup = TRUE}, discarded local changes are not permanently
#' lost: they can be recovered later by running \code{git stash list} to find
#' the relevant entry and \code{git stash pop} (or \code{git stash apply}) to
#' restore it. Note that a stash pop/apply performed after the reset may itself
#' produce merge conflicts if the stashed changes overlap with the new remote
#' content; this is expected and should be resolved manually.
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
#'   that merges remote changes into local work.
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
git_pull_force_remote <- function(branch = NULL, confirm = TRUE, stash_backup = TRUE) {
  if (is.null(branch)) {
    branch <- tryCatch(
      system("git rev-parse --abbrev-ref HEAD", intern = TRUE),
      error = function(e) stop("Could not detect active branch.")
    )
  }

  message(sprintf(
    "- WARNING: This will discard ALL uncommitted local changes on branch '%s'.",
    branch
  ))
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
  system(sprintf("git branch --set-upstream-to=origin/%s %s", branch, branch),
         ignore.stderr = TRUE)

  message(sprintf("- Fetching latest data from 'origin/%s'...", branch))
  fetch_result <- system("git fetch origin")
  if (fetch_result != 0) {
    stop("[ERROR] Git fetch failed. Verify your system SSH keys and config file paths.")
  }

  if (stash_backup) {
    status_output <- tryCatch(
      system("git status --porcelain", intern = TRUE),
      error = function(e) character(0)
    )

    if (length(status_output) > 0) {
      timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      stash_message <- sprintf("Backup before force reset on branch '%s' (%s)", branch, timestamp)

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

  message(sprintf("- Resetting local branch to match 'origin/%s' (hard reset)...", branch))
  reset_result <- system(sprintf("git reset --hard origin/%s", branch))

  if (reset_result == 0) {
    message("[SUCCESS] Local files now match origin exactly.")
    if (stash_backup) {
      message("- If needed, recover discarded changes with: git stash list / git stash pop")
    }
  } else {
    stop("[ERROR] Git reset failed. Verify your system SSH keys and config file paths.")
  }

  invisible(reset_result)
}
