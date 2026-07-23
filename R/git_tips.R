#' Open an HTML Cheat Sheet for This Package's Git Helpers
#'
#' @description
#' Renders a self-contained HTML page documenting every \code{git_*} /
#' SSH-related helper in this package and opens it in the system's default
#' web browser via \code{\link[utils]{browseURL}}. Unlike \code{?function},
#' which documents one function in isolation, this page is organized around
#' \emph{workflows} -- common situations (first-time setup, daily
#' pull/push, an HTTPS remote that needs converting, stashing work in
#' progress, recovering from a forced reset/push) -- and shows which
#' functions are used together, and in what order, to handle each one.
#'
#' @param open Logical. If \code{TRUE} (default), opens the page in the
#'   system browser with \code{\link[utils]{browseURL}}. Set to \code{FALSE}
#'   to only write the file (e.g. in non-interactive sessions).
#' @return Invisibly returns the path to the generated HTML file.
#' @export
#'
#' @examples
#' \dontrun{
#' git_tips()
#' }
git_tips <- function(open = TRUE) {

  workflows <- list(
    list(
      title = "First-time SSH setup on a new machine",
      note  = "Do this once per machine, especially if Documents/user profile is redirected by OneDrive or another cloud-sync tool.",
      steps = c(
        "get_ssh_home()  # confirm where SSH keys/config actually need to live",
        "git_setup_ssh_config(c(work = \"you@work-email.com\", personal = \"you@personal-email.com\"))",
        "check_ssh_setup()  # verify everything lines up; re-run after fixing any warning"
      )
    ),
    list(
      title = "Cloning a repository",
      note  = "Works from a short slug, an HTTPS URL, or an SSH URL -- HTTPS input is converted to SSH automatically.",
      steps = c(
        "git_list_ssh_hosts()  # see which Host aliases are available",
        "git_clone(\"owner/repo\", ssh_alias = \"github.com-work\")"
      )
    ),
    list(
      title = "Push fails with \"repository not found\", or a repo you know exists can't be reached",
      note  = "The most common cause: the local remote is still HTTPS (or its push URL was set separately and never updated).",
      steps = c(
        "git_status_check()  # reports fetch/push URLs and warns if HTTPS is in use",
        "git_convert_to_ssh()  # or git_set_ssh_account(alias, project, owner) for a specific account",
        "git_push_with_local_id()  # also runs this check itself by default (auto_ssh = TRUE)"
      )
    ),
    list(
      title = "Normal daily loop",
      note  = "The _with_local_id() variants always use this machine's own physical SSH config, never a possibly cross-machine-shared core.sshCommand.",
      steps = c(
        "git_pull_with_local_id()",
        "# ... edit files, gert::git_add(), gert::git_commit() ...",
        "git_push_with_local_id()"
      )
    ),
    list(
      title = "Need to put work aside temporarily",
      note  = "No need to drop into a shell for git stash -- these wrap it directly.",
      steps = c(
        "git_stash_save(\"wip: description of what's unfinished\")",
        "# ... switch branches, pull, etc ...",
        "git_stash_list()",
        "git_stash_pop()"
      )
    ),
    list(
      title = "Local copy is broken/diverged -- remote is the source of truth",
      note  = "Destructive: discards local history/changes. A stash backup is created automatically unless stash_backup = FALSE.",
      steps = c(
        "git_pull_force_remote()  # prompts for confirmation, stashes local changes first",
        "git_stash_list()  # only needed if you want to recover the stash afterwards",
        "git_stash_pop()"
      )
    ),
    list(
      title = "Remote history needs to be overwritten with local (after rebase/amend)",
      note  = "Riskier than the previous workflow: rewrites history other machines/people may have fetched. Includes its own divergence check and backup tag.",
      steps = c(
        "git_push_force_remote()  # fetches, checks for remote-only commits, tags the old remote state, then force-pushes"
      )
    ),
    list(
      title = "Something authentication-related looks wrong",
      note  = "Run these top to bottom; each narrows down a different layer of the problem.",
      steps = c(
        "check_ssh_setup()  # '~' vs physical home, core.sshCommand, key files, cloud-sync conflicts",
        "git_status_check()  # is this specific repo's remote using SSH or HTTPS?",
        "git_list_ssh_hosts()  # which aliases actually exist in ~/.ssh/config"
      )
    )
  )

  reference <- list(
    "Setup & diagnostics" = list(
      c("get_os_type()", "Detects windows / macos / linux / unix."),
      c("get_ssh_home()", "Resolves this machine's true physical home directory, routing around OneDrive/Google Drive profile redirection."),
      c("check_cloud_sync_conflicts(path)", "Warns if a path sits inside a cloud-sync folder (OneDrive, Google Drive, iCloud, Dropbox, Box)."),
      c("git_setup_ssh_config(accounts)", "Writes a multi-account SSH config (Host aliases) into the physical .ssh directory and binds core.sshCommand."),
      c("check_ssh_setup()", "Full diagnostic: compares R's '~' to the physical home, verifies core.sshCommand, lists keys found."),
      c("git_list_ssh_hosts()", "Lists the Host aliases currently defined in ~/.ssh/config."),
      c("git_ssh_env() / git_system2()", "Low-level building blocks: per-call GIT_SSH_COMMAND override used internally by every network-facing helper below.")
    ),
    "Remote protocol (HTTPS vs SSH)" = list(
      c("git_remote_urls(remote)", "Reports a remote's fetch AND push URLs separately -- a remote can have a push URL that silently diverges from its fetch URL."),
      c("git_status_check(remote)", "Prints a remote's URLs and warns if HTTPS is in use anywhere."),
      c("git_convert_to_ssh(remote, ssh_alias)", "Rewrites an HTTPS remote (fetch and/or push URL) to SSH automatically. Called by default from the functions below (auto_ssh = TRUE)."),
      c("git_set_ssh_account(account_name, project_name, owner)", "Points the local repo's remote at a specific ~/.ssh/config Host alias.")
    ),
    "Cloning" = list(
      c("git_clone(repo, ssh_alias, ...)", "Clones a repo via SSH from a short slug, HTTPS URL, or SSH URL, converting HTTPS input automatically."),
      c("git_build_ssh_url(repo, host, ssh_alias)", "Builds the SSH remote URL string used by git_clone() and git_convert_to_ssh().")
    ),
    "Daily pull / push" = list(
      c("git_pull_with_local_id(branch, auto_ssh)", "Pull via this machine's own physical SSH config; auto-converts an HTTPS remote first by default."),
      c("git_push_with_local_id(branch, auto_ssh)", "Same, for push.")
    ),
    "Stashing" = list(
      c("git_stash_save(message_text, include_untracked)", "Stash uncommitted local changes with a message; no-ops cleanly if there's nothing to stash."),
      c("git_stash_list()", "Prints and returns the current stash entries."),
      c("git_stash_pop(stash_ref)", "Restores a stash entry (default: the most recent).")
    ),
    "Destructive / recovery (danger zone)" = list(
      c("git_pull_force_remote(branch, confirm, stash_backup, auto_ssh)", "Hard-resets the local branch to match origin, stashing local changes first by default."),
      c("git_push_force_remote(branch, confirm, use_lease, backup_tag, auto_ssh)", "Force-pushes local history over the remote's, with divergence detection and a local backup tag.")
    )
  )

  esc <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x
  }

  workflow_html <- vapply(workflows, function(w) {
    steps_html <- paste(
      vapply(w$steps, function(s) {
        is_comment <- grepl("^\\s*#", s)
        cls <- if (is_comment) " class=\"cmt\"" else ""
        sprintf("<li><code%s>%s</code></li>", cls, esc(s))
      }, character(1)),
      collapse = "\n      "
    )
    sprintf(
      '<div class="card">\n  <h3>%s</h3>\n  <p class="note">%s</p>\n  <ol>\n      %s\n  </ol>\n</div>',
      esc(w$title), esc(w$note), steps_html
    )
  }, character(1))

  reference_html <- vapply(names(reference), function(group) {
    rows <- paste(
      vapply(reference[[group]], function(entry) {
        sprintf("<tr><td><code>%s</code></td><td>%s</td></tr>", esc(entry[1]), esc(entry[2]))
      }, character(1)),
      collapse = "\n        "
    )
    sprintf(
      '<h3>%s</h3>\n<table>\n  <tbody>\n        %s\n  </tbody>\n</table>',
      esc(group), rows
    )
  }, character(1))

  html <- sprintf('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<title>ufmmfu :: Git helpers cheat sheet</title>
<style>
  :root {
    color-scheme: light dark;
    --bg: #f7f7f8; --fg: #1c1c1e; --card: #ffffff; --border: #e0e0e3;
    --accent: #2f6fed; --code-bg: #eef1f6; --muted: #6b6b70;
  }
  @media (prefers-color-scheme: dark) {
    :root { --bg: #16171a; --fg: #eaeaec; --card: #202126; --border: #33343a;
            --accent: #6ea2ff; --code-bg: #292a30; --muted: #a0a0a6; }
  }
  * { box-sizing: border-box; }
  body {
    margin: 0; padding: 2rem 1.25rem 4rem; background: var(--bg); color: var(--fg);
    font-family: -apple-system, "Segoe UI", Roboto, Helvetica, Arial, sans-serif;
    line-height: 1.5;
  }
  .wrap { max-width: 880px; margin: 0 auto; }
  h1 { font-size: 1.6rem; margin-bottom: 0.25rem; }
  .subtitle { color: var(--muted); margin-top: 0; margin-bottom: 2rem; }
  h2 { font-size: 1.15rem; border-bottom: 1px solid var(--border); padding-bottom: 0.4rem;
       margin-top: 2.5rem; }
  .card {
    background: var(--card); border: 1px solid var(--border); border-radius: 10px;
    padding: 1rem 1.25rem; margin: 1rem 0;
  }
  .card h3 { margin-top: 0; margin-bottom: 0.3rem; font-size: 1.02rem; }
  .card .note { color: var(--muted); margin-top: 0; margin-bottom: 0.75rem; font-size: 0.92rem; }
  .card ol { margin: 0; padding-left: 1.25rem; }
  .card li { margin: 0.15rem 0; }
  code {
    background: var(--code-bg); border-radius: 4px; padding: 0.1rem 0.35rem;
    font-family: ui-monospace, SFMono-Regular, Consolas, "Courier New", monospace;
    font-size: 0.88rem;
  }
  code.cmt { background: transparent; color: var(--muted); font-style: italic; }
  table { width: 100%%; border-collapse: collapse; margin: 0.5rem 0 1.5rem; }
  td { padding: 0.4rem 0.6rem; border-bottom: 1px solid var(--border); vertical-align: top; }
  td:first-child { white-space: nowrap; width: 1%%; }
  td:last-child { color: var(--fg); }
  footer { color: var(--muted); font-size: 0.85rem; margin-top: 3rem; text-align: center; }
</style>
</head>
<body>
<div class="wrap">
  <h1>ufmmfu :: Git helpers cheat sheet</h1>
  <p class="subtitle">Generated by <code>git_tips()</code> on %s. Workflows first, full function reference below.</p>

  <h2>Workflows</h2>
  %s

  <h2>Function reference</h2>
  %s

  <footer>Run <code>git_tips()</code> again any time to regenerate this page.</footer>
</div>
</body>
</html>
', esc(format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
   paste(workflow_html, collapse = "\n\n  "),
   paste(reference_html, collapse = "\n\n  "))

  path <- file.path(tempdir(), "ufmmfu_git_tips.html")
  con <- file(path, open = "w", encoding = "UTF-8")
  writeLines(html, con, useBytes = TRUE)
  close(con)

  if (isTRUE(open)) {
    utils::browseURL(path)
  }

  invisible(path)
}
