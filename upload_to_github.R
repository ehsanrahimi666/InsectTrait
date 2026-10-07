# Push the InsectTrait folder to https://github.com/ehsanrahimi666/InsectTrait
# Needs Git (https://git-scm.com) and a GitHub sign-in. Run in R from the
# InsectTrait folder after editing the package:
#   source("upload_to_github.R")
# Works for a folder that is not yet a Git repository (it is linked to the
# existing GitHub repository first) and for an existing clone.

repo <- "ehsanrahimi666/InsectTrait"
url <- paste0("https://github.com/", repo, ".git")
message_text <- "Update InsectTrait"
check_first <- TRUE   # run R CMD check before uploading

git <- function(..., stop_on_error = TRUE) {
  status <- system2("git", c(...))
  if (!identical(as.integer(status), 0L) && stop_on_error) {
    stop("git ", paste(c(...), collapse = " "), " failed (status ", status, ").", call. = FALSE)
  }
  invisible(status)
}

stopifnot(file.exists("DESCRIPTION"), nzchar(Sys.which("git")))
version <- read.dcf("DESCRIPTION", fields = "Version")[1, 1]

if (check_first && requireNamespace("rcmdcheck", quietly = TRUE)) {
  res <- rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran"), error_on = "warning")
  print(res)
}

if (!dir.exists(".git")) {
  git("init")
  git("remote", "add", "origin", url)
  git("fetch", "origin")
  remote_main <- system2("git", c("rev-parse", "--verify", "--quiet", "origin/main"),
                         stdout = FALSE, stderr = FALSE) == 0L
  if (remote_main) git("reset", "origin/main") else git("checkout", "-b", "main")
}
git("add", "-A")
if (system2("git", c("diff", "--cached", "--quiet")) != 0L) {
  git("commit", "-m", shQuote(paste0(message_text, " (version ", version, ")")))
}
git("branch", "-M", "main")
git("push", "-u", "origin", "main")
tag <- paste0("v", version)
if (system2("git", c("ls-remote", "--exit-code", "--tags", "origin", tag), stdout = FALSE) != 0L) {
  git("tag", "-a", tag, "-m", shQuote(paste("InsectTrait", version)))
  git("push", "origin", tag)
}
message("Done. Install with remotes::install_github(\"", repo, "\")")
