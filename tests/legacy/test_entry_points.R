## The two things a person does with a downloaded clone, and what they used to get.
##
## THE DEFECTS THIS EXISTS FOR, both reported 2026-11-21 by a user running the PUBLISHED 3.6.12
## out of a GitHub zip on a Mac. Neither was reachable from any existing gate, because every gate
## drives ACTA the supported way and these are the two UNsupported ways that look supported.
##
## 1. ACTA_App.R died on `could not find function "container"` -- shiny's own internal parameter
##    name. Cause: the UI passed a bare `pre` as a VALUE (`textOutput(..., container = pre)`), and
##    that user's session held a CHARACTER `pre` in the global environment. Confirmed by them:
##        > find("pre");  class(try(get("pre")))
##        [1] ".GlobalEnv" "package:shiny"
##        [1] "character"
##    A CALL to `div(...)` is immune -- R skips non-function bindings when looking up a function --
##    so of ~170 tag-function uses in the app exactly ONE could break, and it did. Fixed by
##    qualifying it `shiny::pre`.
##
## 2. ACTA_Script.R died on `object 'makeGateLayoutGreatAgain' not found`, AFTER printing
##    "ACTA: helpers from the installed ACTA package (3.6.12)". Cause: the script is the BODY of
##    the pipeline, which run_acta() evaluates with namespace:ACTA as the parent environment;
##    sourced directly it sees only the exports, and 96 of the 103 ACTA names it references are
##    internal. The branch is now guarded and refuses legibly.
##
## EXECUTED, NOT GREPPED. Section 1 poisons a real global environment and sources the real app in
## a real subprocess; section 2 sources the real script and reads the real message. Both carry a
## control proving the hostile condition is actually hostile -- a poison that does not poison, or
## a probe symbol that is reachable anyway, would make the section pass against a reverted fix.
## See the repo rule: a gate must execute, and must be broken on purpose before it is believed.

source(file.path(this.path::this.dir(), "acta_test_paths.R"))
ok <- TRUE
chk <- function(label, pass, detail = NULL) {
  d <- paste(as.character(detail), collapse = " ")
  if (!isTRUE(pass)) { ok <<- FALSE
    cat(sprintf("  [FAIL] %s%s\n", label, if (nzchar(d)) paste0(" -- ", d) else "")) }
  else cat(sprintf("  [ok]   %s\n", label))
}

appFile <- actaTestAppFile()
scrFile <- file.path(ACTA_REPO_ROOT, "inst", "pipeline", "ACTA_Script.R")
RSCRIPT <- file.path(R.home("bin"), "Rscript")

runR <- function(code) {
  f <- tempfile(fileext = ".R"); on.exit(unlink(f), add = TRUE)
  writeLines(code, f)
  out <- suppressWarnings(system2(RSCRIPT, c("--vanilla", shQuote(f)),
                                  stdout = TRUE, stderr = TRUE))
  list(status = attr(out, "status") %||% 0L, out = paste(out, collapse = "\n"))
}
`%||%` <- function(a, b) if (is.null(a)) b else a

haveACTA <- requireNamespace("ACTA", quietly = TRUE)

## ---- 1. the app builds its UI under a POISONED global environment ------------------------------
cat("\n-- 1. app UI vs a shadowed tag function --\n")
if (!file.exists(appFile)) {
  cat("  [skip] no ACTA_App.R\n")
} else {
  wd <- file.path(tempdir(), "acta_entry_work"); dir.create(wd, showWarnings = FALSE)
  r <- runR(c(
    ## Exactly the user's situation: tag-function names bound to non-functions in globalenv.
    'pre <- "poison"; div <- "poison"; span <- "poison"; code <- "poison"; p <- "poison"',
    sprintf('Sys.setenv(ACTA_WORK_DIR = %s)', deparse(wd)),
    ## CONTROL FIRST, and it must run BEFORE the app -- if the poison is not hostile, everything
    ## below is vacuous. This is the pre-3.6.13 expression, verbatim.
    'ctl <- tryCatch({ shiny::textOutput("x", container = pre); "NO_ERROR" },',
    '                error = function(e) "ERRORED")',
    'cat("CONTROL:", ctl, "\n")',
    ## Now the real app, sourced the way shiny::runApp() does.
    sprintf('a <- tryCatch({ suppressMessages(suppressWarnings(source(%s))); "BUILT" },',
            deparse(appFile)),
    '              error = function(e) paste("FAILED:", conditionMessage(e)))',
    'cat("APP:", a, "\n")'))
  ctlLine <- grepl("CONTROL: ERRORED", r$out, fixed = TRUE)
  chk("the poison really is hostile (unqualified form errors under it)", ctlLine,
      if (!ctlLine) r$out else NULL)
  chk("the app builds its UI anyway", grepl("APP: BUILT", r$out, fixed = TRUE),
      if (!grepl("APP: BUILT", r$out, fixed = TRUE)) r$out else NULL)
  ## And the specific symptom is gone, by name.
  chk("...and never reports shiny's internal `container`",
      !grepl("could not find function \"container\"", r$out, fixed = TRUE))
}

## ---- 2. the pipeline script refuses legibly when it is not run by run_acta() --------------------
cat("\n-- 2. ACTA_Script.R sourced directly --\n")
if (!haveACTA) {
  cat("  [skip] ACTA is not installed; this branch needs it\n")
} else if (!file.exists(scrFile)) {
  cat("  [skip] no ACTA_Script.R\n")
} else {
  ## CONTROL: the probe symbol must genuinely be unreachable under library(ACTA) but present in
  ## the namespace. If it were exported, the guard's condition would never fire and section 2
  ## would pass against a script with no guard at all.
  rc <- runR(c('suppressMessages(library(ACTA))',
               'cat("REACHABLE:", !inherits(tryCatch(makeGateLayoutGreatAgain,',
               '      error = function(e) e), "error"), "\n")',
               'cat("INNAMESPACE:", exists("makeGateLayoutGreatAgain",',
               '      envir = asNamespace("ACTA")), "\n")'))
  chk("probe symbol is NOT reachable under library(ACTA)",
      grepl("REACHABLE: FALSE", rc$out, fixed = TRUE), rc$out)
  chk("...but does exist in the namespace (so the guard tests a real thing)",
      grepl("INNAMESPACE: TRUE", rc$out, fixed = TRUE), rc$out)

  r <- runR(c('suppressMessages(library(ACTA))',
              sprintf('m <- tryCatch({ suppressMessages(source(%s)); "NO_ERROR" },', deparse(scrFile)),
              '              error = function(e) conditionMessage(e))',
              'cat("MSG_START\n"); cat(m); cat("\nMSG_END\n")'))
  msg <- sub(".*MSG_START\n", "", sub("\nMSG_END.*", "", r$out))
  chk("it refuses", !grepl("NO_ERROR", msg, fixed = TRUE))
  chk("the message says the script is not an entry point",
      grepl("cannot be run on its own", msg, fixed = TRUE), msg)
  chk("...and names run_acta() as the thing to run instead",
      grepl("run_acta()", msg, fixed = TRUE), msg)
  ## The whole point: the operator must no longer be shown an internal they cannot act on.
  chk("...and does NOT name an internal helper",
      !grepl("makeGateLayoutGreatAgain", msg, fixed = TRUE), msg)
}

## ---- 3. the macOS launcher is executable ------------------------------------------------------
cat("\n-- 3. launcher mode --\n")
cmdFile <- file.path(ACTA_REPO_ROOT, "ACTA App.command")
if (!file.exists(cmdFile)) {
  cat("  [skip] no ACTA App.command\n")
} else {
  ## The INDEX mode is what matters: make_public_release.sh copies tracked files, so a 100644 here
  ## ships a launcher nobody can double-click no matter what the local working tree says.
  mode <- suppressWarnings(system2("git", c("-C", shQuote(ACTA_REPO_ROOT), "ls-files", "-s",
                                            shQuote("ACTA App.command")),
                                   stdout = TRUE, stderr = FALSE))
  if (!length(mode) || !nzchar(paste(mode, collapse = ""))) {
    cat("  [skip] not a git checkout\n")
  } else {
    chk("ACTA App.command is mode 100755 in the git index",
        grepl("^100755", paste(mode, collapse = "")), paste(mode, collapse = " "))
  }
  if (.Platform$OS.type != "windows")
    chk("...and executable on disk", file.access(cmdFile, 1L) == 0L)
}

## ---- 4. drift guard: no bare tag name handed to a caller as a VALUE ----------------------------
## Section 1 proves TODAY's app is immune. This stops a future `container = span` reintroducing it.
## A symbol the file binds itself (local, or a function parameter) is not a hazard -- `value = p`
## at ACTA_App.R:301 is a path variable -- so those are subtracted rather than allow-listed by
## line number, which would rot on the first edit above them.
cat("\n-- 4. tag functions passed as values --\n")
if (!file.exists(appFile)) {
  cat("  [skip] no ACTA_App.R\n")
} else {
  TAGS <- c("pre", "span", "div", "code", "em", "strong", "img", "a", "br", "hr", "p",
            paste0("h", 1:6))
  scanHits <- function(exprs) {
    bound <- character(0); hits <- character(0)
    walk <- function(e) {
      if (is.function(e)) return(invisible())
      if (!is.call(e)) return(invisible())
      fn <- if (is.symbol(e[[1]])) as.character(e[[1]]) else ""
      if (fn %in% c("<-", "=", "<<-") && length(e) >= 2L && is.symbol(e[[2]]))
        bound <<- c(bound, as.character(e[[2]]))
      if (fn == "function" && length(e) >= 2L && !is.null(names(e[[2]])))
        bound <<- c(bound, names(e[[2]]))
      ## `$`/`@`/`::` operands are names, not value references to a tag function.
      if (!fn %in% c("$", "@", "::", ":::")) {
        nms <- names(e)
        if (length(e) > 1L) for (i in 2:length(e)) {
          ## TEST BEFORE BINDING. A formal with no default is the empty symbol, and `a <- e[[i]]`
          ## makes `a` a missing-argument binding that throws the moment anything reads it --
          ## which cost one debugging round here.
          if (identical(e[[i]], quote(expr = ))) next
          a <- e[[i]]
          nm <- if (!is.null(nms)) nms[i] else ""
          if (is.symbol(a) && as.character(a) %in% TAGS && !is.na(nm) && nzchar(nm))
            hits <<- c(hits, sprintf("%s = %s", nm, as.character(a)))
        }
      }
      for (i in seq_along(e)) if (!identical(e[[i]], quote(expr = ))) walk(e[[i]])
    }
    for (e in exprs) walk(e)
    list(hits = unique(hits), bound = unique(bound))
  }
  ## CONTROL: the detector must find a planted one, or an empty result means nothing.
  plant <- scanHits(parse(text = 'f <- function() textOutput("l", container = pre)'))
  chk("detector finds a planted `container = pre`",
      any(grepl("container = pre", plant$hits, fixed = TRUE)), paste(plant$hits, collapse = "; "))

  got  <- scanHits(parse(appFile))
  live <- Filter(function(h) !sub("^.* = ", "", h) %in% got$bound, got$hits)
  chk("no unbound tag function is passed as a named argument in the app",
      !length(live), paste(live, collapse = "; "))
}

cat(sprintf("\n%s\n", if (ok) "ENTRY POINTS: all checks passed" else "ENTRY POINTS: FAILURES ABOVE"))
quit(status = if (ok) 0L else 1L)
