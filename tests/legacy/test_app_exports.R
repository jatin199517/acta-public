## Every helper the Shiny app calls by BARE NAME must be exported.
##
## THE DEFECT THIS EXISTS FOR, found by the user on Windows and not by anything here.
##
## NAMESPACE states the rule in its own header: the app is loaded by shiny::runApp, does
## library(ACTA), and then calls helpers by bare name -- so what it uses must be exported. In
## 3.6.12 `actaInfoFlagTrue()` reached the app without being added to the export list. Under
## library(ACTA) an unexported helper is simply not there, so the app died with
##
##     could not find function "actaInfoFlagTrue"
##
## on the first workbook that set `titerpubs_export` -- which is every real run that builds a
## bundle. The shipped OQ fixtures never reached that branch in the app's own smoke tests, and
## test_install_path.R checks what an install-only user can reach without knowing what the app
## ASKS for. Neither could see it.
##
## PARSED, NOT GREPPED. A regex over the source reports two false positives immediately:
##   * `actaAppChildResult` appears inside a sprintf STRING that builds child-process code, where
##     it is reached as `h$actaAppChildResult` in another session entirely;
##   * `actaRecs` is a reactive defined in the app with `<- reactive(`, which a
##     `<- function` pattern does not see.
## Walking the parse tree gets both right: a string is not a call, and `h$f(x)` is a call to `$`.

source(file.path(this.path::this.dir(), "acta_test_paths.R"))
ok <- TRUE
chk <- function(label, pass, detail = NULL) {
  d <- paste(as.character(detail), collapse = " ")
  if (!isTRUE(pass)) { ok <<- FALSE
    cat(sprintf("  [FAIL] %s%s\n", label, if (nzchar(d)) paste0(" -- ", d) else "")) }
  else cat(sprintf("  [ok]   %s\n", label))
}

appFile <- actaTestAppFile()
nsFile  <- file.path(ACTA_REPO_ROOT, "NAMESPACE")
if (!file.exists(appFile) || !file.exists(nsFile)) {
  cat(sprintf("  [skip] needs the app and NAMESPACE; app:%s ns:%s\n",
              file.exists(appFile), file.exists(nsFile)))
  cat("\nAPP EXPORT TESTS SKIPPED\n"); quit(status = 0L)
}

## ---- what the app CALLS by bare name -----------------------------------------------------------
## A call is `f(...)` where f is a SYMBOL. `h$f(...)` parses as a call whose function is a call to
## `$`, so it is excluded automatically -- which is correct: that one is reached through an
## environment, not through the namespace.
exprs <- parse(appFile)
called <- character(0); assigned <- character(0)
walk <- function(e) {
  if (is.call(e)) {
    f <- e[[1]]
    if (is.symbol(f)) {
      nm <- as.character(f)
      if (nm %in% c("<-", "=", "<<-") && length(e) >= 2L && is.symbol(e[[2]]))
        assigned <<- c(assigned, as.character(e[[2]]))
      else called <<- c(called, nm)
    }
    for (i in seq_along(e)) if (!identical(e[[i]], quote(expr = ))) walk(e[[i]])
  } else if (is.pairlist(e) || is.list(e)) {
    for (i in seq_along(e)) if (!identical(e[[i]], quote(expr = ))) walk(e[[i]])
  }
}
for (e in exprs) walk(e)
called   <- unique(called); assigned <- unique(assigned)
appActa  <- sort(setdiff(grep("^acta", called, value = TRUE), assigned))

## ---- what the package EXPORTS ------------------------------------------------------------------
ns <- readLines(nsFile, warn = FALSE)
exported <- gsub('^export\\("?([^")]*)"?\\).*$', "\\1", grep("^export\\(", ns, value = TRUE))

cat("\n-- the scan is alive --\n")
## CONTROLS. A parse that found nothing would report "all reachable" and mean it vacuously -- the
## exact shape of the defect it is checking for.
chk("the app really does call acta* helpers by bare name", length(appActa) >= 20L,
    sprintf("%d found", length(appActa)))
chk("NAMESPACE really does list exports", length(exported) >= 20L,
    sprintf("%d found", length(exported)))
## ...and that the walker excludes the two shapes a regex gets wrong.
chk("a helper reached as h$f() is NOT counted (it is not a namespace lookup)",
    !("actaAppChildResult" %in% appActa))
chk("a reactive defined in the app is NOT counted", !("actaRecs" %in% appActa))

cat("\n-- every bare-name acta* call is exported --\n")
missing <- setdiff(appActa, exported)
chk(sprintf("all %d of them resolve under library(ACTA)", length(appActa)), !length(missing),
    paste(missing, collapse = ", "))
## NAMED EXPLICITLY, because it is the one that shipped broken and the one a future edit is most
## likely to reintroduce: the app reads three Info flags through it.
chk("actaInfoFlagTrue specifically -- the 3.6.12 regression", "actaInfoFlagTrue" %in% exported)

cat("\n-- and they are really there in the INSTALLED namespace --\n")
## The list above is the CONTRACT; this is the fact. An export line naming a function that does not
## exist would satisfy the check above and still fail at runtime.
if (!requireNamespace("ACTA", quietly = TRUE)) {
  cat("  [skip] ACTA is not installed; the contract check above still ran\n")
} else {
  pkg <- getNamespaceExports("ACTA")
  gone <- setdiff(appActa, pkg)
  chk("every one is present in the installed namespace too", !length(gone),
      paste(gone, collapse = ", "))
  ## And callable, not just present -- an export of a non-function would pass the set test.
  bad <- Filter(function(f) !is.function(tryCatch(get(f, envir = asNamespace("ACTA")),
                                                  error = function(e) NULL)),
                intersect(appActa, pkg))
  chk("...and every one is a function", !length(bad), paste(bad, collapse = ", "))
}

cat(sprintf("\n%s\n", if (ok) "APP EXPORTS: all checks passed" else "APP EXPORTS: FAILURES ABOVE"))
quit(status = if (ok) 0L else 1L)
