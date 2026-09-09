## ---------------------------------------------------------------------------------------------
## ONE WORKING-DIRECTORY CONTRACT FOR THE WHOLE SUITE.
##
## Why this exists: the scripts in this folder disagreed about where they had to be run from. Most
## resolved the version folder as the literal string "2_88_WIP" relative to the CURRENT directory,
## so they only worked from the repo root -- while test_compensation.R sourced "ACTA_Functions.R"
## from the current directory, so it only worked from INSIDE the version folder, and smoke_app.R
## did setwd("2_88_WIP"). Run the suite either way and several scripts failed for a reason that had
## nothing to do with the code under test, which is worse than no gate at all: a spurious failure
## teaches people to ignore the output.
##
## Every script now locates ITSELF and derives the rest, so the current directory does not matter:
##
##     .../tests/  ->  ACTA_VERSION_DIR (code)  ->  ACTA_REPO_ROOT  ->  ACTA_CASE_ROOT
##
## An explicit first argument still overrides the version folder -- that is how the suite is pointed
## at another version (`Rscript 2_99/tests/test_preflight.R 2_89`). A path that does not exist
## is a HARD ERROR and never a silent fall back to this folder's own version: falling back would
## report a PASS for a version nobody tested.
## ---------------------------------------------------------------------------------------------

## this.path is already a pipeline dependency (the script and the .Rmd both use it to find
## themselves), so using it here adds nothing to install. The base-R fallback is there so a missing
## package cannot stop a gate from running -- Rscript always passes --file=, and source()ing this
## file from a sibling script leaves that argument pointing at the sibling, which is the same folder.
.actaTestSelfDir <- function() {
  if (requireNamespace("this.path", quietly = TRUE)) {
    d <- try(this.path::this.dir(), silent = TRUE)
    if (!inherits(d, "try-error") && length(d) == 1L && !is.na(d) && dir.exists(d))
      return(normalizePath(d))
  }
  f <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(f)) {
    ## Rscript ENCODES each space in --file= as "~+~" (see ?commandArgs), so a naive sub() returns a
    ## path that does not exist on any machine whose project lives under "Flow Cytometry" or
    ## "TIER 1". That is exactly this repo, and it made all eight gates fail at once when they were
    ## launched by absolute path -- while the same scripts passed when launched by relative path,
    ## because a relative path here happens to contain no spaces.
    d <- dirname(gsub("~+~", " ", sub("^--file=", "", f[[1]]), fixed = TRUE))
    if (dir.exists(d)) return(normalizePath(d))
  }
  normalizePath(".")
}

ACTA_TEST_DIR <- .actaTestSelfDir()

## TWO LAYOUTS, DETECTED RATHER THAN ASSUMED.
##
##   version-folder repo   <repo>/<version>/tests/
##                         code  = <version>/            cases = <version>/Diagnostics/
##   installed-package     <pkg>/tests/legacy/
##                         code  = <pkg>/inst/pipeline/   cases = <pkg>/inst/extdata/oq_small/
##
## The old contract took `dirname(tests)` as the code folder, full stop. That is right for the
## version-folder repo and wrong for a package, where `dirname` lands on `<pkg>/tests` -- a folder
## with no ACTA_Script*.R in it, so every gate failed on a path error rather than on anything it was
## testing. Detection is by looking for the script, which is the same "exactly one ACTA_Script*.R"
## rule run_acta() and actaOQRun() already use, so the three agree by construction.
.actaTestLayout <- function(testDir) {
  hasScript <- function(d) dir.exists(d) && length(list.files(d, pattern = "^ACTA_Script.*[.]R$")) == 1L
  up1 <- normalizePath(dirname(testDir))
  ## version-folder layout: the scripts sit directly above tests/
  if (hasScript(up1))
    return(list(kind = "version", code = up1, root = normalizePath(dirname(up1)),
                cases = file.path(up1, "Diagnostics")))
  ## package layout: walk up for a root carrying inst/pipeline. Bounded, because an unbounded walk
  ## on a machine with an ACTA checkout somewhere above would silently test the WRONG tree.
  d <- up1
  for (i in 1:4) {
    ip <- file.path(d, "inst", "pipeline")
    if (hasScript(ip))
      return(list(kind = "package", code = normalizePath(ip), root = normalizePath(d),
                  cases = file.path(d, "inst", "extdata", "oq_small")))
    nd <- dirname(d); if (identical(nd, d)) break; d <- nd
  }
  stop(sprintf(paste("acta_test_paths: cannot locate the pipeline from '%s'.",
                     "Expected either <version>/ACTA_Script*.R one level up, or",
                     "<pkg>/inst/pipeline/ACTA_Script*.R above that."), testDir), call. = FALSE)
}

ACTA_TEST_LAYOUT <- .actaTestLayout(ACTA_TEST_DIR)
ACTA_VERSION_DIR <- ACTA_TEST_LAYOUT$code
ACTA_REPO_ROOT   <- ACTA_TEST_LAYOUT$root
ACTA_CASE_ROOT   <- ACTA_TEST_LAYOUT$cases

## Where the OQ diagnostic cases live. Never spell "Diagnostics" in a test again -- it is
## `Diagnostics/` in the version-folder repo and `inst/extdata/oq_small/` in a package.
actaTestCaseRoot <- function() ACTA_CASE_ROOT
actaTestCaseDirs <- function() sort(Sys.glob(file.path(ACTA_CASE_ROOT, "OQ_Test*")))

## from_args = FALSE for scripts whose arguments mean something else (verify_oq_sanitised.R takes
## case folders, regression_compare.R takes two snapshots).
actaTestVersionDir <- function(from_args = TRUE) {
  a <- commandArgs(trailingOnly = TRUE)
  if (from_args && length(a) && nzchar(a[[1]])) {
    if (!dir.exists(a[[1]]))
      stop(sprintf("acta_test_paths: version folder '%s' does not exist (working directory is %s)",
                   a[[1]], getwd()), call. = FALSE)
    return(normalizePath(a[[1]]))
  }
  ACTA_VERSION_DIR
}

## WHERE THE APP TESTS RUN.
##
## `ACTA_App.R` sets APP_DIR <- this.path::this.dir() and then expects the instructions workbook,
## Template/ and the pipeline scripts to all be in that one folder. `run_acta()` and `actaOQRun()`
## both separate code_dir from version_dir; the APP DOES NOT -- which is fine in a version folder and
## is the reason the app cannot be run in place from a package, where inst/pipeline/ is code-only by
## design and no user workbook lives there. (Recorded as an open design item: the app needs an
## override in the shape of the existing ACTA_LAYOUT_DIR.)
##
## So in a package layout the app tests get a STAGED working directory -- the code, the templates and
## a copy of the blank instructions workbook, which is exactly what a user assembles after installing
## -- rather than assertions weakened to accommodate a folder the app was never meant to run from.
## Version-folder layout is unchanged: it already IS a working directory.
.acta_app_dir <- NULL
actaTestAppDir <- function() {
  ## A version folder is used DIRECTLY only while it actually has a working workbook. A FRESH CLONE
  ## has none -- the workbook carries real experiment data, so it is gitignored and can never ship --
  ## and without one the app's pre-flight panel has no subject, so every assertion about that panel
  ## fails. Discovered 2026-09-04 by building the public export: a newly cloned repo failed 2 of 11
  ## test scripts on the first thing a new user does.
  ##
  ## The staging path below already solves this for the package layout, and it solves it the same way
  ## the README tells a USER to start: copy the blank workbook out of Template/ and work beside it.
  ## So fall through to it rather than skipping the assertions. A test that cannot see its subject
  ## must not report a pass -- the same rule verify_oq_sanitised.R now applies to its own skips.
  if (identical(ACTA_TEST_LAYOUT$kind, "version") &&
      length(list.files(ACTA_VERSION_DIR, pattern = "Titration_Instructions.*[.]xlsx$")))
    return(ACTA_VERSION_DIR)
  if (!is.null(.acta_app_dir) && dir.exists(.acta_app_dir)) return(.acta_app_dir)
  d <- file.path(tempdir(), "acta_app_stage")
  unlink(d, recursive = TRUE); dir.create(d, recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files(ACTA_VERSION_DIR, full.names = TRUE), d, recursive = TRUE)
  ## The blank template is the working workbook here. Copied OUT of Template/ and beside the app,
  ## which is precisely the "copy the blank workbook out of Template/ and rename it" step the README
  ## tells a user to do.
  tpl <- list.files(file.path(d, "Template"), pattern = "Titration_Instructions.*[.]xlsx$",
                    full.names = TRUE)
  if (length(tpl) == 1L) file.copy(tpl, file.path(d, basename(tpl)))
  ## The app discovers its OQ buttons from `Diagnostics/OQ_Test*` beside itself, so the stage needs
  ## them too or the whole Diagnostics section of the smoke test has nothing to find. SYMLINKED, not
  ## copied: the cases are ~115 MB and a test that copies them every run gets skipped for being slow.
  ## Copy is the fallback for filesystems that refuse links.
  ## An EMPTY Titration_FCS is the shipped state of a working folder (it carries only its README),
  ## so the stage has to reproduce that rather than omitting the folder -- the pre-flight distinguishes
  ## "absent" from "present but empty" and the app's Files check reports them differently.
  dir.create(file.path(d, "Titration_FCS"), showWarnings = FALSE)
  dg <- file.path(d, "Diagnostics")
  if (!isTRUE(suppressWarnings(file.symlink(ACTA_CASE_ROOT, dg)))) {
    dir.create(dg, showWarnings = FALSE)
    file.copy(list.files(ACTA_CASE_ROOT, full.names = TRUE), dg, recursive = TRUE)
  }
  .acta_app_dir <<- normalizePath(d)
  .acta_app_dir
}

## The helper library in its own environment, which is how every test gets at the functions under
## test without running the pipeline.
## Same two-source rule as the pipeline script: a sibling ACTA_Functions.R in a version folder, or
## the package's own R/ sources when the tree is laid out as a package. Reading the R/ files directly
## rather than attaching the installed namespace is deliberate -- a test must exercise the sources in
## THIS checkout, not whatever version happens to be installed on the machine.
actaTestFunctions <- function(vd = actaTestVersionDir()) {
  fn <- file.path(vd, "ACTA_Functions.R")
  h  <- new.env(parent = globalenv())
  if (file.exists(fn)) {
    suppressWarnings(suppressMessages(sys.source(fn, envir = h)))
    return(h)
  }
  rdir <- file.path(ACTA_REPO_ROOT, "R")
  rfiles <- sort(list.files(rdir, pattern = "[.]R$", full.names = TRUE))
  if (!length(rfiles))
    stop(sprintf("acta_test_paths: no ACTA_Functions.R in '%s' and no R/ sources in '%s'", vd, rdir),
         call. = FALSE)
  for (f in rfiles) suppressWarnings(suppressMessages(sys.source(f, envir = h)))
  h
}
