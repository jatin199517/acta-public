## THE SHINY APP, DRIVEN AGAINST THE INSTALLED PACKAGE.
##
## smoke_app.R exercises the app in the SOURCE tree, where the helpers were source()d into the
## global environment and every one of the 203 is visible. That says nothing about the package
## path, where the app does library(ACTA) and can only see the 29 exported names. The two differ by
## 174 symbols, so "smoke_app passes" was never evidence that the installed app works.
##
## It also guards the layout. The app sets CODE_DIR <- APP_DIR and expects the pipeline scripts and
## Template/ in that one folder, so it has to ship in inst/pipeline/ beside them -- it was briefly
## put in inst/app/, where CODE_DIR would have found neither and every version-file check would
## have failed. Nothing else would have caught that before a user did.
##
## NOTE the app still conflates code_dir and version_dir, so it cannot be run IN PLACE from a
## package: inst/pipeline/ is code-only and holds no user workbook. That is a known open design
## item (the app needs an override in the shape of ACTA_LAYOUT_DIR). This test therefore stages a
## working directory, which is exactly what the README tells a user to assemble after installing.
suppressMessages(library(shiny))
source(file.path(this.path::this.dir(), "acta_test_paths.R"))
ok <- TRUE; skipped <- NA_character_
chk <- function(w, c) { if (!c) ok <<- FALSE; cat(sprintf("  [%s] %s\n", if (c) "ok" else "FAIL", w)) }

## The subject must be THIS checkout's package, not whatever ACTA happens to be installed -- the
## machine that wrote this had a stale 2.90.0 sitting in the system library.
## unname(): read.dcf() returns a NAMED character, and identical() compares attributes -- so the
## first version of this compared "3.0.0" to c(Version = "3.0.0"), got FALSE, and SKIPPED while
## printing "ACTA 3.0.0 is not installed (found: 3.0.0)". A gate that skips itself is the exact
## failure this suite has been chasing all week; it deserved to be caught by reading its output.
want <- tryCatch(unname(read.dcf(file.path(ACTA_REPO_ROOT, "DESCRIPTION"))[1, "Version"]),
                 error = function(e) NA_character_)
have <- tryCatch(as.character(utils::packageVersion("ACTA")), error = function(e) NA_character_)
if (is.na(want)) {
  skipped <- "no DESCRIPTION here, so there is no package layout to test"
  cat("  [skip]", skipped, "\n")
} else if (is.na(have) || !identical(have, want)) {
  skipped <- sprintf("ACTA %s is not installed (found: %s)", want,
                     if (is.na(have)) "none" else have)
  cat(sprintf(paste0("  [skip] %s.\n",
                     "         Install this checkout first:  R CMD INSTALL %s\n"),
              skipped, ACTA_REPO_ROOT))
} else {
  suppressMessages(library(ACTA))
  pipe <- system.file("pipeline", package = "ACTA")
  ## ACTA MUST LOAD ON A HEADLESS MACHINE. flowMeans depends on tcltk, which needs XQuartz on
  ## macOS, so an importFrom(flowMeans, ...) made `library(ACTA)` itself fail where there is no
  ## X11 -- and took `R CMD INSTALL` down with it, since its last step is to load the package.
  ## ACTA 3.0 shipped unloadable on any Mac without XQuartz because of it. flowMeans is called as
  ## flowMeans::flowMeans() instead, so you need XQuartz to START A RUN, not to attach the
  ## library. Asserted in a FRESH process: the parent has already loaded everything, so checking
  ## loadedNamespaces() in here would always pass.
  ## --no-save --no-restore, NOT --vanilla. --vanilla also implies --no-init-file, which drops
  ## any library path added by a .Rprofile -- so under renv the parent finds ACTA and the child
  ## cannot, and both assertions below fail on a healthy tree. The parent's .libPaths() is
  ## passed in too, so the child looks exactly where the parent does.
  ## stderr is CAPTURED: without it a failure printed no reason, the same unreadable-gate
  ## problem run_all.R was already fixed for.
  ## A SENTINEL LINE, because stdout and stderr are merged here. library(ACTA) writes warnings to
  ## stderr and renv writes a banner there, and the previous parse did paste(.probe, collapse = "")
  ## -- no separator -- so the answer glued itself onto the last warning line and the exact-match
  ## intersect() then DISCARDED the glued token. One eagerly-loaded package read as none, silently,
  ## in the one gate guarding the regression that shipped 3.0 unloadable. Marking the answer makes
  ## the parse independent of whatever else the child prints.
  ## Requiring the sentinel also detects a dead child better than the exit status alone: no
  ## sentinel means no answer, whatever the status says.
  ## deparse() for the paths rather than sprintf: a library path containing a quote would otherwise
  ## close the string literal and inject R into the child.
  .code <- paste0(
    sprintf(".libPaths(%s); ", paste(deparse(.libPaths()), collapse = "")),
    'suppressMessages(library(ACTA)); ',
    'cat(paste0("\nACTA_PULLED:", paste(intersect(loadedNamespaces(),',
    ' c("tcltk","flowMeans")), collapse=","), "\n"))')
  .probe <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
    c("--no-save", "--no-restore", "-e", shQuote(.code)), stdout = TRUE, stderr = TRUE))
  .mark <- grep("^ACTA_PULLED:", .probe, value = TRUE)
  .st   <- attr(.probe, "status")
  .ran  <- (is.null(.st) || identical(as.integer(.st), 0L)) && length(.mark) == 1L
  .pulled <- if (length(.mark) == 1L)
    Filter(nzchar, strsplit(trimws(sub("^ACTA_PULLED:", "", .mark[[1]])), ",")[[1]]) else character(0)
  chk("the headless-load probe actually ran", .ran)
  chk("library(ACTA) pulls in neither tcltk nor flowMeans (headless-loadable)",
      .ran && length(.pulled) == 0L)
  ## Printed whenever EITHER assertion fails, not only when the probe did not run -- a mangled
  ## answer arrives on a child that exited 0, which is exactly how the defect above hid.
  if (!.ran || length(.pulled))
    cat("          probe output:", paste(utils::head(.probe, 8), collapse = " | "), "\n")

  chk("the app ships beside the pipeline, not in a folder of its own",
      length(list.files(pipe, pattern = "^ACTA_App[.]R$")) == 1L)
  chk("inst/pipeline carries NO sibling ACTA_Functions.R, so the namespace is used",
      length(list.files(pipe, pattern = "^ACTA_Function.*[.]R$")) == 0L)

  d <- file.path(tempdir(), "pkg_app_stage"); unlink(d, recursive = TRUE); dir.create(d)
  file.copy(list.files(pipe, full.names = TRUE), d, recursive = TRUE)
  tpl <- list.files(file.path(d, "Template"), pattern = "Titration_Instructions.*xlsx$",
                    full.names = TRUE)
  if (length(tpl) == 1L) file.copy(tpl, file.path(d, basename(tpl)))
  dir.create(file.path(d, "Titration_FCS"), showWarnings = FALSE)
  cas <- system.file("extdata", "oq_small", package = "ACTA")
  if (!isTRUE(suppressWarnings(file.symlink(cas, file.path(d, "Diagnostics")))))
    file.copy(cas, file.path(d, "Diagnostics"), recursive = TRUE)

  owd <- setwd(d); on.exit(setwd(owd))
  testServer(shinyAppFile("ACTA_App.R"), {
    vf  <- actaVersionFiles(getwd())
    st  <- setNames(vapply(vf, function(r) r$status, ""), vapply(vf, function(r) r$id, ""))
    for (r in vf)
      cat(sprintf("      %-10s %-5s %s\n", r$id, r$status,
                  if (is.na(r$detail)) "" else substr(r$detail, 1, 46)))
    chk("the four shipped files resolve from the package",
        all(st[c("script", "functions", "dashboard", "report")] == "pass"))
    ## The point of the whole file: the helpers arrived through the NAMESPACE.
    chk("helpers came from the installed namespace, not a sibling file",
        grepl("package", Filter(function(r) r$id == "functions", vf)[[1]]$detail, fixed = TRUE))
    chk("the staged workbook resolves", identical(unname(st[["layout"]]), "pass"))
    chk("the diagnostic cases are discoverable beside the app",
        length(list.files("Diagnostics", pattern = "^OQ_Test")) == 3L)
    chk("dependency panel renders", nzchar(paste(as.character(actaDependencyChecks(getwd())),
                                                 collapse = "")))
  })
}
## A SKIP IS NOT A PASS. Every assertion here sits behind "is this checkout installed?", and
## bumping DESCRIPTION guarantees that is false until someone reinstalls -- so the whole file
## skipped and still printed TESTS PASS. That is how the v3.0 headless-load regression could have
## returned unnoticed. Exit stays 0, because not being installed is not a failure and CI installs
## first; the BANNER is what had to stop lying.
if (!is.na(skipped)) cat(sprintf("\nAPP-FROM-PACKAGE TESTS SKIPPED -- %s\n", skipped))
cat(if (!ok) "\nFAILURES ABOVE\n" else if (is.na(skipped)) "\nAPP-FROM-PACKAGE TESTS PASS\n" else "")
quit(status = if (ok) 0 else 1)
