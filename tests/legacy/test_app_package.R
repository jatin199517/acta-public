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
## The app CAN be run in place from a package, through acta_app(), and this comment used to say
## it could not. But the mechanism is ACTA_WORK_DIR, not the getwd() fallback: shiny::runApp()
## setwd()s to the app directory BEFORE it sources the file, so by the time WORK_DIR evaluates
## getwd() the answer is already inst/pipeline/ and WORK_DIR silently equals CODE_DIR -- measured,
## not assumed. acta_app() works because it records getwd() into ACTA_WORK_DIR first. That is why
## it is exported and why the README documents it instead of a bare runApp() call.
## This test stages a working directory, because that is the arrangement a user is most likely to
## have and the one the README describes.
suppressMessages(library(shiny))
source(file.path(this.path::this.dir(), "acta_test_paths.R"))
ok <- TRUE; skipped <- NA_character_
.rqq <- function(x) encodeString(as.character(x), quote = "'")
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

  ## `library(ACTA); run_acta()` FROM A FOLDER OF DATA MUST WORK. code_dir used to default to
  ## version_dir, so the bare call looked for the pipeline among the user's FCS and stopped with
  ## "expected exactly one ACTA_Script*.R". Three people formed that expectation independently
  ## before it changed. Asserted without running an analysis: point it at an EMPTY folder and the
  ## error must be about the missing workbook, which only happens once code_dir already resolved.
  chk("run_acta() resolves code_dir rather than defaulting it to the run folder",
      is.null(formals(ACTA::run_acta)$code_dir))
  local({
    e <- file.path(tempdir(), "acta_bare_gate"); unlink(e, recursive = TRUE)
    dir.create(e, recursive = TRUE, showWarnings = FALSE)
    ## ASSERT ON THE RESOLVER'S OWN ANNOUNCEMENT, and stop the moment it arrives.
    ##
    ## Two earlier versions of this probe let run_acta() carry on past the resolver, which meant
    ## the pipeline script got SOURCED -- and its first act is its own dependency check. That made
    ## the probe assert on whatever the machine happened to be missing (v3.0.5 went red on macOS
    ## for "there is no package called 'BiocManager'"), let a gate reach the NETWORK and install
    ## into the host library, and cost 20 of this file's 20 seconds. Dropping the message-content
    ## assertion fixed the redness and left the rest.
    ##
    ## The resolver announces itself BEFORE the script is looked up or sourced, so that message is
    ## the subject -- and aborting from the handler means the pipeline never runs at all. No
    ## network, no installs, no dependence on the library's state, and the assertions are POSITIVE
    ## rather than "the error was not this one string", which passed on an empty message.
    ## tryCatch OUTSIDE withCallingHandlers, not the other way round. With the calling handler
    ## outermost, an error raised inside it looks for handlers established BEFORE that handler was
    ## registered, so it skipped the tryCatch sitting within and escaped the gate entirely.
    ## ABORT ON THE FIRST MESSAGE, whatever it says, and assert on its CONTENT afterwards.
    ## Aborting only on the expected text made the "never runs the pipeline" property contingent on
    ## the assertion passing: reword the announcement, or let the resolver take the working-folder
    ## branch, and nothing interrupts run_acta() -- it sources the pipeline exactly as the old
    ## probe did (measured: 42 sys.source calls and a download). A guarantee that holds only on the
    ## happy path is not a guarantee.
    ## Belt and braces for the duration: no repos, and a throwaway library FIRST on .libPaths() so
    ## install.packages()'s default lib is not the real one. This used to set R_LIBS_USER, which R
    ## reads only at STARTUP -- .libPaths() was unchanged and that half of the guard did nothing
    ## while the comment claimed it did. The repos half does not cover Bioconductor either:
    ## BiocManager::repositories() ignores getOption("repos"). The message handler is the control
    ## that actually holds; these two narrow the blast radius if a future edit breaks it.
    chk("the probe fixture really is empty before the call",
        length(list.files(e, all.files = TRUE, no.. = TRUE)) == 0L)
    seen <- character(0)
    .oldRepos <- getOption("repos"); .oldLibPaths <- .libPaths()
    .throwaway <- file.path(tempdir(), "acta_probe_lib")
    dir.create(.throwaway, recursive = TRUE, showWarnings = FALSE)
    options(repos = character(0)); .libPaths(c(.throwaway, .oldLibPaths))
    err <- tryCatch(
      withCallingHandlers({ ACTA::run_acta(e, report = FALSE, plots = FALSE); "" },
                          message = function(m) {
                            seen <<- c(seen, conditionMessage(m))
                            stop("ACTA_PROBE_STOP")
                          }),
      error = function(err) conditionMessage(err))
    options(repos = .oldRepos); .libPaths(.oldLibPaths)
    unlink(.throwaway, recursive = TRUE)
    chk("the probe put repos and the library path back",
        identical(getOption("repos"), .oldRepos) && identical(.libPaths(), .oldLibPaths))
    chk("a bare run_acta() announces that it resolved code_dir to the installed package",
        length(seen) >= 1L &&
          grepl("using the installed package", seen[[1]], fixed = TRUE))
    chk("...and the probe stopped there, so the pipeline never ran",
        identical(err, "ACTA_PROBE_STOP"))
    ## POSITIVE, and about the answer rather than about an absence. The version this replaces
    ## asserted that "expected exactly one ACTA_Script" did NOT appear -- but that text comes from
    ## a line the abort above makes unreachable, so it could never fail: plant two ACTA_Script
    ## files, watch the resolver genuinely give up, and it still printed ok. Fourth unfalsifiable
    ## assertion in this release cycle, and the same lesson each time -- break the thing the
    ## assertion guards and confirm it goes red BEFORE keeping it.
    chk("...and it names the installed package's pipeline folder",
        length(seen) >= 1L &&
          grepl(gsub("\\\\", "/", normalizePath(system.file("pipeline", package = "ACTA"))),
                gsub("\\\\", "/", seen[[1]]), fixed = TRUE))
    if (!identical(err, "ACTA_PROBE_STOP"))
      cat("         got:", paste(utils::head(c(seen, err), 4), collapse = " | "), "\n")
    unlink(e, recursive = TRUE)
  })
  ## Which code ran is RECORDED, not merely messaged -- an announced fallback is only auditable
  ## if the run keeps the answer. Checked against the function's own source, because the returned
  ## list is only available from a completed run and this gate must stay fast.
  ## (The first version of this assertion was `... || TRUE`, which passes unconditionally. An
  ## assertion that cannot fail is worse than none: it reports coverage it does not have.)
  .rasrc <- paste(deparse(ACTA::run_acta), collapse = " ")
  chk("the returned list constructs code_dir", grepl("code_dir *= *code_dir", .rasrc))
  chk("the returned list constructs report_file", grepl("report_file *=", .rasrc))

  ## THE NO-INSTALL CLONE PATH MUST DISPATCH S4 GENERICS. When ACTA is not installed the app
  ## sources <root>/R/*.R into the global environment, which does NOT reproduce what the namespace
  ## gives those helpers: NAMESPACE carries four wholesale import()s, and without them a bare
  ## generic whose name also exists in base resolves to base and never dispatches. colnames() is
  ## the one that bit -- colnames(flowSet) returns 0 channels instead of 13 and the run is SILENTLY
  ## wrong, not broken. Measured at 0 before the attach loop was added. Run in a child with ACTA
  ## hidden, because this process has it loaded.
  local({
    .code <- paste0(
      sprintf(".libPaths(%s); ", paste(deparse(.libPaths()), collapse = "")),
      'requireNamespace <- function(package, ...) if (identical(package, "ACTA")) FALSE else ',
      'base::requireNamespace(package, ...); ',
      sprintf('setwd(%s); Sys.setenv(ACTA_WORK_DIR = %s); ',
              .rqq(ACTA_REPO_ROOT), .rqq(ACTA_REPO_ROOT)),
      sprintf('suppressMessages(suppressWarnings(sys.source(%s, envir = globalenv()))); ',
              .rqq(actaTestAppFile())),
      'f <- list.files(file.path("inst","extdata","oq_small"), pattern="[.]fcs$", ',
      'recursive=TRUE, full.names=TRUE)[1]; ',
      'fs <- suppressWarnings(flowCore::read.flowSet(f)); ',
      'cat(paste0("\nACTA_DISPATCH:", length(colnames(fs)), ":", ',
      'length(flowCore::colnames(fs)), "\n"))')
    out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
      c("--no-save", "--no-restore", "-e", shQuote(.code)), stdout = TRUE, stderr = TRUE))
    mk <- grep("^ACTA_DISPATCH:", out, value = TRUE)
    chk("the no-install clone path could be probed", length(mk) == 1L)
    if (length(mk) == 1L) {
      n <- as.integer(strsplit(sub("^ACTA_DISPATCH:", "", mk[[1]]), ":", fixed = TRUE)[[1]])
      chk(sprintf("bare colnames(flowSet) dispatches without the namespace (%d of %d channels)",
                  n[1], n[2]),
          n[1] > 0L && identical(n[1], n[2]))
    } else cat("         probe said:", paste(utils::head(out, 6), collapse = " | "), "\n")
  })

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
