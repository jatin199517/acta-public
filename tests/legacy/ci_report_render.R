## THE REPORT RENDERS FROM A PATH THAT HAS A SPACE IN IT.
##
##     Rscript tests/legacy/ci_report_render.R [OQ_TestN]
##
## WHY THIS IS A SEPARATE FILE AND NOT A `run:` BLOCK IN THE WORKFLOW. The open item this closes
## reads "No CI job renders a report on Windows -- windows-latest runs run_all.R, which excludes
## the OQ cases", and it carries its own warning: "an untestable new workflow job is how v3.1.0's
## CI went red on a tag". A script in the repository can be run on a laptop before it is ever
## pushed; twenty lines of R inlined in YAML cannot. The workflow calls this and does nothing else.
##
## WHY A SPACED PATH IS THE WHOLE POINT, and why a naive Windows job would have been GREEN AND
## INERT. actaRenderStage() returns its argument UNCHANGED unless two things are both true: the
## platform is Windows, and the path contains a space. A GitHub runner checks out to
## D:\a\<repo>\<repo> -- no space anywhere -- so a Windows OQ job run in the workspace would never
## reach the staging branch at all, would pass, and would be cited as evidence for a mechanism it
## had not executed. This stages the case under a directory named with a space on purpose, and
## ASSERTS the space is there before running, so the run cannot be inert without saying so.
##
## WHY RUNNER_TEMP RATHER THAN tempdir() ON CI. The OQ's last check is that no output names the
## operator or the machine, and it looks for absolute home paths. tempdir() on the Windows runner
## is under C:\Users\runneradmin\AppData\Local\Temp, so staging there would make that check FAIL
## on a correct render -- a red job for a reason that is not the thing under test, which is the
## same fault as the 0555 fixture. RUNNER_TEMP is D:\a\_temp and holds no home path.
##
## WHAT IT ASSERTS, in order of what a failure would mean:
##   1. the staged run folder really does contain a space              (else the run is inert)
##   2. actaRenderStage() with the Windows branch FORCED relocates it  (the mechanism, everywhere)
##   3. actaRenderStage() on THIS platform -- reported, and asserted only on Windows
##   4. off Windows, that the real pandoc_path_arg leaves a spaced path alone -- and on Windows,
##      a REPORTED verdict on whether the fault is live here at all   (what makes a pass mean something)
##   5. the case runs and its "Report rendered" record passes          (the actual claim)
##   6. no record failed                                              (same policy as the oq job)
## 3 is what the release manifest asks a human to run by hand on the affected machine. Printing it
## here puts the answer in a log instead of in somebody's terminal.
source(file.path(this.path::this.dir(), "acta_test_paths.R"))

## THE ARGUMENT IS A CASE NAME, NOT A VERSION FOLDER, so the helpers are resolved from
## ACTA_VERSION_DIR directly rather than through actaTestVersionDir() -- that reads argv[[1]] and
## stopped with "version folder 'OQ_Test1' does not exist". Same trap acta_test_paths.R already
## documents for verify_oq_sanitised.R, which takes case folders for the same reason.
argv <- commandArgs(trailingOnly = TRUE)
case <- if (length(argv) && nzchar(argv[[1]])) argv[[1]] else "OQ_Test1"

ok <- TRUE
chk  <- function(what, cond) { if (!isTRUE(cond)) ok <<- FALSE
                               cat(sprintf("  [%s] %s\n", if (isTRUE(cond)) "ok" else "FAIL", what)) }
note <- function(what) cat(sprintf("  [--] %s\n", what))

src <- file.path(actaTestCaseRoot(), case)
if (!dir.exists(src)) {
  cat("\nREPORT RENDER TESTS SKIPPED -- no case '", case, "' under ", actaTestCaseRoot(), "\n", sep = "")
  quit(status = 0L)
}

h <- actaTestFunctions(ACTA_VERSION_DIR)
cat("platform :", .Platform$OS.type, "/", Sys.info()[["sysname"]], "\n")
cat("case     :", src, "\n")
cat("code     :", ACTA_VERSION_DIR, "\n")

## --- stage the case under a directory whose name contains a space -----------------------------
## A LITERAL SPACE IN A NAME WE CHOOSE, not a space we hope the machine supplies. The fault this
## exercises came from a real account name with a space in it; reproducing it by finding such a
## path would make the test depend on the machine it runs on.
parent <- Sys.getenv("RUNNER_TEMP", unset = NA_character_)
if (is.na(parent) || !nzchar(parent) || !dir.exists(parent)) parent <- tempdir()
stage <- file.path(parent, "acta ci report")
unlink(stage, recursive = TRUE)
dir.create(stage, recursive = TRUE, showWarnings = FALSE)
if (!isTRUE(all(file.copy(src, stage, recursive = TRUE))))
  stop(sprintf("could not copy '%s' into '%s'", src, stage), call. = FALSE)
run <- normalizePath(file.path(stage, basename(src)), winslash = "/", mustWork = TRUE)

## The baseline goes beside the case, not inside it -- actaOQRun() looks for it in dirname(oq_dir)
## and WARNS when it is absent, recording the package-version check as skip. Copying it keeps the
## staged run as close to the shipped one as the runner allows.
bl <- file.path(dirname(src), "validated_packages.tsv")
if (file.exists(bl)) invisible(file.copy(bl, file.path(stage, basename(bl)), overwrite = TRUE))

cat("staged   :", run, "\n\n")

cat("=== the precondition: is this run folder actually spaced? ===\n")
chk("the staged run folder contains a space", grepl(" ", run, fixed = TRUE))
if (!ok) {
  cat("\nWithout a space actaRenderStage() is a no-op and this whole file proves nothing.\n")
  quit(status = 1L)
}

## --- actaRenderStage(), forced and native -----------------------------------------------------
cat("\n=== actaRenderStage() ===\n")
forced <- h$actaRenderStage(run, windows = TRUE, tag = "ci")
cat("  windows = TRUE  ->", forced, "\n")
chk("with the Windows branch forced, the render is relocated off the spaced path",
    !identical(forced, run) && !grepl(" ", forced, fixed = TRUE))
if (!identical(forced, run)) unlink(forced, recursive = TRUE)

native <- h$actaRenderStage(run, tag = "ci")
cat("  this platform   ->", native, "\n")
if (identical(.Platform$OS.type, "windows")) {
  ## THE ANSWER THE RELEASE MANIFEST ASKS A HUMAN FOR. A DIFFERENT path means the diagnosis holds.
  chk("on Windows the native call relocates it too", !identical(native, run))
  if (!identical(native, run)) unlink(native, recursive = TRUE)
} else {
  note(sprintf("not Windows, so the native call correctly returns the path unchanged (%s)",
               identical(native, run)))
  chk("off Windows the native call is a no-op", identical(native, run))
}

## --- is the fault actually live on THIS machine? ----------------------------------------------
## THE DIFFERENCE BETWEEN "NOTHING BROKE" AND "THE FIX FIXED SOMETHING". A green render here is
## consistent with two very different worlds: the staging avoided a real fault, or there was
## nothing to avoid. Without telling them apart, a Windows pass cannot close the open item -- it
## only fails to contradict it.
##
## So ask rmarkdown directly, with the REAL function and no stub, about the figure directory this
## run would use. On Windows a spaced path comes back from pandoc_path_arg() shortened and
## BACKSLASHED even with backslash = FALSE, and those backslashes are what land inside
## \includegraphics{} as undefined control sequences. Milliseconds, no second render.
##
## REPORTED, NOT ASSERTED, and deliberately. If the fault is NOT live on a Windows runner the
## right conclusion is that upstream fixed it and actaRenderStage() may be retirable -- which is
## news, not a failure, and must not turn a correct render red. The gating version of this
## mechanism lives in test_diagnostics.R ("THE PREMISE"), which runs on every platform because it
## drives upstream's AST rather than the Windows API.
cat("\n=== is the fault live here? (rmarkdown::pandoc_path_arg, no stub) ===\n")
.fig <- file.path(run, "ACTA_Report_3.1_files", "figure-latex")
.raw <- rmarkdown::pandoc_path_arg(.fig, backslash = FALSE)
cat("  run folder's figure dir ->", .raw, "\n")
if (identical(.Platform$OS.type, "windows")) {
  .live <- grepl("\\\\", .raw, fixed = TRUE)
  .st   <- h$actaRenderStage(run, tag = "probe")
  .sfig <- rmarkdown::pandoc_path_arg(file.path(.st, "ACTA_Report_3.1_files", "figure-latex"),
                                      backslash = FALSE)
  cat("  the stage's figure dir  ->", .sfig, "\n")
  if (!identical(.st, run)) unlink(.st, recursive = TRUE)
  if (.live && !grepl("\\\\", .sfig, fixed = TRUE)) {
    note("VERDICT: the fault IS live on this machine and the stage avoids it -- the fix is doing work")
  } else if (.live) {
    note("VERDICT: the fault is live AND the stage did not avoid it -- the fix is NOT working here")
    ok <- FALSE
  } else {
    note(paste("VERDICT: the fault is NOT reproducible on this machine. Either rmarkdown fixed it",
               "or the mechanism changed -- read test_diagnostics.R's PREMISE section, confirm,",
               "and consider retiring actaRenderStage(). NOT a failure."))
  }
} else {
  chk(sprintf("off Windows the spaced figure path is untouched (%s)", .Platform$OS.type),
      identical(.raw, .fig))
  note("the fault is unreachable here: pandoc_path_arg's whole block is inside is_windows()")
}

## --- the actual claim: the case runs and the report renders -----------------------------------
cat("\n=== running ", basename(run), " from the spaced path ===\n", sep = "")
t0 <- proc.time()[["elapsed"]]
res <- tryCatch(h$actaOQRun(run, quiet = TRUE, code_dir = ACTA_VERSION_DIR),
                error = function(e) e)
if (inherits(res, "error")) {
  chk(paste("the case ran:", conditionMessage(res)), FALSE)
  cat(sprintf("\n%s\nREPORT RENDER TESTS FAILED\n", strrep("-", 62)))
  quit(status = 1L)
}
cat(sprintf("  %.0f s, verdict %s\n", proc.time()[["elapsed"]] - t0, res$verdict))

st  <- vapply(res$recs, function(x) as.character(x$status), character(1))
ids <- vapply(res$recs, function(x) as.character(x$id), character(1))

rep <- which(ids == "report")
chk("the case has a 'Report rendered' record at all", length(rep) == 1L)
if (length(rep) == 1L) {
  r <- res$recs[[rep]]
  chk(sprintf("Report rendered: %s%s", r$status,
              if (!is.na(r$detail) && nzchar(r$detail)) paste0(" -- ", r$detail) else ""),
      identical(as.character(r$status), "pass"))
}

## A PDF ON DISK, not only a record saying one was made. report_ok has come apart from "a PDF is
## in this folder now" before, which is why actaRunArtefacts() checks both.
##
## LOOK IN res$out_dir AS WELL AS THE RUN ROOT. run_acta() writes the PDF beside the workbook and
## the OQ then COLLECTS it into <run>/Outputs, so a root-only glob found zero on a run whose own
## "Report rendered" record said pass -- this assertion failing while the product was correct,
## which is the fixture-not-the-code mistake the release manifest opens with. Taking the folder
## from the returned value rather than spelling "Outputs" here also means the two cannot drift.
pdfs <- list.files(unique(c(run, res$out_dir)), pattern = "^ACTA_Report.*[.]pdf$",
                   full.names = TRUE)
chk(sprintf("a rendered PDF is on disk (%d found%s)", length(pdfs),
            if (length(pdfs)) paste0(": ", paste(basename(pdfs), collapse = ", ")) else ""),
    length(pdfs) >= 1L)

## SAME POLICY AS THE `oq` JOB: warn and skip are legitimate on a runner -- a package-version
## drift note is the usual one -- and only a hard fail breaks this.
bad <- ids[st == "fail"]
chk(sprintf("no record failed (%d pass, %d warn/skip, %d fail)",
            sum(st == "pass"), sum(st %in% c("warn", "skip")), sum(st == "fail")),
    length(bad) == 0L)
if (length(bad)) cat("        failed:", paste(bad, collapse = ", "), "\n")
if (any(st %in% c("warn", "skip")))
  note(paste("warn/skip:", paste(ids[st %in% c("warn", "skip")], collapse = ", ")))

cat("\n  log:", res$log, "\n")
cat(sprintf("\n%s\nREPORT RENDER TESTS %s\n", strrep("-", 62), if (ok) "PASSED" else "FAILED"))
quit(status = if (ok) 0L else 1L)
