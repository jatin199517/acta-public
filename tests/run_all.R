## ---------------------------------------------------------------------------------------------
## Run every fast gate in this folder and print one line per script.
##
##     Rscript 2_99/tests/run_all.R            # this version, from anywhere
##     Rscript 2_99/tests/run_all.R 2_87       # another version's copies of these scripts
##
## Each script runs in its OWN Rscript process, deliberately: they load the same package stack and
## source the same helper library, and a fatal error in one must not take the rest of the suite with
## it. The exit status is 0 only if every script exited 0, so this is usable as a pre-commit gate.
##
## NOT included, because both need a full pipeline run and minutes rather than seconds:
##   * the OQ cases        -- actaOQRun("<version>/Diagnostics/OQ_Test1"), and OQ_Test2
##   * the regression gate -- regression_capture.R / regression_compare.R (see README.md)
## ---------------------------------------------------------------------------------------------
source(file.path(this.path::this.dir(), "acta_test_paths.R"))

vd <- actaTestVersionDir()
## The test folder is THIS script's own folder, not `<code>/tests`. Those coincide in the
## version-folder repo but not in a package, where the code is inst/pipeline/ and the scripts live in
## tests/legacy/ -- deriving the folder from the code dir there looked for inst/pipeline/tests and
## tripped the "predates the contract" guard below on a perfectly healthy checkout.
## An explicit version ARGUMENT still means "that version's own tests", which is the whole point of
## being able to run `run_all.R 2_87`.
argv <- commandArgs(trailingOnly = TRUE)
td <- if (length(argv) && nzchar(argv[[1]])) file.path(vd, "tests") else ACTA_TEST_DIR

## REFUSE a version that predates the shared cwd contract, rather than reporting nonsense about it.
## Pointed at 2_87 this printed "2 of 8 passed" -- and those two passes were WORTHLESS: 2_87's copies
## of the scripts default to the literal string "2_88_WIP", so they resolved against the current
## directory and tested THIS version's code while the runner's header said 2_87. Six failed for the
## unrelated reason that they expect to be launched from the repo root. Earlier versions are frozen,
## so the fix is to say so, not to modify them.
if (!file.exists(file.path(td, "acta_test_paths.R")))
  stop(sprintf(paste("%s/tests predates the shared working-directory contract (no acta_test_paths.R).",
                     "Its scripts must be run one at a time from the repo root, and check what",
                     "version each one actually resolves -- several of them name 2_88_WIP."),
               basename(vd)), call. = FALSE)

## Order: cheapest and most fundamental first, so a broken helper library is reported before the
## slower gates spend a minute rediscovering it.
SCRIPTS <- c("test_marker_nodes.R",   # marker +/- node resolution from the template row
             "test_quad_pops.R",      # openCyto pop= forms, pinned against .preprocess_row()
             "test_preflight.R",      # instructions-check records, incl. the real workbooks
             "test_miflowcyt.R",      # the metadata assembler and its export appendix
             "test_compensation.R",   # compensation pre-flight and csv staging
             "test_app_support.R",    # app-only helpers, incl. the LaTeX package check
             "test_diagnostics.R",    # the three write points and the diagnostic bundle
             "test_combinatorial.R", # the Combinatorial_group declaration and its per-value test
             "test_arg_safety.R",     # no workbook cell is ever evaluated (2_94 review H-1)
             "test_plate_map.R",      # the Layout sheet drawn as a plate, incl. the reagent axis
             "test_workbook_package.R", # every .xlsx is a well-formed OPC package (Excel is strict)
             "smoke_app.R",           # the Shiny server logic, via testServer
             "verify_oq_sanitised.R") # the OQ cases carry nothing that cannot be published

cat(sprintf("ACTA test suite -- %s\n%s\n", basename(vd), strrep("=", 62)))
res <- data.frame(script = SCRIPTS, status = NA_character_, seconds = NA_real_,
                  stringsAsFactors = FALSE)
logs <- list()
for (i in seq_along(SCRIPTS)) {
  f <- file.path(td, SCRIPTS[i])
  if (!file.exists(f)) { res$status[i] <- "MISSING"; next }
  t0 <- proc.time()[["elapsed"]]
  ## Arguments are NOT forwarded: every script defaults to the version folder it lives in, which is
  ## already the one asked for, and verify_oq_sanitised.R would read an argument as a CASE folder.
  ## CAPTURED, not discarded. This used to be stdout=FALSE/stderr=FALSE with a closing hint to
  ## "rerun the failure on its own to see its output" -- advice nobody can take on a CI runner,
  ## which is exactly where it matters. The public badge stayed red across three releases while the
  ## log said only "FAIL test_app_support.R" and threw away the assertion that actually failed.
  ## A gate whose failure cannot be READ is only half a gate.
  lg <- tempfile(fileext = ".log")
  code <- system2("Rscript", shQuote(f), stdout = lg, stderr = lg)
  logs[[SCRIPTS[i]]] <- lg
  res$seconds[i] <- proc.time()[["elapsed"]] - t0
  res$status[i]  <- if (identical(as.integer(code), 0L)) "PASS" else "FAIL"
  cat(sprintf("  %-6s %-24s %5.1f s\n", res$status[i], SCRIPTS[i], res$seconds[i]))
}

bad <- res$status != "PASS"
cat(sprintf("%s\n%d of %d passed in %.0f s\n", strrep("-", 62), sum(!bad), nrow(res),
            sum(res$seconds, na.rm = TRUE)))
if (any(bad)) {
  for (s in res$script[bad]) {
    cat(sprintf("\n%s\n--- captured output of %s ---\n", strrep("=", 62), s))
    lg <- logs[[s]]
    if (!is.null(lg) && file.exists(lg))
      cat(paste(readLines(lg, warn = FALSE), collapse = "\n"), "\n")
    else cat("  (no output captured)\n")
    cat(sprintf("--- end %s ---\n", s))
  }
  cat("\nTo reproduce locally:\n")
  for (s in res$script[bad]) cat(sprintf("  Rscript %s\n", file.path(td, s)))
}
quit(status = if (any(bad)) 1L else 0L)
