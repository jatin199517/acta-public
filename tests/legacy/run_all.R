## ---------------------------------------------------------------------------------------------
## Run every fast gate in this folder and print one line per script.
##
##     Rscript 3_0/tests/run_all.R            # this version, from anywhere
##     Rscript 3_0/tests/run_all.R 2_87       # another version's copies of these scripts
##
## Each script runs in its OWN Rscript process, deliberately: they load the same package stack and
## source the same helper library, and a fatal error in one must not take the rest of the suite with
## it. The exit status is 0 only if every script exited 0, so this is usable as a pre-commit gate.
##
## NOT included, because both need a full pipeline run and minutes rather than seconds:
##   * the OQ cases        -- actaOQRun("<version>/Diagnostics/OQ_Test1"), and OQ_Test2
##   * the regression gate -- regression_capture.R / regression_compare.R (see README.md)
##   * the spaced-path render -- ci_report_render.R, which runs a case and renders its report
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
             "test_gate_mindensity_costain.R", # the Costain floor is a floor, not the gate
             "test_gate_costain.R",   # ...except here, where the floor IS the gate
             "test_gate_range_floor.R", # gate_range pinned to the Costain floor, and its default
             "test_preflight.R",      # instructions-check records, incl. the real workbooks
             "test_qc_rollup.R",      # SKIPPED still fails the run; N/A is excluded from it
             "test_miflowcyt.R",      # the metadata assembler and its export appendix
             "test_equipment_override.R", # $CYT -> model, and the Info Equipment_Make/Model override
             "test_compensation.R",   # compensation pre-flight and csv staging
             "test_app_support.R",    # app-only helpers, incl. the LaTeX package check
             "test_diagnostics.R",    # the three write points and the diagnostic bundle
             "test_combinatorial.R", # the Combinatorial_group declaration and its per-value test
             "test_arg_safety.R",     # no workbook cell is ever evaluated
             ## REGISTERED WITH THE FIX, not after it. The row-F defect reached a published bundle
             ## because the pData repair allow-listed the one mangling somebody had already met;
             ## this drives the whole table through a real GatingSet instead.
             "test_pdata_restore.R",  # every pData string GatingSet() rewrites comes back
             ## ONE CORPUS, EVERY BOUNDARY. You cannot enumerate the defects of the row-F class --
             ## nobody enumerated row F -- but you can enumerate the places a string changes
             ## representation, and push the same 22 hazards through all of them.
             "test_boundary_probes.R", # openxlsx, jsonlite, FCS filenames, the XML scrub
             "test_plate_map.R",      # the Layout sheet drawn as a plate, incl. the reagent axis
             ## MEASURES the facet strip against the panel it has to fit in, at the real device
             ## size -- and pins the export invariant, that `label` is NOT rewritten to suit a plot.
             "test_strip_label.R",    # the strip label, and SI_stats!label staying byte-identical
             "test_which_plate.R",    # the plate token is a DIRECTORY, not a substring of a date
             "test_panel_inventory_source.R", # a caption names the inventory it actually opened
             ## THE APP'S INVENTORY PICKER (3.6.14). The override is the only input that outranks
             ## the workbook, so its fixture carries ALL FOUR sources at once -- a fixture with one
             ## source could not tell "the override won" from "there was nothing to win against".
             ## Holds the rule that a CHOSEN path which is wrong fails to NOTHING rather than
             ## falling back, and that no pre-flight record here may ever block the Run button.
             "test_panel_inventory_app.R",
             "test_scrub_iteration.R", # scrub_confidential removes panel names, keeps composition
             "test_sample_status.R",  # Sample_Status: the column, and the menu it publishes
             "test_info_report.R",    # the Info table and its Source column
             "test_bundle_scan.R",    # the leak scan that GATES a bundle build
             ## REGISTERED THE DAY IT WAS WRITTEN -- see the note under test_titerpubs_tissue.R.
             ## Row order IS the defect here, so the fixture puts the catalogue-less row first and
             ## carries a control proving it still triggers the old bare-logical index.
             "test_inv_by_catalog.R",
             ## THE BUNDLE'S OWN INVENTORY. 0 of 15 titrated reagents appeared in the inventory
             ## their bundle shipped, so a re-run lost Clonality, Isotype and host species on
             ## every deposited dataset. Holds the trap too: the titrated row must carry NO panel
             ## iteration, or a re-run reads it as its own co-stain and the self-costain gate
             ## refuses ACTA's own artefact.
             "test_bundle_inventory.R", # a blank catalogue must not return a phantom all-NA row
             ## REGISTERED THE DAY IT WAS WRITTEN. The written inventory must be able to STAND IN
             ## for the one it was read from: seven columns were emitted always-empty (six of them
             ## one lab's own fields), every column outside ACTA_PI_COLS was discarded on read, and
             ## Fluorochrome_Alias was read to match reagents and never written back. Section 2.2
             ## is the one that must not be relaxed -- there is NO deny-list of column names in
             ## ACTA, so a lab keeping a column the template no longer offers gets it back.
             "test_inventory_carry.R",
             ## REGISTERED THE DAY IT WAS WRITTEN. The shipped fixtures contain NOTHING for this
             ## gate to fire on -- 15 reagents, 0 hits -- so its fixtures are all constructed and
             ## two of its sections are CONTROLS that fail if they stop being adversarial.
             "test_self_costain.R",   # the titrated reagent is not one of its own co-stains
             ## THE ONE REGEX THE WHOLE SELF-DOCUMENTING-HEADER SCHEME RESTS ON. It had been
             ## retyped at five sites and two disagreed; no behavioural test could see it, because
             ## no shipped header has a mid-string bracket yet. Pinned at SOURCE level.
             "test_workbook_conventions.R", # annotations, `#` comment rows, and one regex one place
             ## REGISTERED THE DAY IT WAS WRITTEN, and it found two things nothing else could see:
             ## the app reading `Info!ELN_ID`, a row retired in 3.6.8, so every app-driven copy
             ## archived into `<stamp>_run`; and `scrub_eln_placeholder`, read and validated and in
             ## no template. Both are the house failure: a reader that cannot read returns a
             ## DEFAULT, and four separate readers consume this workbook.
             "test_column_registry.R", # every workbook column, declared once and held to it
             ## THE FAILURE-MODE LAYER for the convention work -- the paths the change CREATED, as
             ## opposed to the contract it promises. It caught two defects the contract tests could
             ## not: `as.character(100000)` is "1e+05" on its way to becoming a folder name, and the
             ## app's infoVal() reading a LONG Info sheet as empty since 3.6.0.
             "test_convention_edge_cases.R", # zero-length, NA, absent, duplicated, wrong-typed
             ## THE ONE CONTROL SCRUBBING CANNOT SUBSTITUTE FOR, and it had no test until 3.6.11:
             ## a construct detection reagent is disclosed by the PANEL, not by any label.
             "test_blocked_markers.R", # acta.bundle_blocked_markers, and its four fail-opens
             "test_titerpubs_scrub.R", # the de-identifiers themselves: NO fixture ran this path
             ## REGISTERED THE DAY IT WAS WRITTEN, and that is the point. test_titerpubs_tissue.R
             ## passed when invoked directly and was INVISIBLE here, because this list is explicit
             ## and nobody added to it -- a gate that cannot run in CI is a gate that does not
             ## exist. Same shape as the index audit that found five defects in unchanged code.
             "test_titerpubs_tissue.R", # Info!Tissue_Type/Tissue_Fallback -> the Tissue column
             ## NO TEST AT ALL until 2026-11-20, which is why a wrong parent population shipped in
             ## 8 published templates across two datasets. The Metrdy maintainers found it on the
             ## page; nothing here could, because nothing exercised the function.
             "test_titerpubs_gating.R", # the parent population a reagent was measured inside
             "test_schema_368.R",       # the 3.6.8 rename, version gate and site-rule firing
             ## PINS stats_cols BESIDE the Layout_Plate column count. 3.6.8 shipped an OQ that
             ## reported FAIL on a correct analysis because no gate here could see expected.json;
             ## the real comparison only runs in CI on a schedule. This is the cheap tripwire.
             "test_oq_schema_pin.R",    # Layout_Plate <-> expected.json coupling
             "test_workbook_package.R", # every .xlsx is a well-formed OPC package (Excel is strict)
             "test_dashboard_runs.R", # which folders count as runs -- the same export pooled twice
             "smoke_app.R", "test_app_package.R",           # the Shiny server logic, via testServer
             "test_install_path.R",   # what an install-only user can reach, incl. biocViews
             ## THE APP ASKS; test_install_path.R only says what is ON OFFER. 3.6.12 called
             ## actaInfoFlagTrue() from the app without exporting it, and the app died with
             ## "could not find function" on the first workbook setting titerpubs_export -- i.e.
             ## every real run that builds a bundle. Found by the user on Windows, not here.
             "test_app_exports.R",    # every bare-name acta* call in the app is exported
             ## THE TWO UNSUPPORTED ENTRY POINTS THAT LOOK SUPPORTED. A user running published
             ## 3.6.12 from a downloaded clone could open neither file: the app died on shiny's
             ## internal `container` (a shadowed bare `pre`), the script on an unexported internal
             ## after announcing that the helpers had loaded. Every other gate drives ACTA the
             ## supported way, so none of them could see either one.
             "test_entry_points.R",   # app survives a shadowed tag fn; script refuses legibly
             "test_readme_calls.R",   # the README's own R examples resolve against this layout
             "test_man_pages.R",      # every export has a man/ topic that still matches its formals
             "test_release_pointer.R", # the published-version pointer still matches the tags
             ## THE PRIVATE WORKFLOW IS NOW A GATE ON PUBLICATION (flow agreed 2026-11-20), and it
             ## is a SEPARATE FILE from the public one -- different layout, different cost profile.
             ## Nothing else joins them, so a package added to one list and not the other means the
             ## private gate goes green and the public one goes red AFTER you have published.
             "test_ci_workflows.R",   # the two CI workflows have not drifted apart
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
  ## THREE STATES, NOT TWO. A script that skips its whole body exits 0, so an exit-status-only
  ## verdict printed PASS for a file that asserted nothing. test_app_package.R does exactly that
  ## when the checkout is not installed, and its own SKIPPED banner went into a log this runner
  ## only prints for FAILURES. The banner stopped lying; this had not.
  res$status[i]  <- if (!identical(as.integer(code), 0L)) "FAIL"
                    ## "TESTS SKIPPED" is the WHOLE-FILE banner. A bare "SKIPPED" is not enough:
                    ## test_app_support.R prints a SECTION skip ("SKIPPED -- no run snapshot
                    ## available") and would then be reported as if it had asserted nothing.
                    else if (any(grepl("TESTS SKIPPED", readLines(lg, warn = FALSE)))) "SKIP"
                    else "PASS"
  cat(sprintf("  %-6s %-24s %5.1f s\n", res$status[i], SCRIPTS[i], res$seconds[i]))
}

bad  <- res$status %in% c("FAIL", "MISSING")
skip <- res$status == "SKIP"
cat(sprintf("%s\n%d of %d passed%s in %.0f s\n", strrep("-", 62),
            sum(res$status == "PASS"), nrow(res),
            if (any(skip)) sprintf(", %d SKIPPED (%s)", sum(skip),
                                   paste(res$script[skip], collapse = ", ")) else "",
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
