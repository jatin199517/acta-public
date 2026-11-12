## THE FOURTH OQ CASE: the same run as OQ_Test3, with scrub_confidential = TRUE.
##
##     Rscript tests/legacy/test_oq_scrub_case.R
##
## NOT IN run_all.R -- like the other OQ cases it needs a full pipeline run and minutes rather
## than seconds. Run it beside actaOQRun() on OQ_Test1/2/3, before a release.
##
## WHY IT EXISTS. All three shipped OQ cases carry scrub_confidential = FALSE, so the switch this
## lab runs with on EVERY REAL RUN was never exercised end to end by the diagnostic suite. The
## unit gates underneath it are good -- test_scrub_iteration.R, test_titerpubs_scrub.R -- but they
## call the de-identifiers directly. Nothing drove a real workbook through a real run and looked
## at the real export, which is where the integration faults have actually been: a mapping that
## honoured the switch in one reader and not another, a blanked SOP reaching a filename, a reader
## that could not read reporting clean.
##
## WHY IT IS DERIVED AND NOT A FOURTH FIXTURE ON DISK. A fourth case would be another 32 MB of FCS
## in a repo that already ships 311 MB of them, and a second numeric baseline to keep in step with
## the first. Deriving it from OQ_Test3 costs nothing and buys a STRONGER assertion than a separate
## baseline could: the scrubbed run must produce the SAME MEASURED VALUES as the unscrubbed one.
## That is the property the lab has been relying on across 3.6.9 / 3.6.10 / 3.6.11 without anything
## checking it -- scrubbing rewrites labels, and a label that reaches a gate or a join key would
## change a number. Here it cannot do so unnoticed.
##
## THE CASE IS COPIED TO A TEMP DIRECTORY and the shipped one is never written to. OQ_Test3's own
## expected.json travels with the copy, so this runs the ordinary OQ verdict as well.

ok <- TRUE
chk <- function(label, pass, detail = NULL) {
  r <- tryCatch(list(v = isTRUE(pass), d = NULL),
                error = function(e) list(v = FALSE, d = paste("ERROR:", conditionMessage(e))))
  d <- r$d; if (is.null(d)) d <- tryCatch(paste(as.character(detail), collapse = " "),
                                          error = function(e) "")
  if (!isTRUE(r$v)) { ok <<- FALSE
    cat(sprintf("  [FAIL] %s%s\n", label, if (nzchar(d)) paste0(" -- ", d) else "")) }
  else cat(sprintf("  [ok]   %s\n", label))
}

`%||%` <- function(a, b) if (is.null(a)) b else a
source(file.path(this.path::this.dir(), "acta_test_paths.R"))
suppressMessages(library(ACTA))
suppressMessages(library(readxl))

## ---- locate the source case and the pipeline -------------------------------------------------
srcCase <- file.path(ACTA_REPO_ROOT, "ACTA_package", "inst", "extdata", "oq_small", "OQ_Test3")
if (!dir.exists(srcCase))
  srcCase <- system.file("extdata", "oq_small", "OQ_Test3", package = "ACTA")
codeDir <- file.path(ACTA_REPO_ROOT, "ACTA_package", "inst", "pipeline")
if (!dir.exists(codeDir)) codeDir <- system.file("pipeline", package = "ACTA")
if (!dir.exists(srcCase) || !dir.exists(codeDir)) {
  cat("OQ_Test3 or the pipeline could not be located; nothing to run.\n")
  cat("FAIL test_oq_scrub_case.R\n"); quit(status = 1L)
}

## OUTSIDE THE SYNCED LIBRARY, deliberately. A run inside OneDrive has had a verified workbook edit
## reverted mid-run and an output folder removed underneath it; tempdir() is local disk.
work <- file.path(tempdir(), paste0("oq4_", basename(tempfile(""))))
dir.create(work, recursive = TRUE)
case <- file.path(work, "OQ_Test4_scrub")
dir.create(case)
for (f in list.files(srcCase, all.files = TRUE, no.. = TRUE))
  file.copy(file.path(srcCase, f), case, recursive = TRUE)
## Outputs and any stale bundle travel with a plain copy; the runner clears Outputs itself, but a
## leftover .zip in the case root would be picked up as this run's bundle.
unlink(list.files(case, pattern = "[.]zip$", full.names = TRUE))

wbk <- file.path(case, "OQ_Test3_Ab_Titration_Instructions.xlsx")
chk("the derived case has its workbook", file.exists(wbk), wbk)

## ---- flip the two Info switches, AT THE XML LEVEL --------------------------------------------
## Never an openxlsx load/save round-trip: that rewrites the whole archive and has corrupted cells
## in sheets nobody touched. set_workbook_cells.R swaps text inside elements that already exist.
inf  <- as.data.frame(suppressMessages(read_excel(wbk, sheet = "Info", col_types = "text")))
vcol <- LETTERS[match("Value", names(inf))]
rows <- match(c("titerpubs_export", "scrub_confidential"), inf$Variable) + 1L   # +1: header row
chk("both Info switches were found", !anyNA(rows) && !is.na(vcol),
    paste(rows, collapse = ", "))
if (anyNA(rows) || is.na(vcol)) { cat("FAIL test_oq_scrub_case.R\n"); quit(status = 1L) }

st <- system2("Rscript",
              c(shQuote(file.path(ACTA_TEST_DIR, "set_workbook_cells.R")), shQuote(wbk), "Info",
                paste(rows, collapse = ","), shQuote(paste0(vcol, "=TRUE"))),
              stdout = TRUE, stderr = TRUE)
inf2 <- as.data.frame(suppressMessages(read_excel(wbk, sheet = "Info", col_types = "text")))
got  <- inf2$Value[match(c("titerpubs_export", "scrub_confidential"), inf2$Variable)]
chk("scrub_confidential and titerpubs_export now read TRUE", identical(got, c("TRUE", "TRUE")),
    paste(c(got, st), collapse = " | "))
if (!identical(got, c("TRUE", "TRUE"))) { cat("FAIL test_oq_scrub_case.R\n"); quit(status = 1L) }

## What the shipped workbook says, so the assertions below test a real substitution rather than a
## value that happened to equal its own placeholder.
lay <- as.data.frame(suppressMessages(read_excel(wbk, sheet = "Layout_Plate", col_types = "text")))
names(lay) <- ACTA:::actaStripHeaderAnnotation(names(lay))
realOf <- function(cn) {
  if (!cn %in% names(lay)) return(character(0))
  v <- unique(trimws(stats::na.omit(as.character(lay[[cn]])))); v[nzchar(v)]
}

## ---- run it ----------------------------------------------------------------------------------
cat("running the scrubbed case (a full pipeline run; minutes) ...\n")
t0  <- proc.time()[["elapsed"]]
res <- try(ACTA::actaOQRun(case, code_dir = codeDir, quiet = TRUE), silent = TRUE)
cat(sprintf("  ... %.0fs\n", proc.time()[["elapsed"]] - t0))
chk("the scrubbed run completed", !inherits(res, "try-error"),
    if (inherits(res, "try-error")) conditionMessage(attr(res, "condition")) else "")
if (inherits(res, "try-error")) { cat("\nFAIL test_oq_scrub_case.R\n"); quit(status = 1L) }

## actaOQRun() returns list(ok, verdict, recs, log, out_dir, elapsed_s); `recs` is a LIST of
## one-row records, not a frame.
st    <- vapply(res$recs, function(r) as.character(r$status), "")
lab   <- vapply(res$recs, function(r) as.character(r$label), "")
det   <- vapply(res$recs, function(r) if (is.na(r$detail)) "" else as.character(r$detail), "")
## ONE CHECK IS EXPECTED TO DIVERGE, AND ONLY ONE. The OQ counts the dashboard rows belonging to
## this case by matching expected.json's `eln_id` against the dashboard's -- and scrub_confidential
## REPLACES the ELN id, which is its job. So that check reads 0 rows for EXP00000003 on a run that
## correctly published EXP26000001. It is a property of deriving the case, not a defect: the OQ
## check is right, and the shipped cases it runs against have scrubbing off.
##
## Named EXACTLY, not waved through by status. The replacement assertion below is stronger than the
## one being excused: the dashboard must carry the expected number of rows under the PLACEHOLDER.
ids    <- vapply(res$recs, function(r) as.character(r$id), "")
EXPECT_DIVERGE <- "dashboard_rows"
fails  <- which(st == "fail" & !(ids %in% EXPECT_DIVERGE))
chk("the OQ verdict has no failures, with scrubbing ON (bar the scrubbed ELN id)",
    length(fails) == 0L,
    paste(utils::head(paste(lab[fails], det[fails], sep = ": "), 5), collapse = " ;; "))
chk("...and the one excused check really is the ELN-keyed dashboard count, and really did fail",
    identical(ids[st == "fail"], EXPECT_DIVERGE),
    paste(ids[st == "fail"], collapse = ", "))
cat(sprintf("  (%d pass, %d fail, %d warn/skip)\n", sum(st == "pass"), sum(st == "fail"),
            sum(st %in% c("warn", "skip"))))

outDir <- file.path(case, "Outputs")
expF   <- list.files(outDir, pattern = "TitrationExport.*[.]xlsx$", full.names = TRUE,
                     recursive = TRUE)
chk("an export was written", length(expF) == 1L, paste(basename(expF), collapse = ", "))
if (length(expF) != 1L) { cat("\nFAIL test_oq_scrub_case.R\n"); quit(status = 1L) }
ex <- as.data.frame(suppressMessages(read_excel(expF[1], sheet = "TitrationExport",
                                                col_types = "text")))

## ---- 1. the placeholders are IN, and the real values are OUT ---------------------------------
SCRUB <- ACTA:::ACTA_SCRUB_CONFIDENTIAL
for (cn in c("Operator", "ELN_ID", "ELN_Name")) {
  if (!cn %in% names(ex)) { chk(sprintf("the export has %s", cn), FALSE); next }
  v <- unique(trimws(as.character(ex[[cn]]))); v <- v[!is.na(v) & nzchar(v)]
  chk(sprintf("%s is the placeholder in the export", cn),
      length(v) == 1L && identical(v, unname(SCRUB[[cn]])), paste(v, collapse = ", "))
}
chk("SOP is BLANK in the export, not placeheld",
    !("SOP" %in% names(ex)) || all(is.na(ex$SOP) | !nzchar(trimws(ex$SOP))),
    paste(unique(ex$SOP), collapse = ", "))
chk("Costain_Panel_Iteration is a placeholder",
    !("Costain_Panel_Iteration" %in% names(ex)) ||
      all(grepl("^Iteration[0-9]+$", trimws(ex$Costain_Panel_Iteration)) |
            is.na(ex$Costain_Panel_Iteration)),
    paste(unique(ex$Costain_Panel_Iteration), collapse = ", "))

## THE HALF THAT MATTERS: the REAL value must be gone from the whole sheet, not merely absent from
## its own column. The scrub is value-keyed, and a value that also sits somewhere else survives.
## Costain_Panel_Iteration is NOT in ACTA_SCRUB_CONFIDENTIAL -- it is numbered rather than mapped
## to a constant -- so its placeholder is asked for by prefix, not by lookup.
placeholderOf <- function(cn)
  if (cn %in% names(SCRUB)) unname(SCRUB[[cn]]) else ACTA:::ACTA_SCRUB_ITERATION_PREFIX
flat <- unlist(lapply(ex, as.character))
for (cn in c("Operator", "ELN_ID", "ELN_Name", "SOP", "Costain_Panel_Iteration")) {
  real <- setdiff(realOf(cn), c("", placeholderOf(cn)))
  if (!length(real)) next
  surv <- real[vapply(real, function(r) any(grepl(r, flat, fixed = TRUE), na.rm = TRUE), TRUE)]
  chk(sprintf("no %s value from the workbook survives anywhere in the export", cn),
      length(surv) == 0L, paste(surv, collapse = ", "))
}

## ---- 2. SCRUBBING CHANGED NO MEASURED VALUE --------------------------------------------------
## The strongest assertion available here, and the one the lab has been relying on unchecked. A
## label that reached a gate, a join key or a sort order would move a number; the numbers are
## compared against the SAME case's own expected.json, which was measured with scrubbing OFF.
expJ <- ACTA:::actaOQExpected(case)
chk("the scrubbed run has the expected number of export rows",
    is.null(expJ$export_rows) || nrow(ex) == expJ$export_rows,
    sprintf("expected %s, got %d", format(expJ$export_rows), nrow(ex)))
## The titration keys are NOT confidential and must come through untouched -- if scrubbing reached
## them the export would still have the right number of rows while naming different reagents.
## expected.json's `group` IS the Alias for a combinatorial folder -- CCR7_combo, not CCR7 -- so
## these are compared as written. setequal, not %in%: a scrub that dropped a titration entirely
## would still satisfy a subset test.
if (!is.null(expJ$group) && "Alias" %in% names(ex))
  chk("the titrated reagents are named as expected, so nothing was scrubbed that should not be",
      setequal(trimws(ex$Alias), unlist(expJ$group)),
      sprintf("expected %s, got %s", paste(unlist(expJ$group), collapse = ", "),
              paste(unique(trimws(ex$Alias)), collapse = ", ")))

## ---- 2b. the dashboard, under the PLACEHOLDER id ---------------------------------------------
## This replaces the one OQ check excused above, and asks more of the run than that check did: the
## dashboard must hold the expected number of rows for this case AND they must be filed under the
## scrubbed id, with the real one nowhere in the page. A scrub that reached only the export would
## pass the export assertions above and fail here.
html <- list.files(outDir, pattern = "[.]html$", full.names = TRUE, recursive = TRUE)
chk("a dashboard was written", length(html) >= 1L, paste(basename(html), collapse = ", "))
if (length(html)) {
  txt  <- paste(readLines(html[1], warn = FALSE), collapse = "\n")
  m    <- regmatches(txt, regexpr("const DATA = \\[.*?\\];", txt))
  rows <- if (length(m))
            tryCatch(jsonlite::fromJSON(sub(";$", "", sub("const DATA = ", "", m)),
                                        simplifyVector = FALSE), error = function(e) NULL)
          else NULL
  elns <- if (is.null(rows)) character(0)
          else vapply(rows, function(r) as.character(r[["eln_id"]] %||% ""), "")
  chk("the dashboard holds this case's rows under the SCRUBBED ELN id",
      is.null(expJ$dashboard_rows) ||
        sum(elns == unname(SCRUB[["ELN_ID"]])) == expJ$dashboard_rows,
      sprintf("expected %s under %s, got %d (ids: %s)", format(expJ$dashboard_rows),
              unname(SCRUB[["ELN_ID"]]), sum(elns == unname(SCRUB[["ELN_ID"]])),
              paste(unique(elns), collapse = ", ")))
  realEln <- setdiff(realOf("ELN_ID"), c("", unname(SCRUB[["ELN_ID"]])))
  realEln <- c(realEln, if (!is.null(expJ$eln_id)) as.character(expJ$eln_id))
  realEln <- unique(realEln[nzchar(realEln) & realEln != unname(SCRUB[["ELN_ID"]])])
  chk("...and the real ELN id appears nowhere in the dashboard page",
      !any(vapply(realEln, function(r) grepl(r, txt, fixed = TRUE), TRUE)),
      paste(realEln, collapse = ", "))
}

## ---- 3. the bundle this run produced is clean by the scanner's own rules ----------------------
zips <- list.files(case, pattern = "TiterPubs.*[.]zip$", full.names = TRUE, recursive = TRUE)
chk("a TiterPubs bundle was built", length(zips) >= 1L, paste(basename(zips), collapse = ", "))
if (length(zips)) {
  z   <- zips[order(file.mtime(zips), decreasing = TRUE)][1]
  ## Seeded with the REAL values as literals, which is the check actaBundleScan() cannot make on
  ## its own: it has no pattern for an operator code or an ELN id.
  seed <- unlist(lapply(c("Operator", "ELN_ID", "ELN_Name", "SOP", "Costain_Panel_Iteration"),
                        function(cn) setdiff(realOf(cn), c("", placeholderOf(cn)))))
  seed <- seed[nchar(seed) >= 4L]          # a 2-3 character code is matched by .tpSiteTokens, not here
  extra <- if (length(seed)) stats::setNames(seed, paste0("site value ", seq_along(seed)))
           else character(0)
  f <- tryCatch(ACTA:::actaBundleScan(z, extra = extra, quiet = TRUE),
                error = function(e) data.frame(where = "scan", pattern = "ERROR",
                                               sample = conditionMessage(e)))
  blocking <- if (nrow(f)) f[ACTA:::actaBundleSeverity(f$pattern) == "blocking", , drop = FALSE]
              else f
  chk("the bundle has no blocking findings", nrow(blocking) == 0L,
      paste(utils::head(paste(blocking$pattern, blocking$where), 5), collapse = " ;; "))
  chk("...including none for the real workbook values, seeded as literals",
      !any(grepl("^site value", blocking$pattern)),
      paste(unique(blocking$sample), collapse = " ;; "))
}

cat(if (ok) "\nPASS test_oq_scrub_case.R\n" else "\nFAIL test_oq_scrub_case.R\n")
cat("case left at:", case, "\n")
quit(status = if (ok) 0L else 1L)
