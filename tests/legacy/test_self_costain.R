## .tpSelfCostain(): the titrated reagent must not be listed in its own co-stain panel.
##
## THE FAULT IT GUARDS. "Other antibodies and fluorophores in tube" is built from the declared
## iteration's `base` rows, so a workbook naming a panel that CONTAINS the titrated reagent
## publishes that reagent as one of its own co-stains. A deposited run shipped that way -- it
## declared a panel containing CD3 while CD3 was the reagent being titrated. The repair was a DATA
## fix (one cell), and nothing in the code stopped a recurrence until 3.6.12.
##
## ⚠ NO REAL IDENTIFIER BELONGS IN THIS FILE. The first draft named the ELN id of the run above, in
## both this file and the mapper, and the mapper's copy was caught by the scrub gate on the first
## full-suite run. This file was NOT caught, because the gate reads TRACKED files only and a new
## test is untracked until it is added -- see [[acta-scrub-gate-skips-untracked]].
##
## ⚠ THE GATE HAS NOTHING TO FIRE ON IN THE SHIPPED FIXTURES -- 15 reagents across all seven
## validation bundles, 0 hits. A test built only from them would pass whether or not the check
## works. So every fixture here is constructed, and sections 1 and 7 are CONTROLS that fail if the
## fixtures stop being adversarial.
##
## THE NORMALISATION IS THE INTERESTING PART. The lab master inventory holds two different CD3
## spellings: a clean `CD3`, and a `CD3` followed by a non-breaking space AND a zero-width space.
## The invisible one is what Costain_3/4/5 use -- i.e. exactly the panels that contain CD3 -- so a
## raw `==` comparison would pass on the only panels capable of failing. Section 1 proves that.

source(file.path(this.path::this.dir(), "acta_test_paths.R"))
ok <- TRUE
chk <- function(label, pass, detail = NULL) {
  d <- paste(as.character(detail), collapse = " ")
  if (!isTRUE(pass)) { ok <<- FALSE
    cat(sprintf("  [FAIL] %s%s\n", label, if (nzchar(d)) paste0(" -- ", d) else "")) }
  else cat(sprintf("  [ok]   %s\n", label))
}
## THIS CHECKOUT's sources, not whatever ACTA happens to be installed -- see actaTestFunctions().
h <- actaTestFunctions()
f <- h$.tpSelfCostain

## The invisible pair, written as escapes so this file stays pure ASCII and the one line whose job
## is to defeat invisible characters is itself readable. Same rule as .tpNorm()'s own literals.
INVIS <- " ​"                     # non-breaking space + zero-width space

mkpanel <- function(antigens, type = "base", fl = NULL) {
  if (!length(antigens))
    return(data.frame(Antigen = character(0), Fluorochrome = character(0),
                      Matched_to = character(0), Verdict = character(0), Catalog = character(0),
                      Vendor = character(0), Clone = character(0), Type = character(0),
                      stringsAsFactors = FALSE))
  data.frame(Antigen      = antigens,
             Fluorochrome = if (is.null(fl)) paste0("BV", 400 + seq_along(antigens)) else fl,
             Matched_to   = NA_character_, Verdict = "matched",
             Catalog      = NA_character_, Vendor  = NA_character_, Clone = NA_character_,
             Type         = rep(type, length.out = length(antigens)),
             stringsAsFactors = FALSE)
}
mkstats <- function(alias, iter, marker = alias) {
  data.frame(Alias = alias, Markername = marker, Costain_Panel_Iteration = iter,
             Cat_Num = "300438", Vendor = "BioLegend", Lot_Num = "B12345",
             stringsAsFactors = FALSE)
}

cat("\n-- 1. CONTROL: the fixture really does defeat a raw comparison --\n")
## If this stops holding, the invisible characters have been lost in transit and sections 3 and 4
## are passing for the wrong reason.
dirty <- paste0("CD3", INVIS)
chk("the dirty spelling is NOT equal to the clean one", !identical(dirty, "CD3"),
    sprintf("%d chars vs %d", nchar(dirty), nchar("CD3")))
chk("...and a raw == would MISS it", !isTRUE(dirty == "CD3"))
chk("...while .tpNorm() resolves them to the same string",
    identical(h$.tpNorm(dirty), "CD3"), h$.tpNorm(dirty))

cat("\n-- 2. the clean case: a reagent NOT in its own panel --\n")
clean <- list("Costain_2" = list(table = mkpanel(c("CD4", "CD8", "CD45"))))
r <- f(mkstats("CD3", "Costain_2"), clean)
chk("no finding", nrow(r) == 0L, nrow(r))

cat("\n-- 3. the fault: exact match, clean spelling --\n")
p3 <- list("Costain_3" = list(table = mkpanel(c("CD3", "CD4", "CD8"))))
r <- f(mkstats("CD3", "Costain_3"), p3)
chk("one finding", nrow(r) == 1L, nrow(r))
chk("it names the reagent",  identical(r$alias[1], "CD3"))
chk("it names the iteration", identical(r$iteration[1], "Costain_3"))
chk("it names the matched antigen", identical(r$antigen[1], "CD3"))
chk("the composition lists EVERY base row, not just the hit",
    grepl("CD3", r$composition[1]) && grepl("CD4", r$composition[1]) &&
    grepl("CD8", r$composition[1]), r$composition[1])

cat("\n-- 4. the fault with the INVISIBLE spelling, which is the real one --\n")
p4 <- list("Costain_4" = list(table = mkpanel(c(paste0("CD3", INVIS), "CD4", "CD8"))))
r <- f(mkstats("CD3", "Costain_4"), p4)
chk("still fires through NBSP + ZWSP", nrow(r) == 1L, nrow(r))
chk("the reported antigen is the NORMALISED form", identical(r$antigen[1], "CD3"), r$antigen[1])
## ...and the other way round: the WORKBOOK is the dirty one.
r <- f(mkstats(paste0("CD3", INVIS), "Costain_4b"),
       list("Costain_4b" = list(table = mkpanel(c("CD3", "CD4")))))
chk("fires when the invisible characters are on the WORKBOOK side", nrow(r) == 1L, nrow(r))

cat("\n-- 5. case folding --\n")
r <- f(mkstats("cd3", "Costain_5"), list("Costain_5" = list(table = mkpanel(c("CD3", "CD4")))))
chk("lower-case alias matches an upper-case panel row", nrow(r) == 1L, nrow(r))

cat("\n-- 6. EXACT MATCH ONLY -- never a substring, in either direction --\n")
near <- function(alias, antigens) nrow(f(mkstats(alias, "P"),
                                         list("P" = list(table = mkpanel(antigens)))))
chk("CD14 does NOT fire against CD14_CD16", near("CD14", c("CD14_CD16", "CD4")) == 0L)
chk("CD3 does NOT fire against CD39",       near("CD3",  c("CD39", "CD4")) == 0L)
chk("CD4 does NOT fire against CD45RA",     near("CD4",  c("CD45RA", "CD8")) == 0L)
chk("CD16 does NOT fire against CD14_CD16", near("CD16", c("CD14_CD16")) == 0L)
## The other direction: the PANEL row is the short one.
chk("CD45RA does NOT fire against CD4",     near("CD45RA", c("CD4", "CD8")) == 0L)
## ...but the whole-token forms still do, so the above is not just a dead check.
chk("CD14 DOES fire against a real CD14 row", near("CD14", c("CD14", "CD4")) == 1L)
chk("CD39 DOES fire against a real CD39 row", near("CD39", c("CD39")) == 1L)

cat("\n-- 7. CONTROL: a substring-based check would have failed section 6 --\n")
## Pins the reason section 6 is meaningful. If someone rewrites the comparison as grepl(), these
## say so rather than leaving section 6 to pass vacuously. EVERY pair above must genuinely be a
## substring relationship -- `CD3` inside `CD137` is NOT one (C-D-1-3-7), and an early draft of
## this control asserted that it was.
chk("every section-6 near miss IS a real substring relationship",
    grepl("CD14", "CD14_CD16", fixed = TRUE) && grepl("CD3",  "CD39",      fixed = TRUE) &&
    grepl("CD4",  "CD45RA",    fixed = TRUE) && grepl("CD16", "CD14_CD16", fixed = TRUE))

cat("\n-- 8. Markername as well as Alias --\n")
## .tpAntigen() publishes Alias or Markername depending on whether the run is combinatorial, so a
## check keyed on only one of them is blind on half the runs.
r <- f(mkstats("Tube_A", "Costain_8", marker = "CD3"),
       list("Costain_8" = list(table = mkpanel(c("CD3", "CD4")))))
chk("fires on Markername when the Alias differs", nrow(r) == 1L, nrow(r))
chk("...and reports the alias the operator typed", identical(r$alias[1], "Tube_A"), r$alias[1])

cat("\n-- 9. only `base` rows count --\n")
p9 <- list("Costain_9" = list(table = mkpanel(c("CD3", "CD4"), type = c("titrated", "base"))))
chk("a non-base row with the same antigen does NOT fire", nrow(f(mkstats("CD3", "Costain_9"), p9)) == 0L)
chk("a base row with the same antigen DOES fire",         nrow(f(mkstats("CD4", "Costain_9"), p9)) == 1L)
chk("Type is matched case-insensitively",
    nrow(f(mkstats("CD4", "C9u"),
           list("C9u" = list(table = mkpanel(c("CD4"), type = "BASE"))))) == 1L)

cat("\n-- 10. several reagents, reported together --\n")
st <- rbind(mkstats("CD3", "Costain_3"), mkstats("CD4", "Costain_3"), mkstats("CD19", "Costain_3"))
r  <- f(st, list("Costain_3" = list(table = mkpanel(c("CD3", "CD4", "CD8")))))
chk("two of three reagents are reported in ONE pass", nrow(r) == 2L, nrow(r))
chk("and the message carries both", {
  m <- h$.tpSelfCostainMsg(r); grepl("CD3", m) && grepl("CD4", m) && length(strsplit(m, "\n")[[1]]) == 2L
}, h$.tpSelfCostainMsg(r))

cat("\n-- 11. it declines, quietly, where there is nothing to judge --\n")
chk("no panels -> no finding",        nrow(f(mkstats("CD3", "Costain_3"), NULL)) == 0L)
chk("empty panels -> no finding",     nrow(f(mkstats("CD3", "Costain_3"), list())) == 0L)
chk("no stats -> no finding",         nrow(f(NULL, p3)) == 0L)
chk("empty stats -> no finding",      nrow(f(mkstats("CD3", "Costain_3")[0, ], p3)) == 0L)
chk("blank iteration -> no finding",  nrow(f(mkstats("CD3", ""), p3)) == 0L)
chk("NA iteration -> no finding",     nrow(f(mkstats("CD3", NA_character_), p3)) == 0L)
chk("iteration not in panels -> no finding", nrow(f(mkstats("CD3", "Costain_99"), p3)) == 0L)
chk("no Costain_Panel_Iteration column -> no finding", {
  s <- mkstats("CD3", "Costain_3"); s$Costain_Panel_Iteration <- NULL
  nrow(f(s, p3)) == 0L })
chk("an empty panel table -> no finding",
    nrow(f(mkstats("CD3", "E"), list("E" = list(table = mkpanel(character(0)))))) == 0L)
chk("a panel table missing Antigen -> no finding", {
  t2 <- mkpanel(c("CD3")); t2$Antigen <- NULL
  nrow(f(mkstats("CD3", "E2"), list("E2" = list(table = t2)))) == 0L })
chk("a blank alias AND blank markername -> no finding",
    nrow(f(mkstats("", "Costain_3", marker = ""), p3)) == 0L)
## A blank panel Antigen must not match a blank alias into a phantom finding.
chk("blank panel antigens do not match a blank alias",
    nrow(f(mkstats("", "B", marker = ""),
           list("B" = list(table = mkpanel(c("", NA_character_)))))) == 0L)

cat("\n-- 12. END TO END: the bundle REFUSES, before writing anything --\n")
## Driving the real entry point, not just the predicate -- a gate that is never wired in is not a
## gate. The catalogue/vendor refusal above it runs first, so the fixture fills both.
stage <- file.path(tempdir(), "selfcostain_e2e"); unlink(stage, recursive = TRUE)
dir.create(stage, recursive = TRUE, showWarnings = FALSE)
tpl <- list.files(file.path(ACTA_REPO_ROOT, "inst", "pipeline", "Template"),
                  pattern = "^TiterPubs_template.*[.]xlsx$", full.names = TRUE)
info <- data.frame(Category = "Export", Variable = "titerpubs_export", Value = "TRUE",
                   Description = "", stringsAsFactors = FALSE)
if (!length(tpl)) {
  chk("the shipped TiterPubs template was found", FALSE,
      "no TiterPubs_template*.xlsx under inst/pipeline/Template")
} else {
  e <- tryCatch({
    h$actaTiterPubsBundle(stats = mkstats("CD3", "Costain_3"), info = info, outputs_dir = stage,
                          template = tpl[1], run_dir = stage, panels = p3)
    NULL
  }, error = function(e) conditionMessage(e))
  ## ⚠ "IT ERRORED" IS NOT EVIDENCE THE GATE RAN. Measured by deleting the check: the bundle still
  ## fails, with an unrelated message ("the run produced no report PDF"), because this fixture has
  ## no rendered report. So every assertion below reads the MESSAGE; a bare !is.null(e) would have
  ## passed against a build with the check removed entirely.
  chk("the bundle refuses at all", !is.null(e), "it returned without error")
  chk("the message says no bundle was written",
      !is.null(e) && grepl("NO BUNDLE WAS WRITTEN", e, fixed = TRUE), e)
  chk("the message names the reagent",      !is.null(e) && grepl("CD3", e, fixed = TRUE))
  chk("the message names the iteration",    !is.null(e) && grepl("Costain_3", e, fixed = TRUE))
  chk("the message names the resolved composition",
      !is.null(e) && grepl("CD4", e, fixed = TRUE) && grepl("CD8", e, fixed = TRUE))
  chk("the message names the cell to fix",
      !is.null(e) && grepl("Costain_Panel_Iteration", e, fixed = TRUE))
  ## REFUSED BEFORE BUILDING, not built and deleted: that distinction is the whole point of where
  ## the check sits, and a leftover .zip or .xlsx would prove it had started staging.
  chk("nothing was written to the output folder",
      length(list.files(stage, recursive = TRUE)) == 0L,
      paste(list.files(stage, recursive = TRUE), collapse = ", "))

  ## ...and the clean case gets PAST this check. Without it, "refuses" could just mean "always
  ## refuses" and every assertion above would be satisfied by a stop() at the top of the function.
  e2 <- tryCatch({
    h$actaTiterPubsBundle(stats = mkstats("CD3", "Costain_2"), info = info, outputs_dir = stage,
                          template = tpl[1], run_dir = stage, panels = clean)
    NULL
  }, error = function(e) conditionMessage(e))
  chk("a CLEAN panel is not refused by this check",
      is.null(e2) || !grepl("own co-stain", e2, fixed = TRUE), e2)
}

cat("\n-- 13. the check is WIRED IN, in both places --\n")
## Same rule as test_strip_label.R: the predicate passing proves nothing if nobody calls it.
mp <- readLines(file.path(ACTA_REPO_ROOT, "R", "TiterPubs_template_mapper.R"), warn = FALSE)
sc <- readLines(file.path(ACTA_REPO_ROOT, "inst", "pipeline", "ACTA_Script.R"), warn = FALSE)
i  <- grep("^\\s*\\.self\\s*<-\\s*\\.tpSelfCostain\\(", mp)
chk("the bundle builder calls it", length(i) == 1L, length(i))
chk("...and the next statement is a stop()",
    length(i) == 1L && any(grepl("^\\s*stop\\(", mp[(i + 1):(i + 3)])),
    if (length(i) == 1L) mp[i + 1] else "")
j <- grep("\\.tpSelfCostain\\(statsForExport, panelIterationInfo\\)", sc)
chk("the run warns at the point the panels resolve", length(j) == 1L, length(j))
chk("...with warning(), not stop() -- the analysis is unaffected",
    length(j) == 1L && any(grepl("warning\\(", sc[j:(j + 8)])) &&
    !any(grepl("^\\s*stop\\(", sc[j:(j + 8)])))
chk("...and it is NOT gated on titerpubs_export",
    length(j) == 1L && !any(grepl("titerpubs_export", sc[max(1, j - 6):j])))

cat(if (ok) "\nSELF-COSTAIN TESTS PASSED\n" else "\nFAILURES ABOVE\n")
quit(status = if (ok) 0L else 1L)
