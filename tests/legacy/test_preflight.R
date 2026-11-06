## Guards the template pre-flight and the artefact-claim helpers -- the "silent wrong answer" family.
##
## What these exist for (all reported/measured 2026-08-18):
##   * a `dims` token naming nothing in the panel reached openCyto and failed AFTER plots were written
##   * a quadrant `pop` died with the opaque "NA not found!"
##   * run_acta() globbed the folder for the export, so a failed run claimed a PREVIOUS run's file,
##     and actaReadyMessage() then said "Titration export is ready" unconditionally
##
## Rscript <this> [version_dir]   -- defaults to 3_0
## One cwd contract for the whole suite -- see acta_test_paths.R. Run this script from anywhere.
source(file.path(this.path::this.dir(), "acta_test_paths.R"))
vd <- actaTestVersionDir()
cat("version_dir:", basename(vd), "\n")
h <- actaTestFunctions(vd)
ok <- TRUE
chk <- function(what, cond) { if (!isTRUE(cond)) ok <<- FALSE
                              cat(sprintf("  [%s] %s\n", if (isTRUE(cond)) "ok" else "FAIL", what)) }

MARK <- c("CD3", "CD4", "CD8", "GZMK", "MarkerB", "LIVEDEAD")
CHAN <- c("FSC-A", "FSC-H", "SSC-A", "BUV805-A", "BUV496-A", "Alexa Fluor 532-A", "FITC-A", "PE-A",
          "FVD eFluor 780-A")

cat("=== dims resolution ===\n")
al <- c("Cells", "Live", "CD3+", "Reagent")
chk("markers and scatter both resolve", length(
  h$actaUnresolvedDims(c("FSC-A,SSC-A", "LIVEDEAD, FSC-A", "CD3, FSC-A", "FITC-A"), al, MARK, CHAN)) == 0)
chk("case does not matter (mirrors resolveDimsToChannels)",
    length(h$actaUnresolvedDims(c("fsc-a,ssc-a", "livedead, FSC-A"), al[1:2], MARK, CHAN)) == 0)
u <- h$actaUnresolvedDims(c("FSC-A,SSC-A", "Viability, FSC-A"), c("Cells", "Live"), MARK, CHAN)
chk("a token naming nothing is reported once", length(u) == 1)
chk("the message names the offending token and its row",
    grepl("'Viability'", u[1], fixed = TRUE) && grepl("row 2", u[1]) && grepl("alias 'Live'", u[1]))
chk("commented-out rows are ignored -- the app passes the RAW sheet",
    length(h$actaUnresolvedDims(c("FSC-A,SSC-A", "Viability, FSC-A"), c("Cells", "#Live"),
                                MARK, CHAN)) == 0)
chk("blank / NA dims are not errors",
    length(h$actaUnresolvedDims(c("", NA), c("A", "B"), MARK, CHAN)) == 0)

## A row the scaffold plots needs TWO dims. Reported by the user on 2026-08-26: a viability row
## written the ordinary openCyto way (`dims = LIVEDEAD`, one channel) gated fine and then killed the
## run in computePlotParams() with "subscript out of bounds", after the whole tree was built. Neither
## OQ case has a single-dim row -- both write `Viability, FSC-A` -- which is why nothing caught it.
cat("=== dims are two-dimensional ===\n")
d2 <- function(dims, aliases) h$actaDimsNot2D(dims, aliases)
GT_OK  <- list(c("FSC-A,SSC-A", "FSC-A, FSC-H", "LIVEDEAD, FSC-A", "FSC-A"),
               c("Cells", "Single_cells", "Live", "Reagent"))
GT_BAD <- list(c("FSC-A,SSC-A", "FSC-A, FSC-H", "LIVEDEAD", "FSC-A"),
               c("Cells", "Single_cells", "Live", "Reagent"))
chk("a fully 2D template raises nothing", !length(d2(GT_OK[[1]], GT_OK[[2]])$fatal) &&
                                          !length(d2(GT_OK[[1]], GT_OK[[2]])$notes))
b <- d2(GT_BAD[[1]], GT_BAD[[2]])
## NO LONGER FATAL, since 2026-08-27: actaPlotDims() supplies the instrument's forward scatter as
## the display y axis, so a one-dim row RUNS. The pre-flight still says so, because the operator
## should know which axis they are looking at -- but it must not block a template that is perfectly
## ordinary openCyto.
chk("a single-dim row is a NOTE, not a failure", !length(b$fatal) && length(b$notes) == 1)
chk("the note names the row and its alias",
    grepl("row 3", b$notes[1]) && grepl("alias 'Live'", b$notes[1], fixed = TRUE))
chk("...says the gate is unaffected",
    grepl("gate is unaffected", b$notes[1], fixed = TRUE))
chk("...and tells them how to choose the axis themselves",
    grepl('dims = "LIVEDEAD, FSC-A"', b$notes[1], fixed = TRUE))

## The example must come from the SHEET. "FSC-A" is not a channel on a ZE5 or a Guava, and telling
## somebody to type a channel their instrument does not have is worse than naming none.
chk("the example scatter channel is taken from the template, not a literal",
    grepl('dims = "LIVEDEAD, FS00-A"',
          d2(c("FS00-A,SS02-A", "LIVEDEAD"), c("Cells", "Live"))$notes[1], fixed = TRUE))
chk("...falling back to FSC-A when the sheet names no scatter at all",
    grepl('dims = "LIVEDEAD, FSC-A"',
          d2(c("CD4, CD8", "LIVEDEAD"), c("CD4_CD8", "Live"))$notes[1], fixed = TRUE))
## The marker row is the exception, and in the other direction: the script prepends the group's
## marker channel and keeps only the first sheet token as y.
chk("the marker row is allowed its single dim", !length(d2("FSC-A", "Reagent")$fatal))
chk("...under either accepted alias", !length(d2(c("SSC-A", "FSC-A"), c("Antibody", "reagent"))$fatal))
chk("a marker row with two dims is a note, not a failure",
    { m <- d2("FSC-A, SSC-A", "Reagent"); !length(m$fatal) && length(m$notes) == 1 })
chk("more than two dims is a note, not a failure",
    { m <- d2("CD4, CD8, CD3", "T"); !length(m$fatal) && length(m$notes) == 1 })
chk("commented-out rows are ignored -- the app passes the RAW sheet",
    !length(d2(c("FSC-A,SSC-A", "LIVEDEAD"), c("Cells", "#Live"))$fatal))
chk("blank / NA dims are left to the checks that own them",
    !length(d2(c("", NA), c("A", "B"))$fatal))
chk("an absent dims column is not an error", !length(d2(NULL, c("A","B"))$fatal))

## And it must reach the operator through the app's pre-flight, not only the script's.
.md <- data.frame(Dirname = "X", Row = "A", Column = 1:2, PlateID = 1, StainType = c("Stain","Costain"),
                  StainQty = c(1, 2), Fix = "N", Perm = "N", ELN_Name = "n", ELN_ID = "i",
                  Operator = "o", Markername = "CD3", stringsAsFactors = FALSE)
.rec <- function(dims) {
  gt <- data.frame(alias = GT_BAD[[2]], pop = c("+","+","-","+/-"), parent = "root", dims = dims,
                   gating_method = "gate_flowmeans", stringsAsFactors = FALSE)
  r <- h$actaPrevalidateRecords(.md, gt, NULL, "X", tempfile(), marker_col = "Markername")
  Filter(function(x) x$id == "template_dims_2d", r)
}
chk("the app's pre-flight WARNS on the single-dim sheet, and does not block it",
    { r <- .rec(GT_BAD[[1]])
      any(vapply(r, function(x) x$status == "warn" &&
                   grepl("alias 'Live'", x$detail, fixed = TRUE), logical(1))) &&
        !any(vapply(r, function(x) x$status == "fail", logical(1))) })
chk("...and passes the corrected one", { r <- .rec(GT_OK[[1]])
                                         length(r) == 1 && r[[1]]$status == "pass" })

## The display axis itself. This is what makes a one-dim row runnable, and it must come from the
## INSTRUMENT: "FSC-A" does not exist on a ZE5 (FS00-A) or a Guava (FSC-HLin).
cat("=== the display y axis for a one-dim row (actaPlotDims) ===\n")
AUR <- c("FSC-A", "FSC-H", "SSC-A", "BUV805-A", "LIVEDEAD-A")
ZE5 <- c("FS00-A", "SS02-A", "BV 605-A")
chk("two dims pass through untouched",
    identical(h$actaPlotDims(c("LIVEDEAD", "FSC-A"), AUR), c("LIVEDEAD", "FSC-A")))
chk("a one-dim row gains the forward scatter",
    identical(h$actaPlotDims("LIVEDEAD", AUR), c("LIVEDEAD", "FSC-A")))
chk("...taken from the instrument, not the literal FSC-A",
    identical(h$actaPlotDims("BV 605-A", ZE5), c("BV 605-A", "FS00-A")))
## Pairing a channel with itself is a degenerate panel, so the next scatter is used instead.
chk("a row whose only dim IS the forward scatter gets a different scatter",
    identical(h$actaPlotDims("FSC-A", AUR), c("FSC-A", "FSC-H")))
chk("more than two dims keeps the first two", identical(h$actaPlotDims(c("CD4","CD8","CD3"), AUR),
                                                       c("CD4", "CD8")))
chk("no dims at all yields nothing to plot", !length(h$actaPlotDims(character(0), AUR)))
## And it must FAIL loudly rather than invent something when there is no second scatter to use.
chk("only one scatter channel, and the row names it -> hard error naming the row",
    inherits(try(h$actaPlotDims("FSC-A", c("FSC-A", "BUV805-A"), alias = "Cells"), silent = TRUE),
             "try-error"))

cat("=== pop values ===\n")
## Every form openCyto's CSV template accepts must pass -- the list is there to catch typos, not to
## make ACTA a subset of openCyto. "+/-+/-" was rejected on 2026-08-18 before it was understood that
## openCyto expands it into six NAMED, PARENTABLE populations; that rejection was the mistake.
chk("+, - and +/- are supported",
    length(h$actaUnsupportedPops(c("+", "-", "+/-"), c("Cells", "Live", "Reagent"))) == 0)
chk("the quadrant forms are supported",
    length(h$actaUnsupportedPops(c("+/-+/-", "++", "+-", "-+", "--", "*"),
                                 paste0("R", 1:6))) == 0)
b <- h$actaUnsupportedPops(c("+", "+/-+"), c("Cells", "Typo"))
chk("a malformed pop is still rejected", length(b) == 1)
chk("the message lists what is valid",
    grepl("not a valid openCyto pop", b[1]) && grepl("row 2", b[1]) && grepl("[+]/-[+]/-", b[1]))
chk("a commented-out row is left alone",
    length(h$actaUnsupportedPops(c("+", "+/-+"), c("Cells", "#Typo"))) == 0)

cat("=== artefact claims ===\n")
tmp <- file.path(tempdir(), "acta_preflight_export.xlsx"); writeLines("x", tmp)
chk("export claimed when the file exists",  isTRUE(h$actaExportMade(list(export_file = tmp))))
chk("not claimed when the path is NA",     !isTRUE(h$actaExportMade(list(export_file = NA_character_))))
chk("not claimed when the file is absent",
    !isTRUE(h$actaExportMade(list(export_file = file.path(tempdir(), "no_such_export.xlsx")))))
arts <- h$actaRunArtefacts(list(version_dir = tempdir(), report_ok = FALSE, wrote_plots = FALSE,
                                plots_dir = NA_character_, export_file = NA_character_))
chk("artefact list marks a missing export 'not written'",
    is.na(arts$export$path) && identical(arts$export$skipped, "not written"))
chk("no artefacts + failed analysis -> says the analysis did not complete",
    h$actaReadyMessage(list(report_ok = FALSE, ok = FALSE, export_file = NA_character_)) ==
      "Analysis did not complete -- nothing was written")
chk("no artefacts but analysis fine -> does not invent an export",
    h$actaReadyMessage(list(report_ok = FALSE, ok = TRUE, export_file = NA_character_)) ==
      "No report, dashboard or titration export was produced")
chk("export present -> the old message still stands",
    h$actaReadyMessage(list(report_ok = FALSE, ok = TRUE, export_file = tmp)) ==
      "Titration export is ready")
chk("a rendered report still reads as before",
    h$actaReadyMessage(list(report_ok = TRUE, ok = TRUE, export_file = tmp)) == "Report is ready")
unlink(tmp)

cat("=== no false positives on the real workbooks ===\n")
suppressMessages(library(readxl))
## INSTRUCTIONS WORKBOOKS ONLY. Template/ was a bare *.xlsx glob until the TiterPubs submission
## template moved in beside TEMPLATE_Ab_Titration_Instructions.xlsx, at which point this loop read
## a workbook that has no gating_template sheet and the whole file died on "Sheet not found".
## Preflight validates ACTA instruction workbooks by definition, so the glob now says so rather
## than meaning "whatever xlsx happens to be in that folder".
for (wb in Sys.glob(c(file.path(vd, "*Instructions*.xlsx"),
                      file.path(vd, "Template", "*Instructions*.xlsx"),
                      file.path(actaTestCaseRoot(), "*", "*Instructions*.xlsx")))) {
  g <- suppressMessages(as.data.frame(read_excel(wb, sheet = "gating_template", col_types = "text")))
  chk(sprintf("%s: no pop rejected", basename(wb)),
      length(h$actaUnsupportedPops(g$pop, g$alias)) == 0)
  ## Every workbook we ship or diagnose with must be 2D-clean, or the pre-flight added on
  ## 2026-08-26 turns into a hard stop on our own files.
  chk(sprintf("%s: every plotted row names two dims", basename(wb)),
      length(h$actaDimsNot2D(g$dims, g$alias)$fatal) == 0)

  ## WORKBOOK INTEGRITY, every sheet, every cell. Added 2026-09-03 after a tool corrupted a
  ## workbook it was only supposed to append a column to: openxlsx's loadWorkbook + saveWorkbook
  ## rewrites the WHOLE file and mangles shared strings carrying xml:space="preserve", turning the
  ## TEMPLATE's gating_template cell `CD3, FSC-A` into `xml:space="preserve">CD3, FSC-A` -- in a
  ## sheet nobody was editing. It surfaced only because an unrelated pop assertion tripped over it.
  ##
  ## Deliberately NOT a diff against the committed copy: that check goes vacuous the moment the
  ## edit is committed, and cannot see corruption that is already in history. XML leaking into a
  ## cell value is wrong whenever it appears and whoever committed it.
  for (sh in readxl::excel_sheets(wb)) {
    cells <- unlist(suppressMessages(readxl::read_excel(wb, sheet = sh, col_types = "text")),
                    use.names = FALSE)
    cells <- as.character(cells[!is.na(cells)])
    bad <- grep("xml:space|</?t>|</?si>|</?is>|preserve\"", cells, value = TRUE)
    chk(sprintf("%s / %s: no XML fragment in any cell value", basename(wb), sh),
        length(bad) == 0)
    if (length(bad)) cat("      first:", substr(bad[[1]], 1, 60), "\n")
  }
  ## PHANTOM COLUMNS. readxl names a header-row gap `...N`, so these appear the moment a column is
  ## written past the last header instead of immediately after it -- which is exactly what happened
  ## when a sheet with <dimension ref="A1"> sent the same tool to column AB, leaving ...25/...26/...27
  ## in front of the new header.
  lp <- names(suppressMessages(readxl::read_excel(wb, sheet = "Layout_Plate", n_max = 0)))
  chk(sprintf("%s: Layout_Plate has no phantom `...N` columns", basename(wb)),
      !any(grepl("^[.]{3}[0-9]+$", lp)))
}

cat("=== the unit of analysis: one titrated marker (actaTitrationGroups) ===\n")
## Combinatorial titration means several markers titrated in the same wells, so one Dirname can carry
## several series. The layout says so in long form -- one row per well per marker -- and this turns
## those rows into the loop's unit of work.
tg <- function(dn, al = NULL, ch = NULL) {
  d <- data.frame(Dirname = dn, stringsAsFactors = FALSE)
  if (!is.null(al)) d$Alias <- al
  if (!is.null(ch)) d[["Channel ($PnN)"]] <- ch
  h$actaTitrationGroups(d, channel_col = if (is.null(ch)) NULL else "Channel ($PnN)")
}
chk("one Alias per Dirname -> the key IS the Dirname",
    identical(tg(c("Strep_PE","Strep_PE"), c("Strep","Strep"))$key, "Strep_PE"))
## Not a stylistic choice. The key names plot files, keys qcList and labels the dashboard, and Alias
## is NOT interchangeable with Dirname even now: OQ_Test1 is Dirname "Strep_PE", Alias "Strep".
## Keying on Alias unconditionally would rename that case's artefacts and move a baseline.
chk("...even when the Alias differs from it",
    identical(tg("Strep_PE", "Strep")$key, "Strep_PE"))
chk("several Aliases in one Dirname -> keyed by Alias",
    identical(tg(c("P","P"), c("CD4","CD8"))$key, c("CD4","CD8")))
chk("and the combinatorial flag says so",
    isTRUE(attr(tg(c("P","P"), c("CD4","CD8")), "combinatorial")) &&
      isFALSE(attr(tg("P", "CD4"), "combinatorial")))

## FIRST-APPEARANCE ORDER. Sorting alphabetically reordered OQ_Test2's groups from CD19/CD7/CD56 to
## CD19/CD56/CD7 and the OQ caught it. The order is load-bearing: it sets the report's section order,
## the export row order, and which group's FCS supplies the MiFlowCyt $PnV table.
chk("group order follows the LAYOUT, not the alphabet",
    identical(tg(c("CD19","CD7","CD56"))$key, c("CD19","CD7","CD56")))

chk("no Alias column at all (legacy sheet) -> Dirname",
    identical(tg(c("X","X","Y"))$key, c("X","Y")))
chk("a blank Alias falls back to the Dirname",
    identical(tg(c("X","X"), c(NA, ""))$key, "X"))

cat("=== and it refuses the four ways the long layout goes wrong ===\n")
ref <- function(expr) tryCatch({ force(expr); FALSE }, error = function(e) TRUE)
chk("a key that collides with another Dirname's name",
    ref(tg(c("P","P","CD4"), c("CD4","CD8","CD4"), c("a","b","c"))))
chk("two co-titrated markers on the SAME channel",
    ref(tg(c("P","P"), c("CD4","CD8"), c("BUV496-A","BUV496-A"))))
chk("one titration naming two channels",
    ref(tg(c("P","P"), c("CD4","CD4"), c("a","b"))))
## The key names files, and unlike a Dirname (a real folder) an Alias is free text.
chk("an Alias that cannot be a filename", ref(tg(c("P","P"), c("CD4/8","CD8"))))

cat("=== membership: which layout rows belong to one titration ===\n")
lay <- data.frame(Dirname = c("P","P","P","P","Q"),
                  Alias   = c("CD4","CD8","CD4","CD8","R"),
                  StainQty = c(5, 2.5, 1, 0.5, 9), stringsAsFactors = FALSE)
chk("picks only that titration's rows",
    identical(lay$StainQty[h$actaTitrationRowMask(lay, "P", "CD4")], c(5, 1)))
chk("does not leak another Dirname's rows",
    !any(h$actaTitrationRowMask(lay, "P", "CD4") & lay$Dirname == "Q"))
chk("blank Alias resolves to the Dirname here too, as in the grouping",
    { l2 <- data.frame(Dirname = c("P","P"), Alias = c(NA, "CD8"), stringsAsFactors = FALSE)
      identical(which(h$actaTitrationRowMask(l2, "P", "P")), 1L) })

cat("=== prevalidation understands a combinatorial sheet (the 2026-08-21 regression) ===\n")
## The first real combinatorial dataset failed pre-flight with "CD4: 0 Costain wells -- exactly one is
## required", naming an Alias. Cause: making the loop's unit a titrated marker changed `antibody` from
## a Dirname to a KEY, and five consumers still did `metadata$Dirname == ab` or looked for a folder of
## that name. Handed a key they matched nothing; handed a folder carrying two markers they pool both.
comb <- data.frame(
  Dirname   = rep("Panel_A", 8), Alias = rep(c("CD4","CD8"), each = 4),
  Row = rep("A", 8), Column = rep(1:4, 2), PlateID = 1,
  StainType = rep(c("Stain","Stain","Stain","Costain"), 2),
  ## BOTH series run the same dilutions, which is the realistic combinatorial case and the one that
  ## exposes the pooling hazard: per titration each quantity appears once, but a folder-wide view
  ## sees every one of them twice.
  StainQty  = c(10, 5, 2.5, 10,   10, 5, 2.5, 10),
  `Channel ($PnN)` = rep(c("BUV496-A","AF532-A"), each = 4),
  flowjo_transformation_arg = "pos=4.5", check.names = FALSE, stringsAsFactors = FALSE)
cg <- h$actaTitrationGroups(comb, channel_col = "Channel ($PnN)")
lg <- function(md, gr) h$validateLayoutGroups(md, gr, stop_on_error = FALSE, return_all = TRUE)

chk("a valid combinatorial sheet raises nothing", !length(lg(comb, cg)$fatal))
## Each series has its own Costain well, so per-folder slicing sees two and calls one too many.
chk("...whereas slicing by FOLDER would report 2 Costain wells",
    any(grepl("2 Costain", lg(comb, unique(comb$Dirname))$fatal)))
## And the stain quantities only look duplicated when the two series are pooled: 10 appears once in
## CD4's series and 8 once in CD8's, but a folder-wide view sees each twice (stain + costain).
chk("...and would report duplicate StainQty that belong to different series",
    any(grepl("duplicate StainQty", lg(comb, unique(comb$Dirname))$fatal)))
## A duplicate WITHIN one series is still caught -- the check must not have been weakened.
chk("a real duplicate inside one series is still fatal",
    { d <- comb; d$StainQty[2] <- 2.5                     # CD4's Stain wells: 10, 2.5, 2.5
      any(grepl("duplicate StainQty", lg(d, h$actaTitrationGroups(d))$fatal)) })
chk("and the message names the Alias AND the folder, so it says where to look",
    { d <- comb; d$StainQty[2] <- 2.5
      grepl("CD4 (Dirname 'Panel_A')", lg(d, h$actaTitrationGroups(d))$fatal[1], fixed = TRUE) })

## The silent one: an empty match is the documented "use defaults" path, so handing keys to
## actaTransArgs ignored the sheet's transform without a word and every SI used different arguments.
chk("the sheet's transform is READ for a combinatorial layout",
    identical(h$actaTransArgs(comb, cg), list(pos = 4.5)))
chk("...and was silently dropped when handed titration keys",
    !length(h$actaTransArgs(comb, cg$key)))

## Un-migrated callers must keep working: a bare Dirname vector still means one titration per folder.
chk("a bare Dirname vector is still accepted",
    { d <- data.frame(Dirname = c("X","X"), Row = "A", Column = 1:2, PlateID = 1,
                      StainType = c("Stain","Costain"), StainQty = c(1, 2),
                      stringsAsFactors = FALSE)
      !length(lg(d, "X")$fatal) })

## ---------------------------------------------------------------------------------------------
## STAINTYPE IS THE ONLY CLASSIFIER. actaFindAntibodyFcs() used to also drop any file whose NAME
## contained "Unstain", which made the filename a second and invisible classifier: a stained file
## called CD3_Unstained_comparison.fcs disappeared from the run, and an unstained well whose file
## was not named that way was loaded and titrated. Neither shows up in the layout, which is the
## document the operator checks, and the report's plate map exists so that check is possible.
## Discovery now returns every .fcs and the StainType column decides, once, in the script.
cat("\n=== discovery does not classify by filename ===\n")
local({
  root <- file.path(tempdir(), "acta_fcs_name_gate"); unlink(root, recursive = TRUE)
  dir.create(file.path(root, "AB"), recursive = TRUE, showWarnings = FALSE)
  nms <- c("AB/run_A1_Unstained.fcs",          # an unstained control, named so
           "AB/CD3_Unstained_comparison.fcs",  # a STAINED file that merely says unstained
           "AB/run_A2.fcs", "AB/run_A3.fcs")
  for (f in nms) file.create(file.path(root, f))
  got <- h$actaFindAntibodyFcs(root, "AB")
  chk(sprintf("every .fcs is returned regardless of its name (%d of %d)", length(got), length(nms)),
      setequal(got, nms))
  chk("a file named *Unstained* is NOT dropped by discovery",
      "AB/run_A1_Unstained.fcs" %in% got)
  chk("a stained file whose name merely contains Unstained is NOT dropped",
      "AB/CD3_Unstained_comparison.fcs" %in% got)
  ## And the parameter that used to do it is gone, so nothing can re-enable it by passing TRUE.
  chk("actaFindAntibodyFcs has no drop_unstained argument",
      !("drop_unstained" %in% names(formals(h$actaFindAntibodyFcs))))
  unlink(root, recursive = TRUE)
})
## Textual, because the fixture above only covers the one function: nothing in the shipped tree may
## match "Unstain" against a PATH again. StainType comparisons go through isLayoutValue(), which is
## an exact match on a column, so they do not look like this.
local({
  files <- c(actaTestFunctionsFiles(vd),
             list.files(file.path(ACTA_REPO_ROOT, "inst", "pipeline"),
                        pattern = "[.](R|Rmd)$", full.names = TRUE))
  hits <- character(0)
  for (f in files) {
    ln <- readLines(f, warn = FALSE)
    ln <- ln[!grepl("^\\s*#", ln)]
    ## CASE-INSENSITIVE AND WIDER THAN ONE FUNCTION. The first version matched only
    ## grepl("Unstain" with a capital U, which could not see the lower-case grepl("unstain",
    ## basename(hit)) this very release added a few functions away -- so the idiom it forbids was
    ## the idiom it could not detect, and a reintroduced second classifier would have shipped.
    ## The allow-list is the one intentional use: a pre-flight DIAGNOSTIC that counts
    ## unstained-looking files to explain a count mismatch. It classifies nothing.
    bad <- grep("(grepl|grep|startsWith|str_detect|regexpr)\\(.*(unstain)", ln,
                value = TRUE, ignore.case = TRUE)
    ## The allow-list is ONE known line, not a shape. Excusing any line containing
    ## `basename(hit)` excused a reintroduced filter that reused that variable name -- and `hit`
    ## is exactly what the neighbouring discovery code calls its FCS paths, so the evasion was a
    ## rename away. Allowed: the single diagnostic in ACTA_Functions.R that COUNTS unstained-looking
    ## files to explain a row-count mismatch and subtracts the listed rows. It classifies nothing,
    ## and it must appear exactly once.
    okline <- grepl("basename(hit)", bad, fixed = TRUE) &
              grepl("sum(grepl(", bad, fixed = TRUE) &
              identical(basename(f), "ACTA_Functions.R")
    if (sum(okline) > 1L)
      hits <- c(hits, sprintf("%s: the counting diagnostic appears %d times, expected once",
                              basename(f), sum(okline)))
    bad <- bad[!okline]
    if (length(bad)) hits <- c(hits, sprintf("%s: %s", basename(f), trimws(bad)))
  }
  chk("no shipped file matches \"Unstain\" against a filename or path", length(hits) == 0L)
  for (x in hits) cat("         ", x, "\n")
})

## COSTAIN IS REQUIRED PER GROUP *WHEN A GATE READS THE FLOOR*, UNSTAINED NEVER IS. Zero Costain
## leaves the floor at -Inf -- no floor at all, which looks like a successful run -- and more than
## one pools events so the floor belongs to no single well. Unstained is dropped before gating and
## contributes nothing, so a group without one is a legitimate layout. Pinned here because the
## difference is invisible in the output: a run with no floor still produces numbers.
##
## THE DEFAULT IS STILL "REQUIRED", and these cases assert it by calling without the argument --
## so a caller that has not been taught about gating methods keeps the strict behaviour. The
## conditional half is asserted in the section below this one.
cat("\n=== Costain required per group, Unstained optional ===\n")
local({
  mk <- function(types, qty) data.frame(
    Dirname = "X", Alias = "X", PlateID = 1,
    Row = LETTERS[seq_along(types)], Column = 1,
    StainType = types, StainQty = qty, stringsAsFactors = FALSE)
  res <- function(types, qty) {
    n <- character(0)
    r <- withCallingHandlers(
      h$validateLayoutGroups(mk(types, qty), "X", stop_on_error = FALSE, return_all = TRUE),
      warning = function(w) { n <<- c(n, conditionMessage(w)); invokeRestart("muffleWarning") })
    list(fatal = length(r$fatal), warn = length(n))
  }
  a <- res(c("Stain","Stain","Stain","Costain"), c(1,2,3,0))
  chk("NO Unstained well is accepted silently -- not fatal, not even a note",
      a$fatal == 0L && a$warn == 0L)
  b <- res(c("Stain","Stain","Stain","Costain","Unstained"), c(1,2,3,0,0))
  chk("one Unstained well is accepted", b$fatal == 0L && b$warn == 0L)
  cc <- res(c("Stain","Stain","Stain","Costain","Unstained","Unstained"), c(1,2,3,0,0,0))
  chk("two Unstained wells warn but do not fail", cc$fatal == 0L && cc$warn == 1L)
  d <- res(c("Stain","Stain","Stain"), c(1,2,3))
  chk("NO Costain well is fatal", d$fatal == 1L)
  e <- res(c("Stain","Stain","Stain","Costain","Costain"), c(1,2,3,0,0))
  chk("two Costain wells are fatal", e$fatal == 1L)
  f <- res(c("Stain","Stain","costain","unstained"), c(1,2,0,0))
  chk("StainType matching is case-insensitive (lower-case costain/unstained recognised)",
      f$fatal == 0L && f$warn == 0L)
})

## ---- the Costain rule is conditional on the gating template ---------------------------------
## WHY THIS IS A GATE AND NOT A COMMENT: the relaxation is only correct while "needs a floor" and
## "names a *_costain method" mean the same thing. If actaTemplateNeedsCostain() ever answers
## FALSE for a template that does read the floor, ACTA stops requiring the well that method is
## built on and the run still produces numbers -- the exact silent-wrong-answer shape this file
## exists for. So every member of the family is named individually rather than trusting the
## substring to keep covering them.
cat("\n=== the Costain requirement follows the gating template ===\n")
local({
  needs <- h$actaTemplateNeedsCostain
  gt <- function(meth, pp = NA_character_, alias = "Reagent")
    data.frame(alias = alias, gating_method = meth, preprocessing_method = pp,
               stringsAsFactors = FALSE)
  ## Every registered member of the family, by name. A new plugin that this misses would be
  ## gated with no floor and report a plausible Stain Index.
  for (m in c("gate_flowmeans_costain", "gate_mindensity_costain", "gate_auto_costain_dip",
              "gate_auto_costain_gmm", "gate_auto2_costain"))
    chk(sprintf("%s requires a Costain well", m), isTRUE(needs(gt(m))))
  chk("a preprocessing_method alone is enough (pp_costain_floor under a plain method)",
      isTRUE(needs(gt("gate_flowmeans", "pp_costain_floor"))))
  chk("...and pp_auto2_costain likewise", isTRUE(needs(gt("gate_flowmeans", "pp_auto2_costain"))))
  chk("matching is case-insensitive", isTRUE(needs(gt("GATE_FLOWMEANS_COSTAIN"))))
  ## THE RELAXING CASE. A whole ordinary template: scatter, singlets, viability, marker.
  plain <- data.frame(alias = c("Cells", "Single_cells", "Live", "Reagent"),
                      gating_method = c("boundary", "singletGate", "gate_flowclust_2d",
                                        "gate_flowmeans"),
                      preprocessing_method = NA_character_, stringsAsFactors = FALSE)
  chk("a template with no *_costain method needs no Costain well", isFALSE(needs(plain)))
  ## A commented-out example must not pin the requirement on -- the shipped TEMPLATE carries
  ## several #Reagent rows naming costain methods, and the app passes the RAW sheet.
  withhash <- rbind(plain, data.frame(alias = "#Reagent", gating_method = "gate_mindensity_costain",
                                      preprocessing_method = "pp_costain_floor"))
  chk("a commented-out #Reagent example does NOT pin the requirement on", isFALSE(needs(withhash)))
  chk("...but the same row uncommented does",
      isTRUE(needs(rbind(plain, data.frame(alias = "Reagent2",
                                           gating_method = "gate_mindensity_costain",
                                           preprocessing_method = "pp_costain_floor")))))
  ## FAILS CLOSED. Each of these is a template we cannot read; none may relax the rule.
  chk("NULL template fails closed (still required)", isTRUE(needs(NULL)))
  chk("a zero-row template fails closed", isTRUE(needs(plain[0, , drop = FALSE])))
  chk("a template with neither method column fails closed",
      isTRUE(needs(data.frame(alias = "Reagent", dims = "FITC-A", stringsAsFactors = FALSE))))
  chk("a non-data-frame fails closed", isTRUE(needs(list(gating_method = "gate_flowmeans"))))
  chk("a template whose rows are ALL commented out fails closed",
      isTRUE(needs(data.frame(alias = c("#a", "#b"), gating_method = "gate_flowmeans",
                              stringsAsFactors = FALSE))))
})

## And the layout check must actually honour the flag -- the helper being right is half of it.
local({
  mk <- function(types, qty) data.frame(
    Dirname = "X", Alias = "X", PlateID = 1,
    Row = LETTERS[seq_along(types)], Column = 1,
    StainType = types, StainQty = qty, stringsAsFactors = FALSE)
  res <- function(types, qty, req) {
    n <- character(0)
    r <- withCallingHandlers(
      h$validateLayoutGroups(mk(types, qty), "X", stop_on_error = FALSE, return_all = TRUE,
                             costain_required = req),
      warning = function(w) { n <<- c(n, conditionMessage(w)); invokeRestart("muffleWarning") })
    list(fatal = length(r$fatal), warn = length(n))
  }
  none <- c("Stain","Stain","Stain");            noneq <- c(1,2,3)
  two  <- c("Stain","Stain","Costain","Costain"); twoq <- c(1,2,0,0)
  chk("costain_required=FALSE: no Costain well is accepted silently",
      { a <- res(none, noneq, FALSE); a$fatal == 0L && a$warn == 0L })
  chk("costain_required=FALSE: two Costain wells are accepted silently",
      { b <- res(two, twoq, FALSE); b$fatal == 0L && b$warn == 0L })
  chk("costain_required=TRUE still fails on zero", res(none, noneq, TRUE)$fatal == 1L)
  chk("costain_required=TRUE still fails on two",  res(two,  twoq, TRUE)$fatal == 1L)
  ## The relaxation must not take the OTHER layout rules with it.
  chk("duplicate StainQty is still fatal with costain_required=FALSE",
      res(c("Stain","Stain","Stain"), c(1,1,3), FALSE)$fatal == 1L)
  chk("...and two Unstained wells still warn",
      res(c("Stain","Unstained","Unstained"), c(1,0,0), FALSE)$warn == 1L)
})

## End to end through the pre-flight the script and the app both call, because that is the only
## path a real run takes. Includes the DISCRIMINATING case: relaxing must not leak into a
## template that does read the floor.
local({
  md <- function(types, qty) data.frame(
    Dirname = "X", Alias = "X", Row = LETTERS[seq_along(types)], Column = 1, PlateID = 1,
    StainType = types, StainQty = qty, Fix = "N", Perm = "N", ELN_Name = "n", ELN_ID = "i",
    Operator = "o", Markername = "CD3", stringsAsFactors = FALSE)
  rec <- function(types, qty, meth, pp = NA_character_) {
    gt <- data.frame(alias = c("Cells", "Reagent"), pop = c("+", "+/-"), parent = "root",
                     dims = c("FSC-A,SSC-A", "CD3,FSC-A"),
                     gating_method = c("boundary", meth),
                     preprocessing_method = c(NA_character_, pp), stringsAsFactors = FALSE)
    r <- h$actaPrevalidateRecords(md(types, qty), gt, NULL, "X", tempfile(),
                                  marker_col = "Markername")
    Filter(function(x) x$id == "layout_groups", r)[[1]]
  }
  noco <- c("Stain","Stain","Stain"); q <- c(1,2,3)
  a <- rec(noco, q, "gate_flowmeans")
  chk("pre-flight PASSES a Costain-free layout under a plain template", a$status == "pass")
  ## The label may still SAY "Costain" -- it explains why none was required. What it must not do
  ## is claim the check was made, which is what an analyst reads it as.
  chk("...and its label does not claim a Costain check it did not make",
      !grepl("exactly one Costain", a$label, fixed = TRUE) &&
        grepl("no gating method reads a Costain floor", a$label, fixed = TRUE))
  b <- rec(noco, q, "gate_mindensity_costain", "pp_costain_floor")
  chk("pre-flight still FAILS the same layout under a *_costain template", b$status == "fail")
  chk("...and the label says what was required", grepl("Costain", b$label, fixed = TRUE))
  cc <- rec(c("Stain","Stain","Costain"), c(1,2,0), "gate_mindensity_costain", "pp_costain_floor")
  chk("...and passes once the Costain well is present", cc$status == "pass")
})

## ---- the Costain floor must actually be computable ---------------------------------------------
## MEASURED, not hypothetical. On 2026-10-31 a Reagent row reading `gate_flowmeans_costain` with
## preprocessing_method left blank ran OQ_Test1 to completion with ok = TRUE and put 50.7% of the
## co-stain above the marker gate, where the floored run puts 1.0%. max(bound, -Inf) is bound, so
## the method silently became the un-floored one underneath it and still produced a Stain Index
## table that looks entirely normal. These assertions are what stop that reaching a plate again.
cat("\n=== the Costain floor is computable, or the run stops ===\n")
local({
  f  <- h$actaCostainArgIssues
  mk <- function(meth, pp = NA_character_, args = NA_character_, alias = "Reagent")
    data.frame(alias = alias, gating_method = meth, preprocessing_method = pp,
               preprocessing_args = args, stringsAsFactors = FALSE)
  nf <- function(...) length(f(mk(...))$fatal)
  nn <- function(...) length(f(mk(...))$notes)

  ## NO CHURN ON SHIPPED CONTENT. If this ever fails, the check has become stricter than the
  ## workbooks ACTA itself ships -- which would make every OQ case fail its own pre-flight.
  for (wb in c("inst/pipeline/Template/TEMPLATE_Ab_Titration_Instructions.xlsx",
               "inst/extdata/oq_small/OQ_Test1/OQ_Test1_Ab_Titration_Instructions.xlsx",
               "inst/extdata/oq_small/OQ_Test2/OQ_Test2_Ab_Titration_Instructions.xlsx",
               "inst/extdata/oq_small/OQ_Test3/OQ_Test3_Ab_Titration_Instructions.xlsx")) {
    pth <- file.path(ACTA_REPO_ROOT, wb)
    if (!file.exists(pth)) { cat("  [skip] not in this layout:", basename(wb), "\n"); next }
    gt <- suppressMessages(readxl::read_excel(pth, sheet = "gating_template", col_types = "text"))
    chk(sprintf("shipped %s passes unchanged", basename(dirname(pth))), length(f(gt)$fatal) == 0L)
  }

  ## THE DEFECT ITSELF.
  chk("a *_costain method with NO preprocessing_method is fatal",
      nf("gate_flowmeans_costain") == 1L)
  chk("...and the message names the row, the method and the fix", {
    m <- f(mk("gate_flowmeans_costain"))$fatal[[1]]
    grepl("row 2", m) && grepl("gate_flowmeans_costain", m, fixed = TRUE) &&
      grepl("pp_costain_floor", m, fixed = TRUE) && grepl("-Inf", m, fixed = TRUE) })
  chk("...for every member of the family", all(vapply(
    c("gate_flowmeans_costain", "gate_mindensity_costain", "gate_auto_costain_dip",
      "gate_auto_costain_gmm", "gate_auto2_costain"), function(m) nf(m) == 1L, logical(1))))
  chk("a *_costain method with a NON-costain preprocessing_method is fatal",
      nf("gate_flowmeans_costain", "pp_something_else", "x=1") == 1L)

  ## REQUIRED vs OPTIONAL, derived from the plugin's formals rather than a list.
  spec <- get(".actaPpArgSpec", envir = h)
  chk("costain_quantile is REQUIRED (its formal default is a bare NA)",
      "costain_quantile" %in% spec("pp_costain_floor")$required)
  chk("sat_threshold is NOT required (it has a real default)",
      !("sat_threshold" %in% spec("pp_auto2_costain")$required) &&
        "sat_threshold" %in% spec("pp_auto2_costain")$all)
  chk("an unresolvable plugin name yields no spec", is.null(spec("pp_not_a_function")))

  chk("pp_costain_floor without costain_quantile is fatal",
      nf("gate_flowmeans_costain", "pp_costain_floor", "") == 1L)
  chk("pp_auto2_costain without costain_quantile is fatal",
      nf("gate_auto2_costain", "pp_auto2_costain", "sat_threshold=0.90, dip_pval=0.05") == 1L)
  ## The discriminating case: optional args must NOT be demanded, or every shipped row fails.
  chk("...but omitting the args that DO have defaults is fine",
      nf("gate_auto2_costain", "pp_auto2_costain", "costain_quantile=0.99") == 0L)
  chk("a complete row is clean",
      nf("gate_flowmeans_costain", "pp_costain_floor", "costain_quantile=0.99") == 0L)

  ## A TYPO IS NEVER INTENTIONAL. Every plugin takes `...`, so this is swallowed in silence.
  chk("an argument name the plugin does not accept is fatal", {
    r <- f(mk("gate_flowmeans_costain", "pp_costain_floor", "costain_quantil=0.99"))$fatal
    length(r) == 2L && any(grepl("does not accept", r)) })
  chk("...and the message lists what IS accepted",
      any(grepl("costain_quantile", f(mk("gate_flowmeans_costain", "pp_costain_floor",
                                         "costain_quantil=0.99"))$fatal, fixed = TRUE)))
  chk("a preprocessing_method naming no function is fatal, and SAYS so", {
    r <- f(mk("gate_flowmeans_costain", "pp_costain_flooor", "costain_quantile=0.99"))$fatal
    ## Counting fatals is not enough here: delete the branch and every argument reads as
    ## unknown instead, so the count stays 1 while the diagnosis becomes nonsense.
    length(r) == 1L && grepl("names no known function", r[[1]], fixed = TRUE) })

  ## A floor computed and thrown away is a note, not a failure -- the numbers are unaffected.
  chk("costain preprocessing on a method that ignores it is a NOTE",
      nf("gate_flowmeans", "pp_costain_floor", "costain_quantile=0.99") == 0L &&
        nn("gate_flowmeans", "pp_costain_floor", "costain_quantile=0.99") == 1L)

  ## Rows the pipeline ignores, this must ignore too.
  chk("commented-out #rows are ignored",
      nf("gate_flowmeans_costain", alias = "#Reagent") == 0L)
  chk("a row with no gating_method is ignored", nf("") == 0L)
  chk("an absent gating_method column is not an error",
      length(f(data.frame(alias = "Reagent", stringsAsFactors = FALSE))$fatal) == 0L)
  chk("NULL / empty templates are not this check's business",
      length(f(NULL)$fatal) == 0L && length(f(mk("x")[0, , drop = FALSE])$fatal) == 0L)

  ## NO WORKBOOK CELL IS EVER EVALUATED -- the rule test_arg_safety.R owns. Reading argument
  ## NAMES must not force the values, or a pre-flight becomes the very execution path that file
  ## was written to close.
  nm <- get(".actaArgNames", envir = h)
  boom <- "costain_quantile=stop('the pre-flight evaluated a workbook cell')"
  chk("argument names are read without evaluating the values",
      identical(tryCatch(nm(boom), error = function(e) "EVALUATED"), "costain_quantile"))
  chk("...and the whole check survives such a cell",
      length(f(mk("gate_flowmeans_costain", "pp_costain_floor", boom))$fatal) == 0L)
  ## The comma matters: `(^|,)` already rejects an inner `=` with no comma before it, so a probe
  ## without one passes even with the quote-blanking deleted.
  chk("a quoted value containing ', name=' is not mistaken for an argument name",
      identical(nm("costain_quantile=0.99, cluster='x, y=z'"), c("costain_quantile", "cluster")))
  chk("...and the same inside double quotes",
      identical(nm('costain_quantile=0.99, cluster="x, y=z"'), c("costain_quantile", "cluster")))
})

## And it has to reach the operator through the same pre-flight the script and app call.
local({
  md <- data.frame(Dirname = "X", Alias = "X", Row = c("A","B","C"), Column = 1, PlateID = 1,
                   StainType = c("Stain","Stain","Costain"), StainQty = c(1,2,0),
                   Fix = "N", Perm = "N", ELN_Name = "n", ELN_ID = "i", Operator = "o",
                   Markername = "CD3", stringsAsFactors = FALSE)
  rec <- function(pp, args) {
    gt <- data.frame(alias = c("Cells","Reagent"), pop = c("+","+/-"), parent = "root",
                     dims = c("FSC-A,SSC-A","CD3,FSC-A"),
                     gating_method = c("boundary","gate_flowmeans_costain"),
                     preprocessing_method = c(NA_character_, pp),
                     preprocessing_args = c(NA_character_, args), stringsAsFactors = FALSE)
    r <- h$actaPrevalidateRecords(md, gt, NULL, "X", tempfile(), marker_col = "Markername")
    Filter(function(x) x$id == "costain_preprocessing", r)[[1]]
  }
  chk("the pre-flight FAILS a costain row with no preprocessing_method",
      rec(NA_character_, NA_character_)$status == "fail")
  chk("the pre-flight FAILS a costain row with no costain_quantile",
      rec("pp_costain_floor", "")$status == "fail")
  chk("...and PASSES a complete one", rec("pp_costain_floor", "costain_quantile=0.99")$status == "pass")
})

cat(if (ok) "\nALL PRE-FLIGHT TESTS PASS\n" else "\nFAILURES ABOVE\n")
quit(status = if (ok) 0 else 1)
