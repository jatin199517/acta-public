## Test_Material_Status: a Layout_Plate column, and the TiterPubs field it publishes.
##
## WHY THIS NEEDS A GATE OF ITS OWN. The column is the first per-well answer to a question the
## submission template asks once -- "Fresh or Frozen" was read from Info, so one plate published
## one value for every reagent on it. Three things have to agree for the new path to mean
## anything, and each can be broken without the other two noticing:
##
##   1. THE COLUMN EXISTS, in every shipped workbook, NEXT TO Test_Material. Position is not
##      cosmetic here: the operator reads the material and the state it was in together, and the
##      export's lead block mirrors the sheet.
##   2. THE VALUE MAPS ONTO THEIR MENU. `Frozen` is one word; the menu holds four sentences, each
##      naming the thaw state AND whether the cells were cultured. An unmapped value is not an
##      error anywhere -- it is published into the off-menu overflow row, quietly.
##   3. AN OLD EXPORT STILL PUBLISHES WHAT IT USED TO. The column did not exist before 3.6.0, so
##      a row without it has to fall back to Info!Fresh_or_Frozen rather than go blank.

ok <- TRUE
chk <- function(label, pass, detail = NULL) {
  r <- tryCatch(list(v = isTRUE(pass), d = NULL),
                error = function(e) list(v = FALSE, d = paste("ERROR:", conditionMessage(e))))
  d <- r$d %||% tryCatch(paste(as.character(detail), collapse = " "), error = function(e) "")
  if (!isTRUE(r$v)) { ok <<- FALSE
    cat(sprintf("  [FAIL] %s%s\n", label, if (nzchar(d)) paste0(" -- ", d) else "")) }
  else cat(sprintf("  [ok]   %s\n", label))
}
`%||%` <- function(a, b) if (is.null(a)) b else a

source(file.path(this.path::this.dir(), "acta_test_paths.R"))
suppressMessages(library(ACTA))

## ---- 1. the shipped workbooks carry it, beside Test_Material, defaulted ------------------------
WBS <- c("inst/pipeline/Template/TEMPLATE_Ab_Titration_Instructions.xlsx",
         "inst/extdata/oq_small/OQ_Test1/OQ_Test1_Ab_Titration_Instructions.xlsx",
         "inst/extdata/oq_small/OQ_Test2/OQ_Test2_Ab_Titration_Instructions.xlsx",
         "inst/extdata/oq_small/OQ_Test3/OQ_Test3_Ab_Titration_Instructions.xlsx")
for (wb in WBS) {
  p <- file.path(ACTA_REPO_ROOT, wb)
  if (!file.exists(p)) { cat("  [skip] not in this layout:", basename(wb), "\n"); next }
  lp <- suppressMessages(readxl::read_excel(p, sheet = "Layout_Plate", col_types = "text"))
  i <- match("Test_Material_Status", names(lp)); j <- match("Test_Material", names(lp))
  chk(sprintf("%s has Test_Material_Status", basename(dirname(p))), !is.na(i))
  if (is.na(i) || is.na(j)) next
  chk("...immediately after Test_Material", i == j + 1L, sprintf("%d vs %d", i, j))
  ## Filled exactly where Test_Material is: a blank plate row stays blank in BOTH.
  chk("...filled on every row that names a test material",
      identical(is.na(lp[[i]]), is.na(lp[[j]])),
      sprintf("%d vs %d filled", sum(!is.na(lp[[i]])), sum(!is.na(lp[[j]]))))
  chk("...and defaults to Frozen",
      length(unique(stats::na.omit(lp[[i]]))) == 1L &&
        identical(unique(stats::na.omit(lp[[i]]))[[1]], "Frozen"),
      paste(unique(stats::na.omit(lp[[i]])), collapse = "/"))
}

## ---- 2. the export keeps the two columns together ----------------------------------------------
lead <- ACTA:::ACTA_EXPORT_LEAD_COLS
chk("the export lead block puts Test_Material_Status straight after Test_Material",
    match("Test_Material_Status", lead) == match("Test_Material", lead) + 1L,
    paste(lead[seq(max(1, match("Test_Material", lead) - 1), length.out = 3)], collapse = " / "))
## An older workbook has no such column, and the export's lead block is a FIXED schema -- the
## script fills it with NA rather than letting the column vanish. Asserted on the source because
## the alternative is a full pipeline run.
scr <- readLines(system.file("pipeline/ACTA_Script.R", package = "ACTA"), warn = FALSE)
chk("the pipeline emits it blank when the workbook predates it",
    any(grepl('if (!"Test_Material_Status" %in% names(stats))', scr, fixed = TRUE)))

## ---- 3. the mapping onto their menu ------------------------------------------------------------
tpl   <- ACTA:::actaTiterPubsTemplate()
menus <- if (nzchar(tpl) && file.exists(tpl)) ACTA:::actaTiterPubsMenus(tpl) else list()
FIELD <- "Fresh or Frozen"
opts  <- menus[[FIELD]]
chk("the submission template still has a 'Fresh or Frozen' menu to map onto", length(opts) > 0L,
    length(opts))

INFO <- data.frame(Fresh_or_Frozen = "Frozen, thawed and incubated with antibody",
                   stringsAsFactors = FALSE)
pub <- function(status) {
  row <- data.frame(Markername = "CD4", Alias = "CD4", stringsAsFactors = FALSE)
  if (!is.null(status)) row$Test_Material_Status <- status
  unname(ACTA:::actaTiterPubsValues(row, INFO, menus = menus)[[FIELD]])
}

CASES <- list(
  c("Frozen",           "Frozen, thawed and incubated with antibody"),
  c("frozen",           "Frozen, thawed and incubated with antibody"),   # case-folded
  c("Thawed",           "Frozen, thawed and incubated with antibody"),
  c("Frozen, cultured", "Frozen, thawed, cultured then incubated with antibody"),
  c("Fresh",            "Fresh, never frozen, incubated with antibody day of collection"),
  c("Fresh, cultured",  "Fresh, never frozen, cultured then incubated with antibody"))
for (cs in CASES) {
  got <- pub(cs[[1]])
  chk(sprintf("'%s' publishes the right sentence", cs[[1]]), identical(got, cs[[2]]), got)
  ## THE POINT OF THE ALIAS TABLE: an unmapped value is not rejected, it is published into the
  ## overflow row. So being ON the menu is the assertion, not merely being non-empty.
  if (length(opts))
    chk("...and it is on their menu, not in the overflow row",
        ACTA:::.tpOnMenu(got, FIELD, menus), got)
}

## A value already typed as one of their sentences is passed through, not blanked -- .tpAlias(),
## not .tpAliasStrict(). The off-menu escape hatch is a designed feature of their template.
long <- "Frozen, thawed, cultured then incubated with antibody"
chk("a full menu sentence in the layout passes through unchanged", identical(pub(long), long),
    pub(long))

## ---- 4. the Info fallback, which is what every archived export relies on -----------------------
chk("a row WITHOUT the column falls back to Info!Fresh_or_Frozen",
    identical(pub(NULL), INFO$Fresh_or_Frozen), pub(NULL))
chk("...and so does a row whose cell is blank", identical(pub(""), INFO$Fresh_or_Frozen), pub(""))
chk("...and one holding the literal string NA", identical(pub("NA"), INFO$Fresh_or_Frozen),
    pub("NA"))
## With neither, the field is empty rather than invented.
chk("with no column and no Info value the field is blank",
    is.na(unname(ACTA:::actaTiterPubsValues(data.frame(Markername = "CD4"),
                                            data.frame(x = 1), menus = menus)[[FIELD]])))

cat(if (ok) "\nSAMPLE STATUS TESTS PASSED\n" else "\nFAILURES ABOVE\n")
quit(status = if (ok) 0L else 1L)
