## Equipment: the $CYT -> model map, and the Info!Equipment_Make / Equipment_Model override.
##
## WHY BOTH EXIST. $CYT is meant to name the cytometer and only sometimes does. A Cytek Aurora
## writes "Aurora". A Bio-Rad ZE5 writes its ACQUISITION SOFTWARE -- "EVEREST" on this site's own
## files, and Bio-Rad documents "ZE5" (Everest 2.0+) and "YETI" (1.4 and earlier, the Propel Labs
## name the ZE5 shipped under). A BD/Diva file carries no $CYT at all. So the export published a
## software name as the equipment, and nothing for BD.
##
## Equipment is produced in ONE place -- fcsEquipment(), via annotate_changeParam_pdWrite() -- and
## feeds the export, the report, MiFlowCyt and the TiterPubs submission. That is why the override
## is applied there and not in four consumers.

ok <- TRUE
chk <- function(label, pass, detail = NULL) {
  d <- paste(as.character(detail), collapse = " ")
  if (!isTRUE(pass)) { ok <<- FALSE
    cat(sprintf("  [FAIL] %s%s\n", label, if (nzchar(d)) paste0(" -- ", d) else "")) }
  else cat(sprintf("  [ok]   %s\n", label))
}
suppressMessages(library(ACTA)); suppressMessages(library(flowCore))
fe <- ACTA:::fcsEquipment

## A frame carrying only the keywords under test.
frm <- function(cyt = NULL, sn = NULL) {
  ff <- flowCore::flowFrame(matrix(1, 1, 1, dimnames = list(NULL, "FSC-A")))
  if (!is.null(cyt)) ff@description[["$CYT"]]   <- cyt
  if (!is.null(sn))  ff@description[["$CYTSN"]] <- sn
  ff
}
quiet <- function(expr) withCallingHandlers(expr, warning = function(w) invokeRestart("muffleWarning"))
warned <- function(expr) { got <- FALSE
  withCallingHandlers(expr, warning = function(w) { got <<- TRUE; invokeRestart("muffleWarning") }); got }

cat("-- $CYT is mapped to the MODEL when no override is given --\n")
chk("Aurora passes through unchanged", identical(fe(frm("Aurora","U1")), "Aurora (U1)"))
chk("EVEREST resolves to the instrument, not the software",
    identical(fe(frm("EVEREST","SN0")), "ZE5 (SN0)"), fe(frm("EVEREST","SN0")))
chk("YETI (Everest 1.4 and earlier) also resolves to ZE5",
    identical(fe(frm("YETI","SN0")), "ZE5 (SN0)"), fe(frm("YETI","SN0")))
chk("ZE5 maps to itself", identical(fe(frm("ZE5","SN0")), "ZE5 (SN0)"))
chk("the map is case-insensitive", identical(fe(frm("everest","SN0")), "ZE5 (SN0)"))
chk("surrounding whitespace does not defeat the map",
    identical(fe(frm("  EVEREST ","SN0")), "ZE5 (SN0)"))
chk("an unmapped instrument passes through untouched",
    identical(fe(frm("CytoFLEX LX","SN0")), "CytoFLEX LX (SN0)"))

cat("-- Info!Equipment_Make / Equipment_Model override it --\n")
## $CYT is deliberately a DIFFERENT instrument in these, so a result of "Aurora ..." would mean
## the override had not been applied at all.
chk("make + model replace $CYT outright",
    identical(fe(frm("Aurora","U1"), "Bio-Rad", "ZE5"), "Bio-Rad ZE5 (U1)"),
    fe(frm("Aurora","U1"), "Bio-Rad", "ZE5"))
chk("model alone is enough", identical(fe(frm("Aurora","U1"), NA, "ZE5"), "ZE5 (U1)"))
chk("make alone is enough",  identical(fe(frm("Aurora","U1"), "Bio-Rad", NA), "Bio-Rad (U1)"))
chk("an empty string counts as UNFILLED, not as a name",
    identical(fe(frm("Aurora","U1"), "", ""), "Aurora (U1)"), fe(frm("Aurora","U1"), "", ""))
chk("whitespace-only counts as unfilled",
    identical(fe(frm("Aurora","U1"), "  ", NA), "Aurora (U1)"))

cat("-- the BD case: no $CYT at all --\n")
chk("a file with no $CYT and no override is NA",
    is.na(quiet(fe(frm(NULL, NULL)))))
chk("...and warns", warned(fe(frm(NULL, NULL))))
chk("an override rescues a file with no $CYT",
    identical(fe(frm(NULL, "SN0"), "BD Biosciences", "FACSymphony A5"),
              "BD Biosciences FACSymphony A5 (SN0)"))
chk("...and does NOT warn", !warned(fe(frm(NULL, "SN0"), "BD Biosciences", "FACSymphony A5")))
chk("no serial, override given -> name with no parenthesis",
    identical(fe(frm(NULL, NULL), "BD Biosciences", "FACSymphony A5"),
              "BD Biosciences FACSymphony A5"))
chk("serial but no name is still reported",
    identical(quiet(fe(frm(NULL, "SN0"))), "(SN0)"))

cat("-- the serial survives the override, and is still scrubbable --\n")
scrub <- ACTA:::actaScrubEquipmentSerial
chk("$CYTSN is kept when the name is overridden",
    grepl("(U1)", fe(frm("Aurora","U1"), "Bio-Rad", "ZE5"), fixed = TRUE))
chk("actaScrubEquipmentSerial still strips it",
    !grepl("U1", scrub(fe(frm("Aurora","U1"), "Bio-Rad", "ZE5")), fixed = TRUE),
    scrub(fe(frm("Aurora","U1"), "Bio-Rad", "ZE5")))

cat("-- the shipped workbooks ship it blank, so default behaviour is unchanged --\n")
iv  <- ACTA:::actaInfoValue
wbs <- c("inst/pipeline/Template/TEMPLATE_Ab_Titration_Instructions.xlsx",
         "inst/extdata/oq_small/OQ_Test1/OQ_Test1_Ab_Titration_Instructions.xlsx",
         "inst/extdata/oq_small/OQ_Test2/OQ_Test2_Ab_Titration_Instructions.xlsx",
         "inst/extdata/oq_small/OQ_Test3/OQ_Test3_Ab_Titration_Instructions.xlsx")
root <- if (dir.exists("ACTA_package")) "ACTA_package" else "."
for (w in wbs) {
  p <- file.path(root, w); if (!file.exists(p)) { chk(paste("workbook present:", basename(w)), FALSE); next }
  d <- as.data.frame(suppressMessages(readxl::read_excel(p, sheet = "Info"))); names(d) <- trimws(names(d))
  cat2 <- d$Category[!is.na(d$Variable) & d$Variable %in% c("Equipment_Make","Equipment_Model")]
  chk(sprintf("%-42s two blank Equipment rows", basename(w)),
      length(cat2) == 2L && all(cat2 == "Equipment") &&
      is.na(iv(d,"Equipment_Make")) && is.na(iv(d,"Equipment_Model")))
}

cat("-- the TiterPubs tables know YETI too --\n")
chk("instrument model", identical(unname(ACTA:::ACTA_TP_INSTRUMENT_MODEL[["YETI"]]), "ZE5"))
chk("manufacturer",     identical(unname(ACTA:::ACTA_TP_INSTRUMENT_MFR[["YETI"]]), "Bio-Rad"))
chk("collection software", identical(unname(ACTA:::ACTA_TP_COLLECTION_SW[["YETI"]]), "EVEREST"))

cat(if (ok) "\nEQUIPMENT OVERRIDE TESTS PASSED\n" else "\nFAILURES ABOVE\n")
quit(status = if (ok) 0L else 1L)
