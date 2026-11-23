## The panel inventory ACTA WRITES must be able to stand in for the one it READ.
##
## TWO DEFECTS AND ONE DESIGN CHANGE, all measured on OQ_Test3 before the fix:
##
##  1. `.tpWriteInventory()` emitted an EIGHTEEN-NAME LITERAL, seven of which nothing ever filled
##     and nothing ever read -- `Supplier Cat`, `Size`, `Flow Cytometry Assay`, `Easy Panel name`,
##     `Easy Panel ID`, `Subset Panel?`, `Storage Temperature`. Six were one lab's own fields. An
##     always-empty column in a published artefact reads as "ACTA looked and found nothing", which
##     is a different claim from "ACTA has no opinion".
##  2. `actaPanelInventory()` discarded every column outside ACTA_PI_COLS, so a lab's own fields
##     could not survive a round trip even in principle.
##  3. `.fromInv()` filled a hand-picked SIX. `Fluorochrome_Alias` is in ACTA_PI_COLS -- ACTA READS
##     it to match reagents -- and nothing ever wrote it back.
##
## THE RULE NOW: known columns are normalised and used; EVERYTHING ELSE IS CARRIED VERBATIM. There
## is deliberately NO deny-list of column names anywhere, so a lab that keeps a column the shipped
## template no longer offers gets it back. Section 2.2 is that assertion and it must not be relaxed.
##
## ACTA asserts nothing about the CONTENTS of a carried column. Withholding a site's own fields
## from a published artefact is site policy, not tool behaviour.

source(file.path(this.path::this.dir(), "acta_test_paths.R"))
ok <- TRUE
chk <- function(label, pass, detail = NULL) {
  d <- paste(as.character(detail), collapse = " ")
  if (!isTRUE(pass)) { ok <<- FALSE
    cat(sprintf("  [FAIL] %s%s\n", label, if (nzchar(d)) paste0(" -- ", d) else "")) }
  else cat(sprintf("  [ok]   %s\n", label))
}
## THIS CHECKOUT's sources, not the installed namespace.
FN   <- actaTestFunctions()
piFn <- FN$actaPanelInventory
wrFn <- FN$.tpWriteInventory
PI   <- FN$ACTA_PI_COLS
blank <- function(x) is.na(x) | !nzchar(trimws(as.character(x)))
tmp  <- file.path(tempdir(), sprintf("invcarry_%s", basename(tempfile(""))))
dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

## THE SEVEN THAT LEFT THE TEMPLATE. Named here ONLY so the tests can prove they are no longer
## hardcoded. Nothing in ACTA may consult a list like this at run time.
GONE <- c("Supplier Cat", "Size", "Flow Cytometry Assay", "Easy Panel name",
          "Easy Panel ID", "Subset Panel?", "Storage Temperature")

cat("1. actaPanelInventory() keeps every column the lab keeps\n")
## THE FIXTURE IS WRITTEN WITH openxlsx, AND THAT CHOICE IS NOT ARBITRARY.
##
## This section first used writexl and went RED on both CI runners while passing here: writexl is
## not an ACTA dependency at all -- it survives only in two comments recording that openxlsx
## replaced it, because it cannot style cells -- so the fixture was inheriting whatever happened to
## be in the author's library. A fixture that depends on the machine is not a fixture, the same way
## a machine-dependent assertion is not an assertion.
##
## openxlsx is a hard Import in DESCRIPTION, so it is present wherever ACTA is installed and this
## section needs no guard and can never silently skip. Three other tests already build throwaway
## workbooks the same way. The standing "never openxlsx" rule is about a load/save ROUND TRIP over
## a workbook that already exists, which corrupts parts nobody touched; writing a new temporary
## file from a data frame is a different operation and nothing here is ever published.
{
  src <- file.path(tmp, "Master_Panel_Inventory.xlsx")
  ## check.names is off: the headers ARE the fixture. "...9" is what readxl calls a header-less
  ## column, "isotype" collides with an internal name, and "Storage Temperature" is one of GONE.
  d <- list(
    "Antigen"            = c("CD4", "CD8", "HLA-DR"),
    "Fluorochrome"       = c("BUV496", "AF532", "PE"),
    "Fluorochrome_Alias" = c("BUV 496", "Alexa 532", ""),
    "Catalog"            = c("612936", "58-0088-42", "568647"),
    "Species / Isotype"  = c("Mouse IgG1", "Mouse IgG1", "Mouse IgG2a"),
    "Panel Iteration"    = c("p1", "p1", ""),
    "Storage Temperature"= c("4C", "4C", "4C"),
    "Notes"              = c("n1", "", "n3"),
    "isotype"            = c("x", "x", "x"),
    "...9"               = c("z", "z", "z"))
  openxlsx::write.xlsx(list(Master = as.data.frame(d, check.names = FALSE)), src,
                       overwrite = TRUE)
  inv <- piFn(src)
  ex  <- attr(inv, "acta_inventory_extra")
  chk("1.1 a lab column is carried", "Notes" %in% ex, paste(ex, collapse = ","))
  chk("1.2 NO DENY-LIST: a stripped template name is carried like any other",
      "Storage Temperature" %in% ex, paste(ex, collapse = ","))
  chk("1.3 a header-less column ('...9') is not a field", !("...9" %in% ex))
  chk("1.4 a name colliding with an internal column is skipped, not renamed",
      !("isotype" %in% ex) && identical(inv$isotype, c("Mouse IgG1", "Mouse IgG1")),
      paste(inv$isotype, collapse = ","))
  chk("1.5 known columns still normalise to internal names",
      all(c("antigen", "fl_alias", "catalog") %in% names(inv)))
  chk("1.6 extras survive the subset (attribute set after `[.data.frame`)",
      all(c("Storage Temperature", "Notes") %in% names(inv)) && nrow(inv) == 2L)
  all_ <- attr(inv, "acta_inventory_all")
  chk("1.7 the unfiltered rows carry the extras too",
      !is.null(all_) && "Notes" %in% names(all_) && nrow(all_) == 3L)
}

cat("2. .tpWriteInventory() emits known + carried, and fills every known column\n")
## Frames rather than a workbook: the writer's contract is about an inventory OBJECT.
mkInv <- function(extra = c("Storage Temperature", "Notes")) {
  f <- data.frame(
    antigen = c("CD4", "CD8", "HLA-DR"), fluorochrome = c("BUV496", "AF532", "PE"),
    fl_alias = c("BUV 496", "Alexa 532", "PE-A"), marker_alias = c("CD4", "CD8", "HLA-DR"),
    vendor = c("BD", "eBio", "BD"), catalog = c("612936", "58-0088-42", "568647"),
    clone = c("SK3", "SK1", "G46-6"), clonality = c("Monoclonal", "Monoclonal", "Monoclonal"),
    isotype = c("Mouse IgG1", "Mouse IgG1", "Mouse IgG2a"), type = c("Base", "Base", "Variable"),
    ## THE TITRATED REAGENT'S ROW CARRIES AN ITERATION HERE ON PURPOSE -- the writer must still
    ## leave its `Panel Iteration` blank, or it publishes the reagent into its own co-stain panel.
    iteration = c("p1", "p1", "p9"), stringsAsFactors = FALSE)
  f[["Storage Temperature"]] <- c("4C", "4C", "-20C")
  f[["Notes"]] <- c("n1", "n2", "n3")
  attr(f, "acta_inventory_all")   <- f
  attr(f, "acta_inventory_extra") <- extra
  f
}
mkPanels <- function() {
  tb <- data.frame(Antigen = c("CD4", "CD8"), Fluorochrome = c("BUV496", "AF532"),
                   Matched_to = c("CD4", "CD8"), Verdict = "matched",
                   Catalog = c("612936", "58-0088-42"), Vendor = c("BD", "eBio"),
                   Clone = c("SK3", "SK1"), Type = "Base", stringsAsFactors = FALSE)
  list(p1 = list(table = tb))
}
STATS <- data.frame(Cat_Num = "568647", Alias = "HLA-DR", Vendor = "", Clone = "",
                    Lot_Num = "", stringsAsFactors = FALSE)
rd <- function(p) suppressMessages(readxl::read_excel(p, sheet = 1, col_types = "text"))

p1 <- file.path(tmp, "with_inv.xlsx")
msg <- capture.output(wrFn(mkPanels(), p1, stats = STATS, inv = mkInv()), type = "message")
w   <- rd(p1)
chk("2.1 the eleven known columns lead, in ACTA_PI_COLS order",
    identical(names(w)[seq_along(PI)], unname(PI)), paste(names(w), collapse = ","))
chk("2.2 NO DENY-LIST: a carried column named like a stripped one is emitted",
    "Storage Temperature" %in% names(w) && "Notes" %in% names(w))
chk("2.3 carried columns follow the known ones, in source order",
    identical(names(w)[-seq_along(PI)], c("Storage Temperature", "Notes")))
chk("2.4 Fluorochrome_Alias is filled -- read by ACTA, never written back before",
    sum(!blank(w$Fluorochrome_Alias)) == 3L, w$Fluorochrome_Alias)
chk("2.5 Clonality and the isotype are filled for the CO-STAIN rows",
    sum(!blank(w$Clonality)) == 3L && sum(!blank(w[["Species / Isotype"]])) == 3L)
ti <- which(!blank(w$Catalog) & w$Catalog == "568647")
chk("2.6 the titrated reagent got its own row", length(ti) == 1L)
chk("2.7 the titrated row's Panel Iteration stays BLANK although its inventory row has one",
    length(ti) == 1L && blank(w[["Panel Iteration"]][ti]), w[["Panel Iteration"]][ti])
chk("2.8 the titrated row's carried columns are filled from the inventory",
    length(ti) == 1L && identical(trimws(w[["Storage Temperature"]][ti]), "-20C"))
chk("2.9 the carried columns are NAMED, not counted",
    any(grepl("Storage Temperature", msg)) && any(grepl("Notes", msg)),
    paste(msg, collapse = " | "))

## A TITRATED REAGENT THAT IS NOT IN THE INVENTORY GETS A ROW AND BLANK INVENTORY FIELDS.
## Stated as a requirement 2026-11-22: identity comes from the OPERATOR'S WORKBOOK (`stats`), the
## reagent facts come from the master by catalogue, and a catalogue that is not there means BLANK
## -- never a guess, and never another row's values. The old `.tpInvByCatalog()` defect returned a
## phantom all-NA row, which looked like this; returning a WRONG row would not, so both are checked.
NOHIT <- data.frame(Cat_Num = "999-not-in-inventory", Alias = "CCR7", Vendor = "BioLegend",
                    Clone = "G043H7", Lot_Num = "", stringsAsFactors = FALSE)
p5 <- file.path(tmp, "nohit.xlsx")
wrFn(mkPanels(), p5, stats = NOHIT, inv = mkInv())
w5 <- rd(p5)
n5 <- which(!blank(w5$Catalog) & w5$Catalog == "999-not-in-inventory")
chk("2.12 an unmatched titrated reagent still gets its row", length(n5) == 1L, nrow(w5))
chk("2.13 its inventory-sourced fields are BLANK, not guessed and not another row's",
    length(n5) == 1L && blank(w5$Clonality[n5]) &&
      blank(w5[["Species / Isotype"]][n5]) && blank(w5$Fluorochrome[n5]),
    paste(w5$Clonality[n5], w5[["Species / Isotype"]][n5], w5$Fluorochrome[n5]))
chk("2.14 but what the WORKBOOK stated survives -- the row is blank, not empty",
    length(n5) == 1L && identical(trimws(w5$Antigen[n5]), "CCR7") &&
      identical(trimws(w5$Clone[n5]), "G043H7") && identical(trimws(w5$Type[n5]), "Variable"),
    paste(w5$Antigen[n5], w5$Clone[n5], w5$Type[n5]))
chk("2.15 CONTROL: the same writer DOES fill them when the catalogue matches, so 2.13 is
       testing the lookup and not a writer that fills nothing",
    !blank(w[["Species / Isotype"]][ti]) && !blank(w$Clonality[ti]))
## AND THE WRITE SAYS SO. This message is also what the operator sees at BUNDLE BUILD, the moment
## the blank becomes irrevocable on a CC BY 4.0 deposit.
m5 <- capture.output(wrFn(mkPanels(), file.path(tmp, "nohit2.xlsx"), stats = NOHIT, inv = mkInv()),
                     type = "message")
chk("2.16 the unmatched reagent is announced at write time, named",
    any(grepl("999-not-in-inventory", m5, fixed = TRUE)) && any(grepl("CCR7", m5, fixed = TRUE)),
    paste(m5, collapse = " | "))
chk("2.17 CONTROL: a run where everything matches says nothing about misses",
    !any(grepl("not in", msg, fixed = TRUE)), paste(msg, collapse = " | "))

p2 <- file.path(tmp, "no_inv.xlsx")
wrFn(mkPanels(), p2)
w2 <- rd(p2)
chk("2.10 with no inventory the file is exactly the shipped template's columns",
    identical(names(w2), unname(PI)), paste(names(w2), collapse = ","))
chk("2.11 none of the seven removed names appears unbidden",
    !any(GONE %in% names(w2)))

cat("3. the run-folder copy fills what the bundle's copy fills\n")
## THE MEASURED PAIR. Before the fix: bundle path `Species / Isotype` 7 of 7, run-folder path 0 of 7,
## from ONE source inventory. The run-folder file is the one ACTA tells the operator to point
## Info!master_panel_inventory_path at, so it being the thinner of the two is the wrong way round.
p3 <- file.path(tmp, "runfolder.xlsx")
wrFn(mkPanels(), p3, stats = STATS, inv = mkInv())
w3 <- rd(p3)
chk("3.1 isotype is filled when inv is passed",
    sum(!blank(w3[["Species / Isotype"]])) == 3L)
p4 <- file.path(tmp, "runfolder_bare.xlsx")
wrFn(mkPanels(), p4)
w4 <- rd(p4)
chk("3.2 CONTROL: without inv it is empty, so 3.1 is testing the wiring and not the fixture",
    all(blank(w4[["Species / Isotype"]])))

cat("4. the pipeline passes stats AND inv at the run-folder call site\n")
## STRUCTURAL, because every check above would pass against a script that stopped passing them.
## The behavioural tests grade the function; this grades the caller.
sf  <- file.path(ACTA_VERSION_DIR, "ACTA_Script.R")
if (!file.exists(sf)) sf <- file.path(ACTA_REPO_ROOT, "inst", "pipeline", "ACTA_Script.R")
calls <- list()
walk <- function(e) {
  if (is.call(e)) {
    f <- e[[1]]
    if (is.name(f) && as.character(f) == ".tpWriteInventory") calls[[length(calls) + 1L]] <<- e
    for (i in seq_along(e)) if (!is.null(e[[i]])) try(walk(e[[i]]), silent = TRUE)
  }
}
invisible(lapply(parse(sf), walk))
chk("4.1 the script calls .tpWriteInventory exactly once", length(calls) == 1L, length(calls))
if (length(calls) == 1L) {
  nm <- names(calls[[1]])
  chk("4.2 it passes inv=",   "inv"   %in% nm, paste(nm, collapse = ","))
  chk("4.3 it passes stats=", "stats" %in% nm, paste(nm, collapse = ","))
}
## ONE RESOLUTION, NOT TWO. Resolving the inventory a second time is how a bundle comes to
## disagree with the report shipped beside it -- the codebase's own stated reason for making the
## override an option rather than an argument. Counted, not read.
txt <- readLines(sf, warn = FALSE)
code <- txt[!grepl("^\\s*#", txt)]
chk("4.4 the script resolves the panel inventory exactly once",
    sum(grepl("actaPanelInventoryPath\\(", code)) == 1L &&
    sum(grepl("actaPanelInventory\\(", code)) == 1L,
    paste(sum(grepl("actaPanelInventoryPath\\(", code)),
          sum(grepl("actaPanelInventory\\(", code))))
## THE GATE ALIGNMENT. A run with titrated reagents but no resolved co-stain panel wrote an
## inventory into its BUNDLE and none into its RUN FOLDER. The writer's own guard (`:1874`) already
## dropped the panels requirement; this one did not.
cl <- grep("\\.tpWriteInventory\\(", txt)
if (length(cl) == 1L) {
  gate <- paste(txt[max(1, cl - 10):cl], collapse = " ")
  chk("4.5 the run-folder gate also fires on stats alone, like the bundle's guard",
      grepl("statsForExport", gate) && grepl("nrow\\(", gate) &&
        grepl("panelIterationInfo", gate), gate)
} else chk("4.5 one call site to read the gate from", FALSE, length(cl))

cat("5. the catalogue miss is detected and said in one sentence\n")
mf <- FN$actaCatalogMisses
cf <- FN$actaCatalogMissCaption
## SHAPED THE WAY actaPanelInventory() HANDS IT OVER, which mkInv() alone is not: the frame is
## FILTERED to rows carrying a Panel Iteration, and the unfiltered rows ride along as an attribute.
## The titrated reagent legitimately has no iteration, so ONLY the attribute can find it. Modelling
## that is the whole point -- against an unfiltered fixture 5.1 passes without testing anything,
## which is how the first draft of this section was green and worthless.
iv <- mkInv()
iv$iteration[3] <- ""                       # HLA-DR, 568647 -- the titrated one, as a master holds it
filt <- iv[nzchar(iv$iteration), , drop = FALSE]
attr(filt, "acta_inventory_all")   <- iv
attr(filt, "acta_inventory_extra") <- c("Storage Temperature", "Notes")
chk("5.1 a titrated catalogue the inventory holds is NOT a miss (found via the unfiltered rows)",
    length(mf(filt, data.frame(Cat_Num = "568647", Alias = "HLA-DR",
                               stringsAsFactors = FALSE))) == 0L)
m1 <- mf(filt, data.frame(Cat_Num = "999-nope", Alias = "CCR7", stringsAsFactors = FALSE))
chk("5.2 one the inventory lacks IS a miss, labelled for a human", identical(m1, "CCR7 (999-nope)"), m1)
chk("5.3 a BLANK catalogue is not a miss -- nothing was looked up",
    length(mf(filt, data.frame(Cat_Num = c("", NA, "NA"), Alias = "x",
                               stringsAsFactors = FALSE))) == 0L)
chk("5.4 formatting is not a miss: the key is normalised",
    length(mf(filt, data.frame(Cat_Num = "58 0088 42", Alias = "CD8",
                               stringsAsFactors = FALSE))) == 0L)
bare <- filt; attr(bare, "acta_inventory_all") <- NULL
chk("5.5 CONTROL: the filtered frame alone reports a FALSE miss, so 5.1 is load-bearing",
    length(mf(bare, data.frame(Cat_Num = "568647", Alias = "HLA-DR",
                               stringsAsFactors = FALSE))) == 1L)
cap <- cf(c("CCR7 (999-nope)"), "Master_Panel_Inventory.xlsx")
chk("5.6 the sentence names the reagent, the file, and what goes blank",
    grepl("CCR7 \\(999-nope\\)", cap) && grepl("Master_Panel_Inventory.xlsx", cap) &&
      grepl("clonality", cap) && grepl("Supplier Cat", cap), cap)
chk("5.7 no misses means no sentence", is.na(cf(character(0))))

cat("6. the pre-flight says it before the run, and can never be a fail\n")
rf <- FN$actaPanelInventoryRecords
chk("6.1 actaPanelInventoryRecords() accepts catalogs=", "catalogs" %in% names(formals(rf)))
af <- actaTestAppFile()
atxt <- paste(readLines(af, warn = FALSE), collapse = " ")
chk("6.2 the app passes the Layout_Plate catalogues to it",
    grepl('catalogs\\s*=\\s*layoutVals\\(layout,\\s*"Cat_Num"\\)', atxt))
## BEHAVIOURAL, against the real workbook section 1 wrote. 6.1/6.2 are structural and would both
## pass against a parameter accepted and ignored.
srcwb <- file.path(tmp, "Master_Panel_Inventory.xlsx")
if (!file.exists(srcwb)) {
  chk("6.3 a fixture workbook exists to drive the pre-flight", FALSE, "section 1 did not run")
} else {
  r1 <- rf(info = NULL, version_dir = tmp, iterations = "p1",
           catalogs = c("612936", "nope-999"))
  st <- vapply(r1, function(z) as.character(z$status)[1], "")
  id <- vapply(r1, function(z) as.character(z$id)[1], "")
  kk <- which(id == "panel_inventory_catalog")
  chk("6.3 the pre-flight raises the miss, as a warn",
      length(kk) == 1L && identical(st[kk], "warn"), paste(id, st, collapse = " | "))
  chk("6.4 and names the catalogue that was not found",
      length(kk) == 1L && grepl("nope-999", r1[[kk]]$detail, fixed = TRUE),
      if (length(kk)) r1[[kk]]$detail else "")
  chk("6.5 NOTHING in the panel is ever a fail -- provenance, not QC", !any(st == "fail"), st)
  r2 <- rf(info = NULL, version_dir = tmp, iterations = "p1", catalogs = "612936")
  k2 <- which(vapply(r2, function(z) as.character(z$id)[1], "") == "panel_inventory_catalog")
  chk("6.6 CONTROL: all-found reports pass, so 6.3 is not a record that always warns",
      length(k2) == 1L && identical(as.character(r2[[k2]]$status)[1], "pass"),
      if (length(k2)) r2[[k2]]$status else "")
  ## STAKES. The same miss on a plate that declares no co-stain panel and publishes no bundle
  ## costs nothing, and a panel that goes amber there is a panel that stops being read. This is
  ## the rule the file states and nothing tested it -- a mutation deleting `stakes` from the
  ## record SURVIVED the first draft of this section.
  r3 <- rf(info = NULL, version_dir = tmp, iterations = character(0), catalogs = "nope-999")
  k3 <- which(vapply(r3, function(z) as.character(z$id)[1], "") == "panel_inventory_catalog")
  chk("6.7 the SAME miss is only a pass when the run has nothing to lose",
      length(k3) == 1L && identical(as.character(r3[[k3]]$status)[1], "pass"),
      if (length(k3)) r3[[k3]]$status else "no record")
  chk("6.8 and it still SAYS so -- lower severity, same sentence",
      length(k3) == 1L && grepl("nope-999", r3[[k3]]$detail, fixed = TRUE))
}

cat(if (ok) "PASS\n" else "FAIL\n")
if (!ok) quit(status = 1)
