## .tpInvByCatalog(): the titrated reagent's inventory row, found by catalogue.
##
## THE DEFECT (shipped in the 2026-11-12 bundles). The lookup indexed a data frame with a BARE
## LOGICAL VECTOR. A row whose `catalog` cell is blank normalises to NA, so the comparison yields
## NA for that row -- and `df[c(NA, TRUE), ]` does not drop the NA, it returns an ALL-NA ROW in its
## place. `hit[1, ]` then reads the phantom row whenever the catalogue-less row sorts first, and
## the reagent publishes blank Clonality / Isotype / Antibody host species. Silently: a row WAS
## found, so nothing downstream could tell.
##
## Observed on CD7 / CD19 / CD56 (OQ_Test2) and on an HLA-DR run.
##
## ROW ORDER IS THE WHOLE DEFECT. A fixture with the blank row LAST passes against the broken
## code. So the fixture here puts it FIRST, and an explicit control below proves the fixture is
## genuinely adversarial -- if someone reorders it, the control fails and says why.

source(file.path(this.path::this.dir(), "acta_test_paths.R"))
ok <- TRUE
chk <- function(label, pass, detail = NULL) {
  d <- paste(as.character(detail), collapse = " ")
  if (!isTRUE(pass)) { ok <<- FALSE
    cat(sprintf("  [FAIL] %s%s\n", label, if (nzchar(d)) paste0(" -- ", d) else "")) }
  else cat(sprintf("  [ok]   %s\n", label))
}
## THIS CHECKOUT's sources, not the installed namespace -- see the note in test_strip_label.R. The
## first draft used `ACTA:::`, which grades whatever ACTA is installed on the machine rather than
## the tree under test.
f <- actaTestFunctions()$.tpInvByCatalog

## An inventory shaped like the real one. The CATALOGUE-LESS ROW IS FIRST, which is the hazard.
mkinv <- function(blank_first = TRUE) {
  rows <- list(
    data.frame(antigen = "Buffer", fluorochrome = NA_character_, vendor = "Inhouse",
               catalog = NA_character_, clone = NA_character_, clonality = NA_character_,
               isotype = NA_character_, host = NA_character_, type = "base",
               stringsAsFactors = FALSE),
    data.frame(antigen = "CD19", fluorochrome = "Alexa Fluor 660", vendor = "BioLegend",
               catalog = "302234", clone = "HIB19", clonality = "Monoclonal",
               isotype = "IgG1κ", host = "Mouse", type = "base",
               stringsAsFactors = FALSE))
  do.call(rbind, if (blank_first) rows else rev(rows))
}

cat("\n-- 0. the TITRATED reagent is findable, and it has no panel iteration --\n")
## REPORTED BY THE USER, 2026-11-20, correcting me. actaPanelInventory() drops every row with no
## `Panel Iteration` -- right for resolving a co-stain panel, wrong for this lookup, because the
## titrated reagent is BY DEFINITION not in its own co-stain panel and so correctly has no
## iteration. The filtered frame therefore could never contain the one reagent this function
## exists to describe.
##
## MEASURED on an HLA-DR titration: the declared co-stain panel resolved to its six co-stains
## exactly as intended, while HLA-DR itself carried Clonality and isotype in the master and
## published both BLANK. I had
## suggested giving that row an iteration; that would have put the titrated reagent into its own
## co-stain panel -- wrong data to work around an ACTA defect.
##
## The reader now carries the unfiltered rows as an attribute and this lookup prefers them.
local({
  .f <- actaTestFunctions()
  mk <- function() {
    all <- data.frame(
      antigen = c("CD3", "HLA-DR"), fluorochrome = c("BUV805", "BUV395"),
      vendor = c("BD", "BD"), catalog = c("612893", "568647"),
      clone = c("SK7", "L243"), clonality = c("Monoclonal", "Monoclonal"),
      isotype = c("Ms IgG1", "Mouse IgG2a, k"), host = c("Mouse", "Mouse"),
      type = c("base", "variable"),
      iteration = c("Iteration1", NA_character_),   # the titrated one has none
      stringsAsFactors = FALSE)
    panel <- all[!is.na(all$iteration), , drop = FALSE]
    attr(panel, "acta_inventory_all") <- all
    panel
  }
  inv <- mk()
  chk("the panel frame really does EXCLUDE the titrated reagent (the fixture is adversarial)",
      !("568647" %in% inv$catalog) && nrow(inv) == 1L)
  hit <- .f$.tpInvByCatalog(inv, "568647")
  chk("...and the lookup finds it anyway, via the unfiltered rows",
      !is.null(hit) && identical(hit$antigen[1], "HLA-DR"))
  chk("...with its clonality and isotype, which is what published blank",
      !is.null(hit) && identical(hit$clonality[1], "Monoclonal") &&
        identical(hit$isotype[1], "Mouse IgG2a, k"))
  chk("a co-stain row is still found too", !is.null(.f$.tpInvByCatalog(inv, "612893")))
  ## WITHOUT the attribute -- an inventory from an older reader -- it must still work on what it has.
  bare <- inv; attr(bare, "acta_inventory_all") <- NULL
  chk("no attribute -> falls back to the frame it was given, no error",
      is.null(.f$.tpInvByCatalog(bare, "568647")) &&
        !is.null(.f$.tpInvByCatalog(bare, "612893")))
})

cat("\n-- 1. the control: this fixture really does trigger the old defect --\n")
## The OLD expression, verbatim. If this stops returning an NA row, the fixture has lost its
## teeth and every assertion below is passing vacuously.
inv <- mkinv(TRUE)
k   <- function(z) gsub("[^a-z0-9]", "", tolower(as.character(z)))
old <- inv[k(inv$catalog) == k("302234"), , drop = FALSE]
chk("the bare-logical index yields an extra phantom row", nrow(old) == 2L, nrow(old))
chk("...and the FIRST row of it is all-NA (what hit[1,] used to read)",
    all(is.na(old[1, ])), paste(unlist(old[1, ]), collapse = "/"))

cat("\n-- 2. the fix: the real row is returned, blank row first --\n")
h <- f(inv, "302234")
chk("a row is returned", !is.null(h) && nrow(h) == 1L)
chk("it is the REAL row, not the phantom", identical(h$catalog[1], "302234"))
chk("Clonality is populated",  identical(h$clonality[1], "Monoclonal"))
chk("Isotype is populated",    identical(h$isotype[1], "IgG1κ"))
chk("host species is populated", identical(h$host[1], "Mouse"))
chk("no NA survived into the returned row's key", !is.na(h$catalog[1]))

cat("\n-- 3. order must not matter (it did) --\n")
h2 <- f(mkinv(FALSE), "302234")
chk("same answer with the blank row LAST", identical(h2$clonality[1], "Monoclonal"))
chk("blank-first and blank-last agree", identical(h$catalog[1], h2$catalog[1]))

cat("\n-- 4. several catalogue-less rows, and one before AND after --\n")
blank <- mkinv(TRUE)[1, , drop = FALSE]
inv3  <- rbind(blank, blank, mkinv(TRUE)[2, , drop = FALSE], blank)
h3 <- f(inv3, "302234")
chk("still finds the one real row", !is.null(h3) && identical(h3$clonality[1], "Monoclonal"))

cat("\n-- 5. normalisation still applies --\n")
chk("case and punctuation are ignored", identical(f(inv, " 302-234 ")$clone[1], "HIB19"))
chk("a different catalogue does NOT match", is.null(f(inv, "999999")))

cat("\n-- 6. it still declines where it should --\n")
chk("no inventory -> NULL",        is.null(f(NULL, "302234")))
chk("empty inventory -> NULL",     is.null(f(inv[0, , drop = FALSE], "302234")))
chk("blank catalogue -> NULL",     is.null(f(inv, "")))
chk("NA catalogue -> NULL",        is.null(f(inv, NA_character_)))
## An inventory in which EVERY catalogue is blank must return nothing, not a phantom row.
allblank <- rbind(blank, blank)
chk("an all-blank inventory -> NULL", is.null(f(allblank, "302234")))

cat(if (ok) "\nINV BY CATALOG TESTS PASSED\n" else "\nFAILURES ABOVE\n")
quit(status = if (ok) 0L else 1L)
