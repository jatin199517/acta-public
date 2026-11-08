## Info!Tissue -> the TiterPubs "Tissue" column, and Info!Tissue_Fallback behind it.
##
## WHY THIS GATE EXISTS. `Tissue` sat in ACTA_TP_OMIT and was hard-blanked at the END of
## actaTiterPubsValues(), AFTER every other rule had run. So a mapping entry alone would have
## looked correct in the source and published nothing -- the two-writers-of-one-cell shape that
## has already cost this file a release. Every assertion here therefore goes through
## actaTiterPubsValues() and the real writer, and reads the value back out of a written workbook.
##
## The reason the column matters is PBMC: their Cell type menu has no PBMC option, so
## .tpCellType() publishes `mononuclear cells`, which could be marrow, cord blood, spleen or
## node. Tissue = blood is the half that makes it peripheral. Both halves on-menu beats one
## half plus free text, because only the menu values are facet-searchable.

ok <- TRUE
chk <- function(label, pass, detail = NULL) {
  d <- paste(as.character(detail), collapse = " ")
  if (!isTRUE(pass)) { ok <<- FALSE
    cat(sprintf("  [FAIL] %s%s\n", label, if (nzchar(d)) paste0(" -- ", d) else "")) }
  else cat(sprintf("  [ok]   %s\n", label))
}
suppressMessages({library(readxl); library(zip)})
source(file.path(this.path::this.dir(), "acta_test_paths.R"))
h <- actaTestFunctions(actaTestAppDir())
.tp <- function(nm) if (!is.null(h[[nm]])) h[[nm]] else getFromNamespace(nm, "ACTA")

tpl <- .tp("actaTiterPubsTemplate")()
if (!nzchar(tpl) || !file.exists(tpl)) {
  cat("  [skip] no TiterPubs template shipped\n")
} else {
  menus <- .tp("actaTiterPubsMenus")(tpl)

  ## A minimal but REAL call: build the Info sheet the way the workbook stores it (long form,
  ## Variable/Value), hand it to actaTiterPubsValues(), then canonicalise, split and WRITE.
  info_of <- function(...) {
    kv <- list(...)
    data.frame(Category = rep("TiterPubs", length(kv)),
               Variable = names(kv), Value = unlist(kv, use.names = FALSE),
               Description = rep("", length(kv)),
               stringsAsFactors = FALSE)
  }
  ## TISSUE IS PER-WELL SINCE 3.6.8, so it is supplied on the ROW, not the Info sheet. It was an
  ## Info cell for one afternoon; the user's call was that a tissue is a property of the sample,
  ## not of the run, and Cell_Type had always been per-well for the same reason.
  row_of <- function(tissue = NULL) {
    r <- data.frame(Markername = "CD3", Alias = "CD3", Cell_Type = "HD Human PBMCs",
                    stringsAsFactors = FALSE)
    if (!is.null(tissue)) r$Tissue_Type <- tissue
    r
  }

  published <- function(info, tissue = NULL) {
    v <- .tp("actaTiterPubsValues")(row = row_of(tissue), info = info, menus = menus)
    v <- .tp(".tpCanonicaliseAll")(v, menus)
    over <- names(v)[!vapply(names(v), function(f) .tp(".tpOnMenu")(v[[f]], f, menus), TRUE)]
    out <- file.path(tempdir(), paste0("tis_", basename(tempfile("")), ".xlsx"))
    dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
    .tp("actaTiterPubsWrite")(tpl, out, v, over)
    d <- as.data.frame(suppressMessages(readxl::read_excel(out, sheet = "to publish",
                                                            col_types = "text", n_max = 2)))
    list(row2 = d[["Tissue"]][1], row3 = d[["Tissue"]][2],
         celltype = d[["Cell type"]][1])
  }

  cat("\n=== Layout_Plate!Tissue_Type reaches the Tissue column, on menu ===\n")
  a <- published(info_of(), "Blood")
  chk("Tissue_Type='Blood' publishes in ROW 2 as the menu's own 'blood'",
      identical(a$row2, "blood"), paste("row2 =", a$row2))
  chk("...and nothing spills into row 3", is.na(a$row3), paste("row3 =", a$row3))
  chk("PBMC still publishes Cell type='mononuclear cells' alongside it",
      identical(a$celltype, "mononuclear cells"), paste("cell type =", a$celltype))

  cat("\n=== Info!Tissue_Fallback fires ONLY when Tissue_Type is empty ===\n")
  b <- published(info_of(Tissue_Fallback = "bone marrow"), "")
  chk("blank Tissue_Type falls back to Info!Tissue_Fallback",
      identical(b$row2, "bone marrow"), paste("row2 =", b$row2))

  cc <- published(info_of(Tissue_Fallback = "spleen"), "Blood")
  chk("a SET Tissue_Type is never overwritten by the fallback",
      identical(cc$row2, "blood"), paste("row2 =", cc$row2))

  d0 <- published(info_of(Tissue_Fallback = ""), "")
  chk("both blank publishes an EMPTY Tissue, not an inferred one",
      is.na(d0$row2) && is.na(d0$row3),
      paste("row2 =", d0$row2, "row3 =", d0$row3))

  e0 <- published(info_of())
  chk("no Tissue_Type column at all publishes empty (no error)",
      is.na(e0$row2), paste("row2 =", e0$row2))

  cat("\n=== an off-menu tissue is published, not discarded ===\n")
  f0 <- published(info_of(), "pancreas")
  chk("a tissue their menu does not list lands in ROW 3 verbatim",
      is.na(f0$row2) && identical(f0$row3, "pancreas"),
      paste("row2 =", f0$row2, "row3 =", f0$row3))

  cat("\n=== the shipped template carries both cells ===\n")
  tf <- file.path(ACTA_REPO_ROOT, "inst", "pipeline", "Template",
                  "TEMPLATE_Ab_Titration_Instructions.xlsx")
  if (file.exists(tf)) {
    ti <- as.data.frame(suppressMessages(readxl::read_excel(tf, sheet = "Info",
            col_types = "text", .name_repair = "minimal")), check.names = FALSE)
    gv <- function(k) { j <- which(!is.na(ti$Variable) & ti$Variable == k)
                        if (!length(j)) "<ABSENT>" else
                        if (is.na(ti$Value[j[1]])) "<BLANK>" else ti$Value[j[1]] }
    chk("TEMPLATE Info!Tissue is GONE (it is a per-well column now)",
        identical(gv("Tissue"), "<ABSENT>"), gv("Tissue"))
    chk("TEMPLATE Info!Tissue_Fallback ships BLANK",
        identical(gv("Tissue_Fallback"), "<BLANK>"), gv("Tissue_Fallback"))
    lpn <- .tp("actaStripHeaderAnnotation")(names(suppressMessages(readxl::read_excel(
             tf, sheet = "Layout_Plate", col_types = "text", .name_repair = "minimal"))))
    chk("TEMPLATE Layout_Plate carries Tissue_Type", "Tissue_Type" %in% lpn)
  } else chk("shipped template present", FALSE, tf)
}

cat(if (ok) "\nPASS\n" else "\nFAIL\n")
quit(status = if (ok) 0L else 1L)
