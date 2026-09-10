## Guards actaMarkerNodesFromPaths(): the marker +/- populations must be resolved from the MARKER ROW,
## never by taking the first suffixed node in the tree.
##
## The bug this exists for (an internal run, 2026-08-18): with CD3, MarkerB and Reagent all pop = "+/-", the
## old `grep("[+]$", basename(paths))[1]` selected CD3+/CD3-, so the Stain Index would have been
## computed on CD3. Neither OQ case catches it -- in both, Reagent is the only "+/-" row, so the old
## code was right by accident. Hence this test.
## Rscript <this> [version_dir]   -- defaults to 3_0; pass 2_87 to check the backport.
## One cwd contract for the whole suite -- see acta_test_paths.R. Run this script from anywhere.
source(file.path(this.path::this.dir(), "acta_test_paths.R"))
vd <- actaTestVersionDir()
cat("version_dir:", basename(vd), "\n")
h <- actaTestFunctions(vd)
ok <- TRUE
chk <- function(what, cond) { if (!cond) ok <<- FALSE; cat(sprintf("  [%s] %s\n", if (cond) "ok" else "FAIL", what)) }

gt <- function(...) do.call(rbind, lapply(list(...), function(r)
  data.frame(alias = r[[1]], pop = r[[2]], parent = r[[3]], dims = r[[4]], stringsAsFactors = FALSE)))

cat("=== intermediate +/- gates present (the reported failure) ===\n")
## openCyto names "+/-" nodes from the first dims token, so the Reagent row yields FSC-A+/FSC-A-.
paths <- c("root", "/Cells", "/Cells/Single_cells", "/Cells/Single_cells/Live",
           "/Cells/Single_cells/Live/CD3+", "/Cells/Single_cells/Live/CD3-",
           "/Cells/Single_cells/Live/CD3+/MarkerB+", "/Cells/Single_cells/Live/CD3+/MarkerB-",
           "/Cells/Single_cells/Live/CD3+/MarkerB-/FSC-A+", "/Cells/Single_cells/Live/CD3+/MarkerB-/FSC-A-")
tmpl <- gt(list("Cells","+","root","FSC-A,SSC-A"), list("Single_cells","+","Cells","FSC-A,FSC-H"),
           list("Live","-","Single_cells","LIVEDEAD,FSC-A"), list("CD3","+/-","Live","CD3,FSC-A"),
           list("MarkerB","+/-","CD3+","MarkerB,FSC-A"), list("Reagent","+/-","MarkerB-","FSC-A"))
r <- h$actaMarkerNodesFromPaths(paths, tmpl, "Granzyme-K")
chk("picks the marker pair, not the first suffixed node", r$pos == "FSC-A+" && r$neg == "FSC-A-")
chk("indices point at the marker paths",
    paths[r$pos_index] == "/Cells/Single_cells/Live/CD3+/MarkerB-/FSC-A+")
old <- basename(paths)[grepl("[+]$", basename(paths))][1]
chk(sprintf("the OLD heuristic would have picked %s -- demonstrating the bug", old), old == "CD3+")

cat("=== single +/- row (both OQ cases) still resolves ===\n")
p2 <- c("root", "/Cells", "/Cells/Single_cells", "/Cells/Single_cells/Live",
        "/Cells/Single_cells/Live/FSC-A+", "/Cells/Single_cells/Live/FSC-A-")
t2 <- gt(list("Cells","+","root","FSC-A,SSC-A"), list("Single_cells","+","Cells","FSC-A,FSC-H"),
         list("Live","-","Single_cells","LIVEDEAD,FSC-A"), list("Reagent","+/-","Live","FSC-A"))
r2 <- h$actaMarkerNodesFromPaths(p2, t2, "Strep_PE")
chk("unchanged for a single +/- row", r2$pos == "FSC-A+" && r2$neg == "FSC-A-")

cat("=== suffixed ALIASES on single-pop rows (that run's tree as run) ===\n")
## Not a "+/-" row at all: the aliases themselves end in + and -, so a suffix scan finds them first.
## This is the shape that actually crashed the render -- the CD3+/MarkerB- gates are on BUV805-A/PE-A,
## neither of which is on the pseudocolor's axes, so ggcyto's draw_panel subset the layer data by a
## column that was not there: "undefined columns selected", reported as the 2nd layer.
p6 <- c("root", "/Cells", "/Cells/Single_cells", "/Cells/Single_cells/Live",
        "/Cells/Single_cells/Live/CD3+", "/Cells/Single_cells/Live/CD3+/MarkerB-",
        "/Cells/Single_cells/Live/CD3+/MarkerB-/Reagent+", "/Cells/Single_cells/Live/CD3+/MarkerB-/Reagent-")
t6 <- gt(list("Cells","+","root","FSC-A,SSC-A"), list("Single_cells","+","Cells","FSC-A,FSC-H"),
         list("Live","-","Single_cells","LIVEDEAD,FSC-A"), list("CD3+","+","Live","CD3,FSC-A"),
         list("MarkerB-","-","CD3+","MarkerB,FSC-A"), list("Reagent","+/-","MarkerB-","FITC-A, FSC-A"))
r6 <- h$actaMarkerNodesFromPaths(p6, t6, "Granzyme-K")
chk("resolves Reagent+/Reagent- under MarkerB-", r6$pos == "Reagent+" && r6$neg == "Reagent-")
chk("old heuristic picked CD3+ / MarkerB- instead",
    basename(p6)[grepl("[+]$", basename(p6))][1] == "CD3+" &&
    basename(p6)[grepl("[-]$", basename(p6))][1] == "MarkerB-")
chk("parent_path is the marker row's parent, for parentPop",
    r6$parent_path == "/Cells/Single_cells/Live/CD3+/MarkerB-")

cat("=== a quadrant row SHARING the marker's parent (internal run, 2026-08-19) ===\n")
## gate_quad_tmix under Live names its four populations from the CHANNELS, so Live ends up with three
## "+"-suffixed and three "-"-suffixed children. The old "exactly one of each" rule rejected that and
## the whole run stopped: "found 3 '+' and 3 '-' child population(s)".
p7 <- c("root", "/Cells", "/Cells/Single_cells", "/Cells/Single_cells/Live",
        "/Cells/Single_cells/Live/Reagent+", "/Cells/Single_cells/Live/Reagent-",
        "/Cells/Single_cells/Live/BUV496-A+Alexa Fluor 532-A+",
        "/Cells/Single_cells/Live/BUV496-A+Alexa Fluor 532-A-",
        "/Cells/Single_cells/Live/BUV496-A-Alexa Fluor 532-A+",
        "/Cells/Single_cells/Live/BUV496-A-Alexa Fluor 532-A-")
t7 <- gt(list("Cells","+","root","FSC-A,SSC-A"), list("Single_cells","+","Cells","FSC-A,FSC-H"),
         list("Live","-","Single_cells","Viability,FSC-A"),
         list("CD4_CD8","+/-+/-","Live","CD4, CD8"), list("Reagent","+/-","Live","FITC-A, FSC-A"))
r7 <- h$actaMarkerNodesFromPaths(p7, t7, "Strep_PE")
chk("picks Reagent+/Reagent- despite four quadrant siblings",
    r7$pos == "Reagent+" && r7$neg == "Reagent-")
chk("a bare suffix scan would have found 3 of each -- which is why naming beats matching",
    sum(grepl("[+]$", basename(p7))) == 3 && sum(grepl("-$", basename(p7))) == 3)

cat("=== dims-based auto-naming still resolves (alias ignored by openCyto) ===\n")
p8 <- c("root", "/Live", "/Live/FITC-A+", "/Live/FITC-A-")
t8 <- gt(list("Live","-","root","Viability,FSC-A"), list("Reagent","+/-","Live","FITC-A, FSC-A"))
r8 <- h$actaMarkerNodesFromPaths(p8, t8, "X")
chk("falls back to the first dims token", r8$pos == "FITC-A+" && r8$neg == "FITC-A-")

cat("=== failure modes are named, not silent ===\n")
t3 <- gt(list("Cells","+","root","FSC-A,SSC-A"), list("Reagent","+","Cells","FSC-A"))
p3 <- c("root", "/Cells", "/Cells/Reagent")
e <- try(h$actaMarkerNodesFromPaths(p3, t3, "X"), silent = TRUE)
chk("marker row without +/- children errors", inherits(e, "try-error") &&
      grepl("must have pop = '\\+/-'", conditionMessage(attr(e, "condition"))))
t4 <- gt(list("Reagent","+/-","root","FSC-A"), list("Antibody","+/-","root","FSC-A"))
e2 <- try(h$actaMarkerNodesFromPaths(c("root","/FSC-A+","/FSC-A-"), t4, "X"), silent = TRUE)
chk("two marker rows error", inherits(e2, "try-error") &&
      grepl("exactly one is required", conditionMessage(attr(e2, "condition"))))
t5 <- gt(list("Reagent","+/-","Dup","FSC-A"))
e3 <- try(h$actaMarkerNodesFromPaths(c("root","/A/Dup","/B/Dup"), t5, "X"), silent = TRUE)
chk("ambiguous parent errors", inherits(e3, "try-error") &&
      grepl("matches 2 populations", conditionMessage(attr(e3, "condition"))))
chk("root as parent resolves", {
  r5 <- h$actaMarkerNodesFromPaths(c("root","/FSC-A+","/FSC-A-"),
                                   gt(list("Reagent","+/-","root","FSC-A")), "X")
  r5$pos == "FSC-A+" && r5$neg == "FSC-A-" })

## Its sibling, added 2026-08-26 after a run whose layout drew wells from TWO notebook entries. The
## old `unique(metadata_file$ELN_ID)` was a length-2 VECTOR, which killed the export in
## saveWorkbook() ("'length = 2' in coercion to logical(1)") and, before that, wrote the run-level
## PNGs from element [1] only -- so wells from the other entry were misattributed in the filename.
## The optional title/strip segment. Added 2026-08-27 with the Clone field, which is the first
## piece of metadata to appear in BOTH a plot title and a facet strip -- and they punctuate
## differently on purpose, so the formatter is shared and the separator is the argument.
cat("=== optional title segments (actaTitleStamp) ===\n")
chk("a plot title chains on ' | ' and the prefix goes inside it",
    identical(h$actaTitleStamp("EH12.2H7", "Clone "), " | Clone EH12.2H7"))
chk("the facet strip chains on ' || ', matching the strip's own separators",
    identical(h$actaTitleStamp("EH12.2H7", "Clone: ", " || "), " || Clone: EH12.2H7"))
## The whole reason it is a function: absent metadata must print NOTHING, separator included.
## "Cat: 123 || Clone: " reads as a field that failed rather than one this workbook never fills.
chk("nothing to say prints nothing at all -- separator, prefix and value",
    all(vapply(list("", NA, "   ", NULL, character(0), NA_character_),
               function(x) identical(h$actaTitleStamp(x, "Clone ") , ""), logical(1))))
chk("a value is trimmed, not printed with the sheet's padding",
    identical(h$actaTitleStamp("  EH12.2H7  ", "Clone "), " | Clone EH12.2H7"))
## Same guard actaElnLabel has, for the same reason: a length>1 title silently renders one plot
## per element and duplicates the saved PNGs.
chk("a vector is joined, never recycled -- the result is always one string",
    identical(h$actaTitleStamp(c("A", "B"), "Clone "), " | Clone A, B") &&
      length(h$actaTitleStamp(c("A", "B", "C"))) == 1L)
chk("no prefix and no separator given behaves like the ELN/operator segments it replaced",
    identical(h$actaTitleStamp("EXP00000001"), " | EXP00000001"))

## THE COUPLING THAT MADE THE CLONE A NO-OP, 2026-08-27. abMeta is built with
## `distinct(Cat_Num, Vendor, Lot_Num, Clone, Test_Material, e6_cells_per_well)`, and distinct()
## keeps ONLY the columns it is handed. A field added to the descriptor but not to that list
## resolves to NULL, and actaTitleStamp() -- which must stay quiet for a workbook that genuinely
## has no such column -- then prints nothing. The clone went missing from the facet strip, both
## marker plots and every layout title, and NOTHING caught it: the plots rendered, the fast suite
## was 8/8 and both OQ cases were 16/16. Only reading the built title strings showed it.
##
## So this is a TEXT check on the script, not a behaviour check on a helper: the defect is a
## mismatch between two lists, which is decidable without running anything.
cat("=== the descriptor's fields survive distinct() (the silent-clone regression) ===\n")
.scr <- list.files(vd, pattern = "^ACTA_Script.*\\.R$", full.names = TRUE)
chk("exactly one ACTA_Script to read", length(.scr) == 1L)
if (length(.scr) == 1L) {
  .txt  <- paste(readLines(.scr[[1]], warn = FALSE), collapse = "\n")
  .call <- regmatches(.txt, regexpr("distinct\\(Cat_Num[^)]*\\)", .txt))
  chk("the abMeta distinct() call is still recognisable", length(.call) == 1L)
  if (length(.call) == 1L) {
    .kept <- trimws(strsplit(gsub("^distinct\\(|\\)$", "", .call), ",")[[1]])
    .used <- unique(sub("^abMeta\\$", "",
                        regmatches(.txt, gregexpr("abMeta\\$[A-Za-z0-9_.]+", .txt))[[1]]))
    .missing <- setdiff(.used, .kept)
    chk(sprintf("every abMeta$<field> the script prints is kept by distinct() (%d used, %d kept)",
                length(.used), length(.kept)),
        !length(.missing))
    if (length(.missing))
      cat("        NOT kept by distinct(), so they print as nothing: ",
          paste(.missing, collapse = ", "), "\n", sep = "")
    chk("Clone is both kept and used -- the field this test exists for",
        "Clone" %in% .kept && "Clone" %in% .used)
    ## MUTATION SELF-CHECK. A static check that cannot be shown to FAIL is indistinguishable from
    ## one that never looks, and this one would rot silently the first time the call was reshaped.
    ## So re-run the same logic against the text with ", Clone" deleted from distinct() -- exactly
    ## the state the script was in on 2026-08-27 -- and require that it reports the miss.
    .broken <- sub("distinct\\(Cat_Num, Vendor, Lot_Num, Clone,",
                   "distinct(Cat_Num, Vendor, Lot_Num,", .txt, fixed = FALSE)
    .bcall  <- regmatches(.broken, regexpr("distinct\\(Cat_Num[^)]*\\)", .broken))
    .bkept  <- trimws(strsplit(gsub("^distinct\\(|\\)$", "", .bcall), ",")[[1]])
    chk("...and the check BITES: drop Clone from distinct() and it is reported missing",
        !identical(.broken, .txt) && "Clone" %in% setdiff(.used, .bkept))
  }
}

cat("=== ELN labels for the run-level figures and the export (actaElnLabel) ===\n")
chk("one entry comes back untouched -- the ordinary case, and every OQ case",
    identical(h$actaElnLabel(rep("EXP00000001", 12)), "EXP00000001") &&
      identical(h$actaElnLabel(rep("EXP00000001", 12), for_file = TRUE), "EXP00000001"))
chk("two entries are BOTH named, in layout order",
    identical(h$actaElnLabel(c(rep("EXP00000002", 96), rep("EXP00000001", 12))),
              "EXP00000002, EXP00000001"))
chk("...and a filename joins them without a comma or a space",
    identical(h$actaElnLabel(c("EXP00000002", "EXP00000001"), for_file = TRUE),
              "EXP00000002+EXP00000001"))
## The whole point: whatever goes in, ONE string comes out. A length>1 value here vectorised
## paste0() and then file.exists(), which is how the failure reached openxlsx.
chk("the result is always a single string",
    all(vapply(list(character(0), NA, c("A", NA, "B"), c("A", "A"), c("A", "B", "C")),
               function(x) length(h$actaElnLabel(x)) == 1L, logical(1))))
chk("NA and blank entries are dropped, not printed",
    identical(h$actaElnLabel(c("EXP00000001", NA, "", "  ")), "EXP00000001"))
chk("nothing recorded says so, and stays filename-safe",
    identical(h$actaElnLabel(character(0)), "not recorded") &&
      identical(h$actaElnLabel(c(NA, ""), for_file = TRUE), "NA"))
chk("a character a filesystem would choke on is replaced, in the file variant only",
    identical(h$actaElnLabel("EXP/0001 v2", for_file = TRUE), "EXP_0001_v2") &&
      identical(h$actaElnLabel("EXP/0001 v2"), "EXP/0001 v2"))

cat("=== parent labels for the run-level figures (actaParentLabel) ===\n")
## SIPlot / SatPlot / ConcatPlot are ONE figure across every antibody group, so they cannot name a
## single parent unless the groups agree. Note what is NOT claimed here: with one gating_template
## sheet the marker row's parent is the same for every group -- only its `dims` is patched per
## antibody -- so a disagreement cannot arise today. This is a guard against the day it can, and it
## replaces reading a loop variable after the loop, which was only correct by accident.
chk("groups that agree give the shared parent",
    identical(h$actaParentLabel(c(CD19 = "Live", CD7 = "Live", CD56 = "Live")), "Live"))
chk("a single group gives its own parent",
    identical(h$actaParentLabel(c(Strep_PE = "CD4-CD8+")), "CD4-CD8+"))
chk("groups that disagree are spelled out, naming which is which",
    { lab <- h$actaParentLabel(c(A = "Live", B = "MarkerB-CD8+"))
      grepl("A: Live", lab, fixed = TRUE) && grepl("B: MarkerB-CD8+", lab, fixed = TRUE) })
chk("a filename collapses a disagreement to one short token",
    identical(h$actaParentLabel(c(A = "Live", B = "MarkerB-CD8+"), for_file = TRUE), "mixed"))
chk("a filename keeps the real name when they agree",
    identical(h$actaParentLabel(c(A = "Live", B = "Live"), for_file = TRUE), "Live"))
chk("nothing recorded says so rather than writing NA into a path",
    identical(h$actaParentLabel(character(0)), "not recorded") &&
      identical(h$actaParentLabel(character(0), for_file = TRUE), "NA"))
chk("blank and NA entries are ignored, not counted as a disagreement",
    identical(h$actaParentLabel(c(A = "Live", B = NA, C = "")), "Live"))

cat(if (ok) "\nALL MARKER-NODE TESTS PASS\n" else "\nFAILURES ABOVE\n")
quit(status = if (ok) 0 else 1)
