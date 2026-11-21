## The facet-strip label: actaStripLabel(), and the two invariants around it.
##
## THE DEFECT IT FIXES. abLabel's line 1 -- "<alias> <channel> (<gating method>)" -- is the only
## one of its four lines with no break opportunity. The strip text is CENTRED, so a line 1 wider
## than its panel clips at BOTH ends: EXP1460's CD127 strip read
## "D127 Alexa Fluor 700-A (gate_mind_fmeans_costain", losing its leading "C" and its bracket.
##
## WHY IT ONLY SHOWS ON SOME PLOTS: facet_wrap(scales = "free_y") gives EVERY panel its own y
## axis, and that axis costs ~0.40 in of panel width. Same text, same ncol, same device.
##
## THIS TEST MEASURES, it does not grep. The geometric assertions below resolve real panel widths
## off a built gtable and compare them against real text widths at 8pt bold -- so reverting
## actaStripLabel() to identity FAILS them. A string-shape assertion alone would not.

source(file.path(this.path::this.dir(), "acta_test_paths.R"))
ok <- TRUE
chk <- function(label, pass, detail = NULL) {
  d <- paste(as.character(detail), collapse = " ")
  if (!isTRUE(pass)) { ok <<- FALSE
    cat(sprintf("  [FAIL] %s%s\n", label, if (nzchar(d)) paste0(" -- ", d) else "")) }
  else cat(sprintf("  [ok]   %s\n", label))
}
suppressMessages(library(ggplot2)); suppressMessages(library(grid))
## THIS CHECKOUT's sources, not the installed namespace. The first draft used `ACTA:::` and so
## graded whatever ACTA happened to be installed on the machine: a stale install failed it for a
## reason that had nothing to do with the tree, and -- the dangerous direction -- a CORRECT install
## would have reported a pass over a regressed tree. Same rule actaTestFunctions() exists for.
h  <- actaTestFunctions()
sl <- h$actaStripLabel

## A real six-reagent-run label, verbatim from that run's SI_stats!label.
L <- paste0("CD127 Alexa Fluor 700-A (gate_mind_fmeans_costain)\n",
            "Cat: 351344 || Clone: A019D5\n",
            "BioLegend || Lot: B413005\n",
            "Test sample: HD Human PBMC at 1e6/well")

cat("\n-- 1. the break itself --\n")
B <- sl(L)
chk("line 1 loses the gating method",
    identical(strsplit(B, "\n", fixed = TRUE)[[1]][1], "CD127 Alexa Fluor 700-A"))
chk("line 2 becomes the gating method, brackets intact",
    identical(strsplit(B, "\n", fixed = TRUE)[[1]][2], "(gate_mind_fmeans_costain)"))
chk("the label gains exactly one line", length(strsplit(B, "\n", fixed = TRUE)[[1]]) ==
    length(strsplit(L, "\n", fixed = TRUE)[[1]]) + 1L)
chk("NOTHING is lost -- the characters are the same bar the inserted newline",
    identical(gsub("[\n ]", "", B), gsub("[\n ]", "", L)))
chk("lines 2-4 are untouched",
    identical(strsplit(B, "\n", fixed = TRUE)[[1]][3:5],
              strsplit(L, "\n", fixed = TRUE)[[1]][2:4]))

cat("\n-- 2. it declines to act when it should --\n")
chk("a label with no gating method is returned unchanged",
    identical(sl("CD3 FITC-A\nCat: 1\nV || Lot: 2\nTest sample: x"),
              "CD3 FITC-A\nCat: 1\nV || Lot: 2\nTest sample: x"))
chk("a parenthesis on a LATER line is not broken",
    identical(sl("CD3 FITC-A\nCat: 1 (repack)\nV"), "CD3 FITC-A\nCat: 1 (repack)\nV"))
chk("NA survives as NA", is.na(sl(NA_character_)))
chk("it is vectorised", identical(sl(c(L, L)), c(B, B)))
chk("applying it twice changes nothing more", identical(sl(B), B))
## An alias carrying its own brackets must break at the LAST group, not the first.
chk("breaks at the gating method, not at an earlier bracket",
    identical(strsplit(sl("CD3 (clone X) FITC-A (gate_flowmeans)\nCat: 1"), "\n", fixed = TRUE)[[1]][1],
              "CD3 (clone X) FITC-A"))

cat("\n-- 3. THE GEOMETRY -- measured, at the real device size --\n")
## facetLayout() is the package's own; 6 facets is EXP1460 and the only grid in the validation
## set that reaches 4 columns.
siLay <- h$facetLayout(6)
chk("6 reagents still give a 4-column grid", siLay$ncol == 4L, siLay$ncol)

tw <- function(s) {
  pdf(NULL, width = siLay$width, height = siLay$height); on.exit(dev.off(), add = TRUE)
  max(vapply(strsplit(s, "\n", fixed = TRUE)[[1]], function(ln)
    as.numeric(convertWidth(grobWidth(textGrob(ln, gp = gpar(fontsize = 8, fontface = "bold"))),
                            "in")), 0))
}
## Panel widths are null units; resolve them the way grid does -- absolute widths first, then
## share what is left among the nulls in proportion.
pw <- function(free_y) {
  d <- do.call(rbind, lapply(1:6, function(i)
         data.frame(label = paste0("f", i), x = factor(c("0.5", "1", "2", "5")),
                    y = c(1, 20, 300, 4000) * i)))
  p <- ggplot(d, aes(x, y)) + geom_point() +
       (if (free_y) facet_wrap(~label, scales = "free_y", ncol = siLay$ncol)
        else        facet_wrap(~label,                    ncol = siLay$ncol)) +
       theme_minimal() +
       theme(axis.text.y = element_text(size = 12, face = "italic"),
             axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1, size = 12),
             strip.text  = element_text(size = 8, face = "bold"),
             panel.spacing = unit(0.4, "lines"))
  pdf(NULL, width = siLay$width, height = siLay$height); on.exit(dev.off(), add = TRUE)
  g <- ggplotGrob(p); w <- g$widths
  isnull  <- grepl("null", as.character(w), fixed = TRUE)
  abs_in  <- sum(vapply(which(!isnull), function(i) as.numeric(convertWidth(w[i], "in")), 0))
  nullamt <- vapply(which(isnull), function(i) as.numeric(w[i]), 0)
  per <- (siLay$width - abs_in) * nullamt / sum(nullamt)
  idx <- match(unique(g$layout$l[grep("^panel", g$layout$name)]), which(isnull))
  min(per[idx[!is.na(idx)]])
}
P_free  <- pw(TRUE)
P_fixed <- pw(FALSE)
cat(sprintf("     panel width: free_y %.3f in, fixed %.3f in (the y axis costs %.3f in)\n",
            P_free, P_fixed, P_fixed - P_free))
chk("a free_y panel really is narrower than a fixed-scale one", P_free < P_fixed)

chk("THE DEFECT IS REAL: the unbroken label overflows a free_y panel",
    tw(L) > P_free, sprintf("text %.3f in vs panel %.3f in", tw(L), P_free))
chk("THE FIX WORKS: the broken label fits a free_y panel",
    tw(B) <= P_free, sprintf("text %.3f in vs panel %.3f in", tw(B), P_free))
chk("...and fits the fixed-scale (concatenation) panel too", tw(B) <= P_fixed)
## If someone neuters actaStripLabel() to identity, the two assertions above become
## contradictory and this one reports why in one line.
chk("the fix buys real headroom, not a rounding error", (P_free - tw(B)) > 0.05,
    sprintf("headroom %.3f in", P_free - tw(B)))

cat("\n-- 4. THE EXPORT INVARIANT: the STORED label must not be rewritten --\n")
## `label` is published as the export's SI_stats!label. The break is presentation only, so
## ACTA_Script.R must still store the bare abLabel and apply actaStripLabel only as a labeller.
## Parsed, not grepped: deparse the real expressions.
src <- system.file("pipeline/ACTA_Script.R", package = "ACTA")
chk("ACTA_Script.R is where we think it is", nzchar(src) && file.exists(src), src)
txt <- paste(deparse(parse(src)), collapse = "\n")
chk("the stored label is still the bare abLabel",
    grepl("label\\s*=\\s*abLabel", txt))
chk("abLabel is never assigned from actaStripLabel",
    !grepl("abLabel\\s*<-[^\n]*actaStripLabel", txt))
chk("the labeller IS wired in (and to more than one facet)",
    lengths(regmatches(txt, gregexpr("as_labeller\\(actaStripLabel\\)", txt)))[1] >= 2L,
    lengths(regmatches(txt, gregexpr("as_labeller\\(actaStripLabel\\)", txt)))[1])

cat(if (ok) "\nSTRIP LABEL TESTS PASSED\n" else "\nFAILURES ABOVE\n")
quit(status = if (ok) 0L else 1L)
