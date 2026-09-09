## Pins actaTemplateRowNodes() against openCyto's OWN row expansion.
##
## ACTA has to know, before gating, which populations a template row will create -- the scaffold keys
## its layout plots and stats on those names. For a `+/-+/-` quadrant row openCyto rewrites one row
## into six and names them from the DIMS, so predicting them is the only way to plot them. Predicting
## means duplicating an upstream rule, which is exactly the kind of thing that rots silently: hence
## this test compares the prediction to openCyto:::.preprocess_row(). The internal is used HERE ONLY,
## never in ACTA itself, so production code carries no dependency on it -- but an upstream change to
## the naming or the order is caught rather than mis-plotted.
##
## Rscript <this> [version_dir]   -- defaults to 2_98_WIP
## One cwd contract for the whole suite -- see acta_test_paths.R. Run this script from anywhere.
source(file.path(this.path::this.dir(), "acta_test_paths.R"))
vd <- actaTestVersionDir()
cat("version_dir:", basename(vd), "\n")
suppressMessages({library(openCyto); library(data.table)})
h <- actaTestFunctions(vd)
ok <- TRUE
chk <- function(what, cond) { if (!isTRUE(cond)) ok <<- FALSE
                              cat(sprintf("  [%s] %s\n", if (isTRUE(cond)) "ok" else "FAIL", what)) }

mkrow <- function(alias, pop, dims, gm = "mindensity")
  as.data.table(data.frame(alias = alias, pop = pop, parent = "MarkerB-", dims = dims,
                           gating_method = gm, gating_args = "", collapseDataForGating = "",
                           groupBy = "", preprocessing_method = "", preprocessing_args = "",
                           stringsAsFactors = FALSE))

cat("=== the quadrant expansion, against openCyto's own ===\n")
r  <- mkrow("*", "+/-+/-", "CD4, CD8")
up <- as.character(openCyto:::.preprocess_row(r)$alias)
mine <- h$actaTemplateRowNodes("*", "+/-+/-", "CD4, CD8")
cat("   openCyto:", paste(up, collapse = ", "), "\n")
cat("   ACTA    :", paste(mine, collapse = ", "), "\n")
chk("same names in the same order", identical(mine, up))
chk("the parentable quadrant is present", "CD4-CD8+" %in% mine)
chk("six populations from one row", length(mine) == 6)

cat("=== dims order decides which sign is which ===\n")
rev <- h$actaTemplateRowNodes("*", "+/-+/-", "CD8, CD4")
chk("reversing dims reverses the names", identical(rev, as.character(
  openCyto:::.preprocess_row(mkrow("*", "+/-+/-", "CD8, CD4"))$alias)))
chk("CD8-CD4+ replaces CD4-CD8+ when reversed",
    "CD8-CD4+" %in% rev && !("CD4-CD8+" %in% rev))

cat("=== the simple forms ===\n")
chk("'+' is named by the alias",   identical(h$actaTemplateRowNodes("Cells", "+", "FSC-A,SSC-A"), "Cells"))
chk("'-' is named by the alias",   identical(h$actaTemplateRowNodes("Live", "-", "LIVEDEAD,FSC-A"), "Live"))
chk("'+/-' yields the pair",       identical(h$actaTemplateRowNodes("Reagent", "+/-", "FSC-A"),
                                             c("Reagent+", "Reagent-")))
chk("a single quadrant is named by the alias",
    identical(h$actaTemplateRowNodes("Thelper", "-+", "CD4, CD8"), "Thelper"))
chk("'*' predicts nothing (names unknown before gating)",
    length(h$actaTemplateRowNodes("*", "*", "CD4, CD8")) == 0)
chk("'+/-+/-' with one dim predicts nothing rather than guessing",
    length(h$actaTemplateRowNodes("*", "+/-+/-", "CD4")) == 0)
chk("a blank pop falls back to the alias",
    identical(h$actaTemplateRowNodes("Cells", "", "FSC-A"), "Cells"))

cat("=== channel-named quadrants get their markers back (actaQuadNodeRenames) ===\n")
## A gate_quad_* row leaves its four populations named after the CHANNELS. The rename map is pure
## string work and is tested as such; that it is APPLIED correctly is proved by OQ_Test1, which is
## the case carrying the tmix form.
bd <- data.frame(Target  = c("CD4", "CD8", "Viability"),
                 Channel = c("BUV496-A", "Alexa Fluor 532-A", "Zombie-A"),
                 stringsAsFactors = FALSE)
gtq <- data.frame(alias = c("Cells", "CD4_CD8", "#CD4_CD8_off", "Reagent"),
                  pop   = c("+",     "+/-+/-",  "+/-+/-",       "+/-"),
                  dims  = c("FSC-A,SSC-A", "CD4, CD8", "CD4, CD8", "FSC-A"),
                  stringsAsFactors = FALSE)
mp <- h$actaQuadNodeRenames(gtq, bd)
chk("one map entry per quadrant of the one live +/-+/- row", nrow(mp) == 4L)
chk("commented-out rows contribute nothing", all(mp$alias == "CD4_CD8"))
chk("from = the channel names openCyto used",
    identical(sort(mp$from), sort(c("BUV496-A+Alexa Fluor 532-A+", "BUV496-A-Alexa Fluor 532-A+",
                                    "BUV496-A+Alexa Fluor 532-A-", "BUV496-A-Alexa Fluor 532-A-"))))
chk("to = the marker names, in openCyto's own expansion order",
    identical(mp$to, c("CD4+CD8+", "CD4-CD8+", "CD4+CD8-", "CD4-CD8-")))
## The pairing is the part that could silently MISLABEL a population: element k of `from` has to be
## the same quadrant as element k of `to`, which is what makes zipping the two sets safe.
##
## Note how this is checked. The obvious test -- strip everything but + and - from both names and
## compare -- FAILS, and the reason is the whole argument for zipping rather than parsing: channel
## names contain hyphens ("BUV496-A", "Alexa Fluor 532-A"), so a sign extracted from a composite
## channel-named population is not the gate's sign at all. Both sides are therefore rebuilt from the
## known tokens and the known quadrant order instead.
sgn <- c("++", "-+", "+-", "--")
chk("element k is the same quadrant on both sides", all(mapply(function(f, t, sg) {
  identical(f, paste0("BUV496-A", substr(sg, 1, 1), "Alexa Fluor 532-A", substr(sg, 2, 2))) &&
  identical(t, paste0("CD4",      substr(sg, 1, 1), "CD8",               substr(sg, 2, 2)))
}, mp$from, mp$to, sgn)))
chk("the tail of the map matches actaTemplateRowNodes' channel-named block",
    identical(mp$from, tail(h$actaTemplateRowNodes("CD4_CD8", "+/-+/-", "CD4, CD8",
                                                   c("BUV496-A", "Alexa Fluor 532-A")), 4)))
chk("dims written as channels are left alone (no identity renames)",
    nrow(h$actaQuadNodeRenames(data.frame(alias = "SS", pop = "+/-+/-", dims = "FSC-A, SSC-A",
                                          stringsAsFactors = FALSE),
                               data.frame(Target = "FSC-A", Channel = "FSC-A",
                                          stringsAsFactors = FALSE))) == 0)
chk("a template with no quadrant row yields an empty map, not an error",
    nrow(h$actaQuadNodeRenames(gtq[gtq$pop != "+/-+/-", ], bd)) == 0)
chk("a template missing the columns yields an empty map",
    nrow(h$actaQuadNodeRenames(data.frame(alias = "x", stringsAsFactors = FALSE), bd)) == 0)

cat("=== a quadrant method reads `pop`, not `alias` (actaQuadPopFix) ===\n")
## The user wrote the row the way openCyto's multi-population alias syntax reads:
##   alias = "CD4-CD8+,CD4+CD8+,CD4+CD8-,CD4-CD8-"   pop = "*"   gating_method = gate_quad_tmix
## For the four quadrant methods openCyto ignores the alias and PARSES pop, so that row dies inside
## openCyto with "subscript out of bounds" after every gate has been computed. ACTA rewrites the pop.
##
## The predicate is pinned against openCyto's OWN two lines rather than a list of strings I believe
## to be right -- the same discipline as the .preprocess_row() pinning above.
ocCross <- function(p) {
  x <- try({ pops <- gsub("([\\+-])([^/$])", "\\1&\\2", p)
             pops <- strsplit(pops, "&")[[1]]; pops <- strsplit(pops, "/")
             paste0(rep(pops[[1]], each = length(pops[[2]])), pops[[2]]) }, silent = TRUE)
  !inherits(x, "try-error")
}
for (pv in c("+/-+/-", "-+", "++", "+-", "--", "+/-+", "*", "", "+", "-", "+/-"))
  chk(sprintf("pop '%s': ACTA agrees with openCyto on parseability", pv),
      identical(h$.actaQuadPopOK(pv), ocCross(pv)))

qrow <- function(alias, pop, gm = "gate_quad_tmix")
  data.frame(alias = c("Cells", alias), pop = c("+", pop), dims = c("FSC-A,SSC-A", "CD4, CD8"),
             gating_method = c("gate_flowclust_2d", gm), stringsAsFactors = FALSE)

f <- h$actaQuadPopFix(qrow("CD4-CD8+,CD4+CD8+,CD4+CD8-,CD4-CD8-", "*"))
chk("the user's row is rewritten to the full quadrant spec", identical(f$template$pop[2], "+/-+/-"))
chk("and it says so, naming the row and the method",
    length(f$notes) == 1 && grepl("gate_quad_tmix", f$notes[1], fixed = TRUE) &&
      grepl("IGNORES `alias`", f$notes[1], fixed = TRUE))
chk("a blank pop on a quadrant row is rewritten too",
    identical(h$actaQuadPopFix(qrow("CD4_CD8", ""))$template$pop[2], "+/-+/-"))
chk("a pop openCyto CAN parse is left exactly as written",
    { g <- h$actaQuadPopFix(qrow("CD4_CD8", "-+")); identical(g$template$pop[2], "-+") &&
        !length(g$notes) })
chk("every quadrant method openCyto has is covered",
    all(vapply(h$ACTA_QUAD_METHODS, function(m)
      identical(h$actaQuadPopFix(qrow("X", "*", m))$template$pop[2], "+/-+/-"), TRUE)))
chk("a NON-quadrant method keeps pop '*' -- it is legitimate there",
    { g <- h$actaQuadPopFix(qrow("A,B,C", "*", "gate_flowclust_2d"))
      identical(g$template$pop[2], "*") && !length(g$notes) })
chk("commented-out quadrant rows are left alone",
    { g <- h$actaQuadPopFix(qrow("#CD4_CD8", "*")); identical(g$template$pop[2], "*") &&
        !length(g$notes) })
chk("a template without the columns is returned untouched",
    { d <- data.frame(alias = "x", stringsAsFactors = FALSE)
      identical(h$actaQuadPopFix(d)$template, d) })


## ---------------------------------------------------------------------------------------------
## actaDrawnNodes(): which of a multi-population row's nodes get drawn on its one plot.
##
## The bug this exists for (reported 2026-08-28): a `pop = "+/-"` row written the natural way --
## alias CD3, dims "CD3, FSC-A" -- gated correctly, appeared correctly in the tree and in
## StatsExport, and plotted only CD3-. The quadrant rule drops `<dim>+` as a redundant intermediate,
## and with the alias equal to the dim that string IS the row's own CD3+.
## The quadrant assertions below are kept as they were, so the contract stays legible: the rule
## still drops real intermediates, it just never drops the row's own pair.
## ---------------------------------------------------------------------------------------------
cat("\n=== actaDrawnNodes: a +/- row keeps BOTH of its own populations ===\n")

chk("alias EQUALS a dim token -- both drawn (the reported bug)",
    setequal(h$actaDrawnNodes(c("CD3+","CD3-"), "CD3, FSC-A", "CD3"), c("CD3+","CD3-")))

chk("alias differs from the dim -- both drawn, as before",
    setequal(h$actaDrawnNodes(c("CD3+","CD3-"), "CD3-BUV496, FSC-A", "CD3"), c("CD3+","CD3-")))

chk("the marker row's shape -- both drawn",
    setequal(h$actaDrawnNodes(c("Reagent+","Reagent-"), "FITC-A, FSC-A", "Reagent"), c("Reagent+","Reagent-")))

chk("a one-dim +/- row keeps both",
    setequal(h$actaDrawnNodes(c("CD3+","CD3-"), "CD3", "CD3"), c("CD3+","CD3-")))

cat("\n=== ... and a quadrant still drops its 1D intermediates ===\n")

.q <- c("CD4+","CD8+","CD4+CD8+","CD4-CD8+","CD4+CD8-","CD4-CD8-")
chk("quadrant: the four rectangles are drawn",
    setequal(h$actaDrawnNodes(.q, "CD4, CD8", "CD4_CD8"),
             c("CD4+CD8+","CD4-CD8+","CD4+CD8-","CD4-CD8-")))
chk("quadrant: CD4+ and CD8+ are NOT drawn",
    !any(c("CD4+","CD8+") %in% h$actaDrawnNodes(.q, "CD4, CD8", "CD4_CD8")))
chk("quadrant authored as alias '*' behaves the same",
    setequal(h$actaDrawnNodes(.q, "CD4, CD8", "*"),
             c("CD4+CD8+","CD4-CD8+","CD4+CD8-","CD4-CD8-")))

## Excluding everything would leave an empty panel, so the rule falls back to drawing all of them
## rather than producing a plot with no gates on it.
chk("if the exclusion would empty the set, everything is drawn",
    setequal(h$actaDrawnNodes(c("CD4+","CD8+"), "CD4, CD8", "CD4_CD8"), c("CD4+","CD8+")))

cat(if (ok) "\nALL QUAD-POP TESTS PASS\n" else "\nFAILURES ABOVE\n")
quit(status = if (ok) 0 else 1)
