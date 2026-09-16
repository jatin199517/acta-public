## Guards make_plate_layout_great_again(), the "Plate map" section added in 3_0 and
## rebuilt on ggplate later the same version.
##
## The map's whole value is that a transcription error is visible on it, so a map that is itself
## wrong is worse than no map: it would show a clean gradient for a sheet that has none. Nothing
## about it fails loudly -- a transposed plate, a dropped well and a phantom dose all render as a
## perfectly well-formed table. Hence the four things pinned here:
##
##   1. THE REAGENT AXIS. OQ_Test3 runs one reagent per row, OQ_Test2 one reagent per COLUMN.
##      Whichever way the detection is hardcoded, one of the two shipped cases renders transposed,
##      and a transposed plate map still looks like a plate map.
##   2. DIRNAME-LESS WELLS. OQ_Test1's sheet holds a StainQty for 45 wells that carry no Dirname
##      (the template was filled down the plate). Mapping them would put four phantom titration
##      series on the map of a single-reagent run.
##   3. THE COMBINATORIAL PAIR ORDER. A well showing "0.625/0.3125" is only readable if the two
##      doses are in the same order as the two aliases in the tier beside it. Nothing downstream
##      would notice them swapped.
##   4. THE HEADER SPAN CONTRACT. A tier's span widths must sum to ncol(body). Nothing enforces it,
##      and a tier one cell short emits a header row with the wrong number of `&`, so the report
##      fails at pdflatex with "extra alignment tab" hundreds of lines from the cause.
##   5. THE GRID ITSELF, and the escaping under it. A missing rule or a mis-escaped `_` is only
##      visible in a rendered PDF, which no gate opens.
##
## Rscript <this> [version_dir]   -- defaults to the version folder this file lives in.
source(file.path(this.path::this.dir(), "acta_test_paths.R"))
vd <- actaTestVersionDir()
cat("version_dir:", basename(vd), "\n")
h <- actaTestFunctions(vd)
ok <- TRUE
chk <- function(what, cond) { if (!isTRUE(cond)) ok <<- FALSE; cat(sprintf("  [%s] %s\n", if (isTRUE(cond)) "ok" else "FAIL", what)) }

mk <- function(...) h$make_plate_layout_great_again(...)

## The real sheets, read exactly as the script reads them: header annotations stripped, nothing else.
layoutOf <- function(case) {
  f <- Sys.glob(file.path(actaTestCaseRoot(), case, "*Instructions*.xlsx"))
  if (!length(f)) return(NULL)
  d <- suppressMessages(readxl::read_excel(f[[1]], sheet = "Layout_Plate"))
  names(d) <- h$actaStripHeaderAnnotation(names(d))
  d
}

cat("=== doses are formatted as typed, not as R stores them ===\n")
## The trap: format() on a VECTOR picks one common format, which pads 5 to "5.0000000" to line up
## with 0.009765625. And OQ_Test1's StainQty column is CHARACTER ("7.8125E-2"), because Excel typed
## the column off its first cells -- exponent form on a plate map reads as a different quantity.
chk("no trailing zeros", identical(h$.actaDose(c(5, 2.5)), c("5", "2.5")))
chk("no scientific notation from a numeric",
    identical(h$.actaDose(0.009765625), "0.009765625"))
chk("no scientific notation from the character column either",
    identical(h$.actaDose(c("7.8125E-2", "1.953125E-2")), c("0.078125", "0.01953125")))
chk("blank and NA render empty", identical(h$.actaDose(c(NA, "", "  ")), c("", "", "")))
chk("a value that is not a number survives verbatim", identical(h$.actaDose("2 to 5"), "2 to 5"))

## Row is UPPER-CASED because $WELLID reports an upper-case letter: a layout typed `a` has already
## cost one class of silently dropped well elsewhere in the pipeline, and here it would split row A.
dlc <- data.frame(PlateID = 1L, Row = c("a", "A"), Column = c(1L, 2L), Dirname = "CD4",
                  Alias = "CD4", StainQty = c(5, 2.5), StainUnit = "uL", StainType = "Stain",
                  stringsAsFactors = FALSE)
chk("a lower-case Row is folded into the upper-case one",
    identical(sort(h$actaPlateWells(dlc)$well), c("A1", "A2")))

cat("=== the plate size is measured, and never shrinks below 96 ===\n")
## "Smallest plate that fits" was the first rule and OQ_Test3 broke it: its wells reach only row E
## and column 8, so it was detected as a 48-well plate when it is physically a 96 with most of it
## unused. A partially filled plate is the NORMAL case for a titration.
for (want in list(c("OQ_Test1", 96), c("OQ_Test2", 96), c("OQ_Test3", 96))) {
  d <- layoutOf(want[1])
  chk(sprintf("%s is a %s-well plate", want[1], want[2]),
      identical(h$actaPlateSize(d)$size, as.integer(want[2])))
}
mkrc <- function(rows, cols) data.frame(Row = rows, Column = cols, stringsAsFactors = FALSE)
chk("a 384 layout grows past the default",   h$actaPlateSize(mkrc(c("A","P"),  c(1,24)))$size == 384L)
chk("a 1536 layout grows further",           h$actaPlateSize(mkrc(c("A","AF"), c(1,48)))$size == 1536L)
chk("a single well still draws a 96",        h$actaPlateSize(mkrc("A", 1))$size == 96L)
chk("a declaration overrides detection",     h$actaPlateSize(mkrc("A", 1), declared = 384)$size == 384L)
chk("an unsupported declaration is a hard error",
    inherits(try(h$actaPlateSize(mkrc("A", 1), declared = 100), silent = TRUE), "try-error"))
chk("past 1536 is a hard error",
    inherits(try(h$actaPlateSize(mkrc("A", 99)), silent = TRUE), "try-error"))

cat("=== row letters are bijective base-26, not single characters ===\n")
## 1536-well plates run A..Z then AA..AF, so match(x, LETTERS) returns NA for "AA" and would
## silently drop the bottom fifth of the plate.
chk("A/H/P/Z", identical(h$actaRowIndex(c("A","H","P","Z")), c(1L, 8L, 16L, 26L)))
chk("AA is 27, AF is 32", identical(h$actaRowIndex(c("AA","AF")), c(27L, 32L)))
chk("lower case resolves the same", identical(h$actaRowIndex("a"), 1L))
chk("junk is NA, not 0", is.na(h$actaRowIndex("1A")))

cat("=== $WELLID parsing tolerates vendor spellings ===\n")
w <- h$actaParseWellId(c("A01", "H12", "P24", "AF48", "B3", "junk", NA))
chk("padded, unpadded and multi-letter rows all parse",
    identical(w$row, c(1L, 8L, 16L, 32L, 2L, NA_integer_, NA_integer_)) &&
      identical(w$col, c(1L, 12L, 24L, 48L, 3L, NA_integer_, NA_integer_)))

cat("=== the figure is a ggplot, one per PlateID ===\n")
L1 <- h$make_plate_layout_great_again(layoutOf("OQ_Test1"), eln = "EXP00000001")
chk("one plate, named by PlateID", length(L1) == 1L && identical(names(L1), "1"))
chk("it is a ggplot", inherits(L1[[1]], "ggplot"))
chk("title is the Benchling id passed in", identical(L1[[1]]$labels$title, "EXP00000001"))
chk("subtitle is the PlateID", identical(L1[[1]]$labels$subtitle, "PlateID: 1"))
chk("caption names StainQty and its unit",
    grepl("Numbers inside the well show StainQty \\(uL\\)", L1[[1]]$labels$caption))
## Four layers: the empty plate frame, the wells, OUR ring, the dose labels. The ring must sit over
## the fill and under the labels, or alpha dims it / the labels vanish beneath it.
chk("four layers, ring third", length(L1[[1]]$layers) == 4L)
chk("the wells carry fill AND alpha",
    all(c("fill", "alpha") %in% names(L1[[1]]$layers[[2]]$mapping)))
chk("the ring layer carries colour at full opacity",
    "colour" %in% names(L1[[1]]$layers[[3]]$mapping) &&
      !("alpha" %in% names(L1[[1]]$layers[[3]]$mapping)))
## "transparent", NOT NA: ggplot2's remove_missing() treats an NA colour as a MISSING aesthetic and
## drops the row, which deleted every well and left only the rings.
chk("the well layer's own colour is transparent, not NA",
    identical(L1[[1]]$layers[[2]]$aes_params$colour, "transparent"))
chk("all three well sizes are pinned, not left to ggplate",
    identical(L1[[1]]$layers[[1]]$aes_params$size, L1[[1]]$layers[[2]]$aes_params$size))
chk("multiple plates yield multiple figures",
    length(h$make_plate_layout_great_again(
      rbind(transform(layoutOf("OQ_Test1"), PlateID = 1),
            transform(layoutOf("OQ_Test1"), PlateID = 2)))) == 2L)
chk("nothing to map yields an empty list, never a half-map",
    identical(h$make_plate_layout_great_again(NULL), list()))

cat("=== label geometry is calibrated, not assumed ===\n")
## `size` is a FONT size: a point at size 10.6 renders an 8.00 mm circle, not 10.6. Text metrics
## also differ by device. Both were wrong before, and both hid overflow.
r <- h$actaWellTextRadius()
chk("the usable radius comes out near 7.5 mm for a 12-column plate", r > 7 && r < 8)
sz <- min(vapply(h$ACTA_PLATE_LABEL_REFS, h$actaFitLabelSize, numeric(1)))
chk("the fitted label size is ~4, not the old 2.28", sz > 3.5 && sz < 4.5)
chk("both design references fit at that size", !any(h$.actaLabelOverflows(h$ACTA_PLATE_LABEL_REFS, sz)))
## The envelope is a 6-character stripped label; 3 arms or two 8-character arms are past it.
chk("a 3-arm label is correctly reported as overflowing",
    isTRUE(h$.actaLabelOverflows(".00977\n.00977\n.00977", sz)))

cat("=== leading zeros are stripped; significant figures are not cut ===\n")
## Stripping buys a character with no precision loss. Cutting to 2 sigfigs would print a 1.25 uL
## well as "1.2", and this figure exists to catch a value that disagrees with the sheet.
chk("0.00977 -> .00977", identical(h$.actaStripLeadZero("0.00977"), ".00977"))
chk("a negative keeps its sign", identical(h$.actaStripLeadZero("-0.5"), "-.5"))
chk("5 and 1.25 are untouched", identical(h$.actaStripLeadZero(c("5","1.25")), c("5","1.25")))
chk("1.25 still shows three significant figures", h$ACTA_PLATE_SIGFIG == 3L)

cat("=== combinatorial wells ===\n")
W3 <- h$actaPlateWells(layoutOf("OQ_Test3"))
e6 <- W3[W3$well == "E6", ]
chk("matched arms print ONE number, not a repeat", identical(e6$dose, ".625"))
chk("the fill level is the set of aliases in the well",
    grepl("CCR7_combo \\+ CD14_combo", e6$compo))
## Mismatched arms: stacked, because "a/b" is 15.89 mm wide and forces the label down to 1.23.
d <- data.frame(PlateID = 1L, Row = "A", Column = c(1L, 1L), Dirname = "X+Y",
                Alias = c("X", "Y"), StainQty = c(5, 1.25), StainUnit = "uL",
                StainType = "Stain", stringsAsFactors = FALSE)
chk("mismatched arms are stacked with a newline",
    identical(h$actaPlateWells(d)$dose, "5\n1.25"))
chk("both arm doses are kept for the alpha ranking",
    identical(h$actaPlateWells(d)$arms, "5|1.25"))

cat("=== a well with no Dirname is not mapped ===\n")
W1 <- h$actaPlateWells(layoutOf("OQ_Test1"))
chk("only the 12 keyed wells survive", nrow(W1) == 12L)
chk("and they are all in row A", all(grepl("^A", W1$well)))

cat("=== the fill palette is explicit, ordered, and never cycled ===\n")
## ggplate's default palette comes from a dataset it loads with data(), which only resolves when the
## package is ATTACHED -- called through the namespace it fails with "Unknown colour name:
## placeholder". Passing our own is what makes it work at all.
## The COUNT and the pairwise-distinguishability property are asserted in their own section
## further down, added in 3.1 along with the sixteen-colour palette -- "eight fixed hues" lived
## here until then and is what caught the change, correctly.
chk("no black (that is the Stain ring) and no yellow (it dies under the alpha floor)",
    !any(toupper(h$ACTA_PLATE_FILL_COL) %in% c("#000000", "#F0E442")))
chk("every hue distinct", !anyDuplicated(h$ACTA_PLATE_FILL_COL))

cat("=== the script exports one PNG per plate, and the report prints the same object ===\n")
scr <- paste(readLines(Sys.glob(file.path(vd, "ACTA_Script_*.R"))[1], warn = FALSE), collapse = "\n")
rmd <- paste(readLines(Sys.glob(file.path(vd, "ACTA_Report_*.Rmd"))[1], warn = FALSE), collapse = "\n")
chk("the script builds PlateMapList", grepl("PlateMapList <- tryCatch", scr))
## PlateMap_<BID>_<PlateID>.png -- the kind leads, as it does for every other run-level figure.
## The PlateID is load-bearing, not decoration: a run can span several plates and without it they
## would overwrite each other.
chk("filename is PlateMap_<BID>_<PlateID>.png",
    grepl('"Plots/PlateMap_", BIDfile, "_"', scr))
chk("ggplate is declared in the script's package list", grepl('"ggplate"', scr))
chk("...and in the report's",                            grepl('"ggplate"', rmd))
chk("the report prints PlateMapList rather than rebuilding it", grepl("PlateMapList\\[\\[", rmd))
chk("ggplate is in the validated-package baseline",
    "ggplate" %in% read.delim(file.path(actaTestCaseRoot(), "validated_packages.tsv"),
                              stringsAsFactors = FALSE)[[1]])

## ---------------------------------------------------------------------------------------------
## MORE THAN EIGHT COMPOSITIONS MUST STILL DRAW. This is the defect 3.1 exists to fix, and the
## reason it went unnoticed for four releases is worth stating: the palette held eight colours,
## ggplate stops with "more categories in the value column than provided colours" at the ninth,
## and the caller turns that into a warning so a figure cannot cost a completed analysis. So a
## real panel -- one whose reagents share COMBINED control wells, where a well holding six
## reagents is its own composition -- produced a run with every number correct and no plate map
## in the report, and the only trace was a warning on the console.
##
## BEHAVIOURAL, through the drawing function, at the counts that matter: the old ceiling, one past
## it, the 13 that was actually reported, and the new cap. A test that only read the length of
## ACTA_PLATE_FILL_COL would have passed for the whole of 3.0 as well.
cat("\n=== a plate with more than eight compositions ===\n")
local({
  ## One reagent per row, six doses across, a Costain at zero -- the shape both shipped cases use.
  mk <- function(reagents) do.call(rbind, lapply(seq_along(reagents), function(i) data.frame(
    PlateID = "P1", Row = LETTERS[i], Column = 1:6,
    Dirname = reagents[i], Alias = reagents[i],
    StainQty = c(0, 0.25, 0.5, 1, 2, 4),
    StainType = c("Costain", rep("Stain", 5)),
    ELN_ID = "EXPGATE", stringsAsFactors = FALSE)))
  ## Named so a failure says which count broke, and including the combined-well shape that
  ## produces these counts in the first place.
  nm <- c(sprintf("R%02d", 1:12), "CD11b+CD11c+CD14+CD5+CD7+CD8", "CD20+CD56+hCD45+mCD45",
          "CD3+CD4+CD8a", "CD19+CD20")
  for (n in c(8L, 9L, 13L, 16L)) {
    md <- mk(nm[seq_len(n)])
    got <- tryCatch({
      r <- h$make_plate_layout_great_again(md, eln = "EXPGATE")
      if (is.list(r) && length(r) >= 1L && inherits(r[[1]], "ggplot")) "drew" else "no plot"
    }, error = function(e) paste("error:", conditionMessage(e)))
    chk(sprintf("%d compositions draw a plate map", n), identical(got, "drew"))
    if (!identical(got, "drew")) cat("          ", got, "\n")
  }
  ## AND THE PLOT MUST ACTUALLY CARRY n FILLS, not silently collapse two compositions into one
  ## colour -- which is the failure the palette length was protecting against in the first place.
  md <- mk(nm[seq_len(13L)])
  pl <- h$make_plate_layout_great_again(md, eln = "EXPGATE")[[1]]
  fills <- unique(pl$data$colours[!is.na(pl$data$colours)])
  chk(sprintf("...and a 13-composition plate uses 13 distinct fills (%d)", length(fills)),
      length(fills) == 13L)
})

## ---------------------------------------------------------------------------------------------
## THE PALETTE ITSELF, as a property rather than a transcription. Checking the literal hex list
## against a copy in the test proves only that two lists match; what matters is that no two
## compositions can read as the SAME colour, which is exactly what slot 8 did before 3.1 --
## #11A579 sat 2.2 OKLab-ΔE from slot 3's #009E73, indistinguishable with full colour vision, so
## an eight-composition plate already showed two categories as one.
cat("\n=== the fill palette is 16 mutually distinguishable colours ===\n")
local({
  pal <- h$ACTA_PLATE_FILL_COL
  chk(sprintf("the palette provides %d fills", h$ACTA_PLATE_FILL_MAX),
      length(pal) == h$ACTA_PLATE_FILL_MAX && h$ACTA_PLATE_FILL_MAX == 16L)
  chk("...none of them repeated", !anyDuplicated(pal))
  ## Slots 1-7 are Okabe-Ito and are FROZEN: any plate with seven or fewer compositions must
  ## render exactly as it did in 3.0, or every archived plate map silently stops matching.
  chk("...slots 1-7 are unchanged from 3.0",
      identical(pal[1:7], c("#0072B2", "#D55E00", "#009E73", "#CC79A7",
                            "#E69F00", "#56B4E9", "#7F3C8D")))
  ## OKLab ΔE, the same metric and floor the project's palette validator applies. ALL pairs, not
  ## adjacent ones: a plate is a map, so any two wells can end up side by side.
  srgb2lin <- function(c) ifelse(c <= 0.04045, c / 12.92, ((c + 0.055) / 1.055) ^ 2.4)
  oklab <- function(hex) {
    v <- srgb2lin(grDevices::col2rgb(hex)[, 1] / 255)
    l <- 0.4122214708 * v[1] + 0.5363325363 * v[2] + 0.0514459929 * v[3]
    m <- 0.2119034982 * v[1] + 0.6806995451 * v[2] + 0.1073969566 * v[3]
    s <- 0.0883024619 * v[1] + 0.2817188376 * v[2] + 0.6299787005 * v[3]
    l_ <- l ^ (1/3); m_ <- m ^ (1/3); s_ <- s ^ (1/3)
    c(0.2104542553 * l_ + 0.7936177850 * m_ - 0.0040720468 * s_,
      1.9779984951 * l_ - 2.4285922050 * m_ + 0.4505937099 * s_,
      0.0259040371 * l_ + 0.7827717662 * m_ - 0.8086757660 * s_)
  }
  L <- lapply(pal, oklab)
  worst <- Inf; who <- ""
  for (i in seq_along(L)) for (j in seq_along(L)) if (i < j) {
    d <- sqrt(sum((L[[i]] - L[[j]]) ^ 2)) * 100
    if (d < worst) { worst <- d; who <- paste(pal[i], pal[j]) }
  }
  ## 15 is the validator's normal-vision floor: below it a pair is hard to tell apart even with
  ## full colour vision. Simulated-CVD separation is NOT asserted -- it cannot be met at sixteen
  ## categories (measured 4.3, floor 6), which is why the legend, the in-well dose and the
  ## StainType ring carry identity and colour is a coarse aid past eight.
  chk(sprintf("...no two fills are closer than 15 OKLab dE (worst %.1f, %s)", worst, who),
      worst >= 15)
})

## ---------------------------------------------------------------------------------------------
## AND PAST THE CAP IT REFUSES, in words aimed at the operator. ggplate's own message names an
## argument the operator has never seen, and it arrived as a warning attached to a missing figure.
cat("\n=== past the cap, a legible refusal ===\n")
local({
  for (n in c(1L, 8L, 16L))
    chk(sprintf("%d compositions is within the cap", n),
        length(tryCatch(h$actaPlateFillFor(n), error = function(e) character(0))) == n)
  msg <- tryCatch({ h$actaPlateFillFor(17L); NA_character_ }, error = conditionMessage)
  chk("17 compositions is refused, not drawn", !is.na(msg))
  ## The message has to carry the three things a person needs: how many they have, what the limit
  ## is, and what to do. Asserted separately so a rewrite cannot quietly drop the remedy.
  chk("...naming the count they have", isTRUE(grepl("17", msg, fixed = TRUE)))
  chk("...naming the limit", isTRUE(grepl("16", msg, fixed = TRUE)))
  chk("...and what to do about it", isTRUE(grepl("PlateID", msg, fixed = TRUE)))
  ## Zero is its own error, not a zero-length palette handed to ggplate.
  ## is.character(NA_character_) is TRUE, so the first version of this passed whether or not the
  ## error was raised -- replacing the `n < 1L` guard with `if (FALSE)` left the file at exit 0.
  ## The sibling eight lines up already had the right shape; this one did not. Twenty-second
  ## assertion of that kind in this project, in a gate THIS release added.
  .msg0 <- tryCatch({ h$actaPlateFillFor(0L); NA_character_ }, error = conditionMessage)
  chk("an empty plate is refused too", !is.na(.msg0))
})

## ---------------------------------------------------------------------------------------------
## THE WELLS MUST NOT OVERLAP. The diameter comes from the panel WIDTH and the column count, so a
## plate needs a matching amount of HEIGHT -- and the figure height was a constant while the
## legend, which eats into it, grows with the number of compositions and the length of their
## names. A real 13-composition run left 5.01 in for a panel needing 6.20 and the wells overlapped
## by 5%. It was never far off: the same plate with short keys cleared by only 4%.
##
## MEASURED THE WAY THE DEFECT WAS FOUND: lay the plot out at the width and height that will be
## used, read the heights that are NOT the panel (the panel is the one in null units, it absorbs
## the remainder), and compare the row pitch that leaves against the fill diameter. A test that
## asserted on the constants could not have seen this, because every constant was unchanged.
cat("\n=== wells never overlap, at any plate shape or legend size ===\n")
local({
  mkPlate <- function(reagents, doses, pooled = list()) {
    ## One reagent per COLUMN, doses down the rows -- the orientation a real titration uses.
    rows <- LETTERS[seq_along(doses)]
    d <- do.call(rbind, lapply(seq_along(reagents), function(j) data.frame(
      PlateID = "1", Row = rows, Column = j + 1L,
      Dirname = reagents[j], Alias = reagents[j], StainQty = doses,
      StainType = c("Costain", rep("Stain", length(doses) - 1L)),
      ELN_ID = "EXPGATE", stringsAsFactors = FALSE)))
    ## POOLED CONTROL WELLS: several reagents sharing one well, which is what makes a composition
    ## label long enough to wrap and the legend tall enough to matter. This is the shape that
    ## produced the overlap in the field.
    for (pw in pooled) {
      d <- d[!(d$Row == pw$row & d$Column == pw$col), , drop = FALSE]
      d <- rbind(d, do.call(rbind, lapply(pw$reagents, function(r) data.frame(
        PlateID = "1", Row = pw$row, Column = pw$col, Dirname = r, Alias = r,
        StainQty = 0, StainType = "Costain", ELN_ID = "EXPGATE", stringsAsFactors = FALSE))))
    }
    d
  }
  ## Names are SYNTHETIC but the same length as real reagent+fluorophore labels, because it is the
  ## label length that decides how a legend key wraps and therefore how tall the legend is.
  mk <- function(i) sprintf("MKR%02d DYE%03d", i, 100 + i * 7)
  short <- sprintf("R%02d", 1:11)

  geom <- function(md) {
    pm <- h$make_plate_layout_great_again(md, eln = "EXPGATE")
    if (!length(pm)) return(NULL)
    p <- pm[[1]]
    rows <- attr(p, "acta_plate_rows"); cols <- attr(p, "acta_plate_cols")
    H <- h$actaPlateFigHeight(p, width = h$ACTA_PLATE_FIG$width)
    grDevices::pdf(NULL, width = h$ACTA_PLATE_FIG$width, height = H)
    on.exit(grDevices::dev.off(), add = TRUE)
    g <- ggplot2::ggplotGrob(p)
    nullOf <- function(u) vapply(seq_along(u),
      function(i) any(grepl("null", as.character(grid::unitType(u[i])))), logical(1))
    hs <- g$heights; ws <- g$widths
    panel  <- H - sum(grid::convertHeight(hs[!nullOf(hs)], "in", valueOnly = TRUE))
    availW <- h$ACTA_PLATE_FIG$width -
                sum(grid::convertWidth(ws[!nullOf(ws)], "in", valueOnly = TRUE))
    gbi <- which(g$layout$name == "guide-box-bottom")
    legW <- if (length(gbi))
              grid::convertWidth(sum(g$grobs[[gbi[1]]]$widths), "in", valueOnly = TRUE) else 0
    fill <- h$actaPlateWellSize(cols) * h$ACTA_PLATE_FILL_RATIO
    list(H = H, rows = rows, cols = cols, pitch = panel / rows * 25.4, fill = fill,
         ratio = fill / (panel / rows * 25.4), legW = legW, availW = availW)
  }

  cases <- list(
    list(nm = "1 reagent, 7 doses (smallest real plate)",
         md = mkPlate(mk(1), c(0, 5, 2.5, 1.25, .625, .312, .156))),
    list(nm = "8 reagents, short names (the old ceiling)",
         md = mkPlate(short[1:8], c(0, 5, 2.5, 1.25, .625, .312, .156))),
    ## THE FIELD CASE'S SHAPE: 11 reagents across columns 2-12, 7 dose rows, and two pooled Costain
    ## wells holding 6 and 4 reagents -> 13 compositions, two of whose labels wrap to 4-5 lines.
    list(nm = "field case: 11 reagents + 2 pooled costains",
         md = mkPlate(mk(1:11), c(0, 5, 2.5, 1.25, .625, .312, .156),
                      pooled = list(list(row = "A", col = 2L, reagents = mk(1:6)),
                                    list(row = "A", col = 4L, reagents = mk(7:10))))),
    list(nm = "16 compositions, all long (the cap)",
         md = mkPlate(mk(1:14), c(0, 5, 2.5, 1.25, .625),
                      pooled = list(list(row = "A", col = 2L, reagents = mk(1:6)),
                                    list(row = "A", col = 3L, reagents = mk(7:12)))))
  )
  for (cs in cases) {
    g <- geom(cs$md)
    if (is.null(g)) { chk(sprintf("%s -- draws at all", cs$nm), FALSE); next }
    chk(sprintf("%s: fill/pitch %.2f", cs$nm, g$ratio), g$ratio < 1)
    if (g$ratio >= 1)
      cat(sprintf("          %dx%d plate, figure %.2f in, pitch %.2f mm, fill %.2f mm\n",
                  g$rows, g$cols, g$H, g$pitch, g$fill))
    ## And the legend must FIT: the StainType key, which is what explains the rings, was clipped
    ## mid-word ("Costai") whenever the two guides side by side wanted more width than the figure
    ## had. Stacking them fixed it; this is what stops it coming back.
    chk(sprintf("...and its legend fits the width (%.2f of %.2f in)", g$legW, g$availW),
        g$legW <= g$availW)
  }
  ## THE FLOOR: a plate that renders correctly today must not change. actaPlateFigHeight() never
  ## returns less than the constant it replaced, so a short-legend plate keeps its 7.6 in.
  g1 <- geom(cases[[1]]$md)
  chk(sprintf("a small plate keeps the original figure height (%.2f in)", g1$H),
      g1$H >= h$ACTA_PLATE_FIG$height)
  ## ...and a big one is genuinely taller, or the measurement is not doing anything.
  g3 <- geom(cases[[3]]$md)
  chk(sprintf("...while a 13-composition plate is made taller (%.2f in)", g3$H),
      g3$H > h$ACTA_PLATE_FIG$height)
  ## The geometry has to travel with the plot, or a caller cannot size the figure at all.
  p1 <- h$make_plate_layout_great_again(cases[[1]]$md, eln = "EXPGATE")[[1]]
  ## Compared by VALUE: the geometry table stores these as doubles and an identical() against
  ## 8L/12L fails on type alone, which says nothing about whether the attribute is there.
  chk("the plate's row and column counts travel with the plot",
      isTRUE(attr(p1, "acta_plate_rows") == 8) && isTRUE(attr(p1, "acta_plate_cols") == 12))
  ## And the height must be SQUARE, not merely non-overlapping: row pitch matching column pitch is
  ## what gives the wells the same 0.85 fill fraction on both axes.
  chk(sprintf("...and the pitch is square (rows %.2f mm vs columns %.2f mm)",
              g3$pitch, h$ACTA_PLATE_PANEL_IN * 25.4 / g3$cols),
      abs(g3$pitch - h$ACTA_PLATE_PANEL_IN * 25.4 / g3$cols) < 0.5)
})

cat(if (ok) "\nPLATE MAP: all checks passed\n" else "\nPLATE MAP: FAILURES ABOVE\n")
quit(status = if (ok) 0L else 1L)
