## Guards make_plate_layout_great_again(), the "Plate map" section added in 2_98 and
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
  f <- Sys.glob(file.path(vd, "Diagnostics", case, "*Instructions*.xlsx"))
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
chk("eight fixed hues", length(h$ACTA_PLATE_FILL_COL) == 8L)
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
    "ggplate" %in% read.delim(file.path(vd, "Diagnostics", "validated_packages.tsv"),
                              stringsAsFactors = FALSE)[[1]])

cat(if (ok) "\nPLATE MAP: all checks passed\n" else "\nPLATE MAP: FAILURES ABOVE\n")
quit(status = if (ok) 0L else 1L)
