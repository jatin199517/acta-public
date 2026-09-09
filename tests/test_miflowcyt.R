## Guards actaMiFlowCyt(), the MiFlowCyt metadata assembler.
##
## Why this exists: the assembler was written, left with ZERO call sites and ZERO tests, and my own
## notes recorded it as "tested" for a day. It is the single source for two consumers -- the report
## typesets it, the FlowRepository export will serialise it -- so a silent change here would put wrong
## metadata into a public submission. Data only: nothing below renders anything.
##
## Rscript <this> [version_dir]   -- defaults to 2_98_WIP
## One cwd contract for the whole suite -- see acta_test_paths.R. Run this script from anywhere.
source(file.path(this.path::this.dir(), "acta_test_paths.R"))
vd <- actaTestVersionDir()
cat("version_dir:", basename(vd), "\n")
h <- actaTestFunctions(vd)
ok <- TRUE
chk <- function(what, cond) { if (!isTRUE(cond)) ok <<- FALSE
                              cat(sprintf("  [%s] %s\n", if (isTRUE(cond)) "ok" else "FAIL", what)) }

## A minimal `stats`-shaped frame: the layout columns the assembler reads, nothing else.
st <- data.frame(
  ELN_ID = "EXP00000001", ELN_Name = "Benchling", Operator = "OP",
  Acquisition_date = "2026-07-30", Equipment = "Aurora (SN0000)", Test_Material = "PBMC",
  e6_cells_per_well = "1", Well_volume_uL = "100", SOP = "001", Fix = "FALSE", Perm = "FALSE",
  ## `Fl` is the RESOLVED marker channel and `Channel` the layout's own column. Channel is deliberately
  ## absent here: that is the common case (the user named the marker instead), and it is what makes the
  ## appendix fall back to Fl for the detector rather than printing NA.
  Fl = "PE-A", Markername = "Strep", Alias = "Strep_PE", Clone = "CLONE", Vendor = "Acme",
  Cat_Num = "101-101", Lot_Num = "1001", StainQty = c("1", "2"),
  Dirname = "Strep_PE", name = c("a.fcs", "b.fcs"), stringsAsFactors = FALSE)
info <- data.frame(compensation = NA, keywords = NA, stringsAsFactors = FALSE)
gt <- data.frame(alias = c("Cells", "Live", "#Reagent", NA, "Reagent"),
                 pop = c("+", "-", "+/-", NA, "+/-"), stringsAsFactors = FALSE)
gsx <- data.frame(Population = c("Cells", "Live"), count = c(10L, 9L), stringsAsFactors = FALSE)

m <- h$actaMiFlowCyt(stats = st, info = info, gating_template = gt, qcList = list(list(1, 2, 3)),
                     gate_stats = gsx, comp = list(mode = "none", raw = NA),
                     trans_args = list(maxValue = 4e6), max_value = 4e6,
                     script_version = "2_88", representative_fcs = NA_character_,
                     panel = data.frame(detector = "PE-A", marker = "Strep",
                                        stringsAsFactors = FALSE),
                     downsample = h$actaDownsampleReport(12000L, c(a = 12000L, b = 12000L)))

cat("=== the five MiFlowCyt sections, plus the standard block ===\n")
for (s in c("standard", "experiment", "specimen", "instrument", "reagents", "analysis"))
  chk(sprintf("section '%s' present", s), !is.null(m[[s]]))

cat("=== honesty about completeness ===\n")
## The single most important field: it is what stops a consumer presenting this as a compliant
## MiFlowCyt submission when six items are deferred to the ELN.
chk("completeness says structured-not-complete",
    identical(m$standard$completeness, "structured-not-complete"))
chk("the note says it is not self-contained", grepl("not a compliant submission", m$standard$note,
                                                    ignore.case = TRUE))
chk("standard is named and versioned",
    identical(m$standard$name, "MiFlowCyt") && identical(m$standard$version, "1.0"))

cat("=== keywords are never invented ===\n")
chk("blank Info!keywords -> NA, not a guess", is.na(m$experiment$keywords))
m2 <- h$actaMiFlowCyt(st, data.frame(keywords = "titration, anonymised", stringsAsFactors = FALSE))
chk("a supplied value is carried verbatim", identical(m2$experiment$keywords, "titration, anonymised"))

cat("=== the ELN reference, and where it comes from ===\n")
## ELN_Name/ELN_ID left the Info sheet on 2026-08-19; they are layout columns now, so the reference
## has to resolve from `stats`.
chk("ELN ref built from the LAYOUT, not Info",
    grepl("EXP00000001", m$experiment$purpose) && grepl("Benchling", m$experiment$purpose))
chk("deferred items point at the ELN rather than being blank",
    identical(m$experiment$organization, m$experiment$purpose) &&
      identical(m$specimen$source, m$experiment$purpose))
m3 <- h$actaMiFlowCyt(st[, setdiff(names(st), c("ELN_ID", "ELN_Name"))], info)
chk("no ELN anywhere -> the documented placeholder, still not blank",
    identical(m3$experiment$purpose, h$ACTA_MIFLOWCYT_DEFERRED))

cat("=== specimen and analysis content ===\n")
chk("SOP lands in specimen treatment (MiFlowCyt 2.3)", identical(m$specimen$treatment$sop, "001"))
## Protocol cites BOTH: the SOP is the controlled document, the ELN entry holds the run's deviations.
## Naming only one sends a reader to the wrong place.
chk("protocol cites the SOP and the ELN",
    grepl("SOP 001", m$specimen$treatment$protocol, fixed = TRUE) &&
      grepl("EXP00000001", m$specimen$treatment$protocol, fixed = TRUE))
chk("protocol falls back to the ELN alone when there is no SOP",
    identical(h$actaMiFlowCyt(st[, setdiff(names(st), "SOP")], info)$specimen$treatment$protocol,
              m$experiment$purpose))
chk("test material read from the layout", identical(m$specimen$characteristics$test_material, "PBMC"))
chk("data files listed once each", nrow(m$analysis$data_files) == 2)
chk("gating statistics carried through", nrow(m$analysis$gating$statistics) == 2)
chk("transformation arguments carried", identical(m$analysis$transformation$max_value, 4e6))
chk("software records the ACTA version", identical(m$analysis$software$acta_version, "2_88"))

cat("=== commented-out template rows are not reported as populations ===\n")
## Reading the raw sheet would list '#Reagent' and an NA as gated populations.
chk("only live aliases", identical(sort(m$analysis$gating$populations),
                                   sort(c("Cells", "Live", "Reagent"))))

cat("=== downsampling is recorded as a preprocessing step ===\n")
chk("downsample slot present", !is.null(m$analysis$downsample))
chk("it states the cap", grepl("12,000", m$analysis$downsample$text))
m4 <- h$actaMiFlowCyt(st, info)
chk("absent downsample record reads 'Not recorded.', not silence",
    identical(m4$analysis$downsample$text, "Not recorded."))
m5 <- h$actaMiFlowCyt(st, info, downsample = h$actaDownsampleReport(NA_integer_, c(a = 60000L)))
chk("no downsampling says so explicitly",
    identical(m5$analysis$downsample$text, "No downsampling performed."))

cat("=== the appendix table (what miflowcyt_export writes into the export) ===\n")
## The appendix numbers per MIFLOWCYT 1.0, which is NOT the report's numbering: the standard has FOUR
## sections, with reagents at 2.4 and data analysis at 4. Checked against ISAC's checklist and the
## published example annotation. An appendix that does not match the standard is useless for a
## submission, so this pins the numbering.
ap <- h$actaMiFlowCytAppendix(m)
chk("returns an Item / Description / Value table",
    identical(names(ap), c("Item", "Description", "Value")))
chk("four MIFlowCyt 1.0 top-level sections, not five",
    identical(intersect(c("1","2","3","4","5"), trimws(ap$Item)), c("1","2","3","4")))
chk("reagents sit at 2.4, under Specimen -- not a section of their own",
    any(trimws(ap$Item) == "2.4") &&
      grepl("Fluorescence Reagent", ap$Description[trimws(ap$Item) == "2.4"]))
chk("data analysis is section 4",
    grepl("Data Analysis", ap$Description[trimws(ap$Item) == "4"], ignore.case = TRUE))
chk("one row per titrated reagent", any(trimws(ap$Item) == "2.4.1"))
chk("the titrated row names the detector, not NA",
    grepl("detector PE-A", ap$Value[trimws(ap$Item) == "2.4.1"], fixed = TRUE))
chk("completeness is carried into the appendix too",
    any(grepl("structured-not-complete", ap$Value, fixed = TRUE)))
chk("a literal \"NA\" cell reads as not supplied, not as the text NA", {
  stNA <- st; stNA$Well_volume_uL <- "NA"
  a2 <- h$actaMiFlowCytAppendix(h$actaMiFlowCyt(stNA, info))
  grepl("(not supplied)", a2$Value[trimws(a2$Item) == "2.2"], fixed = TRUE) })
chk("NULL metadata yields no table rather than an error", is.null(h$actaMiFlowCytAppendix(NULL)))

cat(if (ok) "\nALL MIFLOWCYT TESTS PASS\n" else "\nFAILURES ABOVE\n")
quit(status = if (ok) 0 else 1)
