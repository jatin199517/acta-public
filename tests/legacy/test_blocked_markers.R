## actaAssertBundleAllowed(): the gate that refuses to publish a panel carrying a construct.
##
## WHY IT MATTERS MORE THAN THE LEAK SCAN. Everything else in the bundle path can be fixed by
## renaming -- an operator code becomes JD, an ELN id becomes a placeholder. A detection reagent
## for an engineered receptor cannot: the PANEL COMPOSITION is the disclosure, and no amount of
## scrubbing removes it. This is the only control for that, and until 3.6.11 it had NO TEST.
##
## It also had four ways to pass a panel it was written to refuse, every one of them the same
## shape -- a reader that cannot read reporting "nothing found":
##
##   1. $PnS ONLY. Which keyword holds the marker is the acquisition template's choice. A file
##      with the marker in $PnN and $PnS blank read as a panel with no markers at all.
##   2. SILENT TRUNCATION AT 50 FILES. A 96-well plate is 96 files; a blocked reagent in the back
##      half of the plate was never looked at, and the gate said yes.
##   3. AN UNREADABLE FCS CONTRIBUTED SILENCE. Indistinguishable from a clean one.
##   4. AN UNEVALUABLE PATTERN RETURNED NO HITS. A site whose construct regex had a stray bracket
##      published the construct with the gate green -- the exact rule .bsHits() states for the
##      leak scan, not applied here.
##
## Every one is asserted in BOTH directions below: the gate must also still pass a clean panel,
## or it is just a different way to be useless.

ok <- TRUE
chk <- function(label, pass, detail = NULL) {
  r <- tryCatch(list(v = isTRUE(pass), d = NULL),
                error = function(e) list(v = FALSE, d = paste("ERROR:", conditionMessage(e))))
  d <- r$d %||% tryCatch(paste(as.character(detail), collapse = " "), error = function(e) "")
  if (!isTRUE(r$v)) { ok <<- FALSE
    cat(sprintf("  [FAIL] %s%s\n", label, if (nzchar(d)) paste0(" -- ", d) else "")) }
  else cat(sprintf("  [ok]   %s\n", label))
}
`%||%` <- function(a, b) if (is.null(a)) b else a

suppressMessages(library(ACTA))
suppressMessages(library(flowCore))

allow   <- ACTA:::actaAssertBundleAllowed
markers <- ACTA:::actaFcsMarkerNames

## pdftools gates the whole function before any marker is read (a bundle whose report PDF cannot
## be read must not be published), so without it every assertion below would fail on that message
## instead of the one under test.
if (!requireNamespace("pdftools", quietly = TRUE)) {
  cat("pdftools is not installed -- actaAssertBundleAllowed() stops on that first.\n")
  cat("BLOCKED MARKER TESTS SKIPPED\n"); quit(status = 0)
}

root <- file.path(tempdir(), paste0("bm_", basename(tempfile(""))))
dir.create(root, recursive = TRUE)

## A minimal real FCS, written by flowCore, so the gate reads a genuine TEXT segment rather than a
## stub this test invented. `stain` of NA leaves $PnS off entirely -- the template that was never
## given stain names, which is case 1.
mkfcs <- function(nm, chan, stain) {
  m <- matrix(runif(20 * length(chan)), ncol = length(chan),
              dimnames = list(NULL, chan))
  ff <- flowCore::flowFrame(m)
  pd <- flowCore::pData(flowCore::parameters(ff))
  pd$desc <- stain
  flowCore::pData(flowCore::parameters(ff)) <- pd
  f <- file.path(root, nm)
  suppressWarnings(flowCore::write.FCS(ff, f))
  f
}

PAT   <- "anti[- ]?ACME[0-9]"
clean <- mkfcs("clean.fcs",  c("FSC-A", "BV421-A"), c(NA, "CD4"))
inS   <- mkfcs("in_pns.fcs", c("FSC-A", "BV421-A"), c(NA, "anti-ACME1 idiotype"))
## Case 1: the marker is the CHANNEL name and there is no stain name at all.
inN   <- mkfcs("in_pnn.fcs", c("FSC-A", "anti-ACME1-A"), c(NA, NA))

cat("=== what the header read actually sees ===\n")
chk("a stain name is read", "CD4" %in% markers(clean), paste(markers(clean), collapse = ", "))
chk("a CHANNEL name is read too, which is what $PnS-only missed",
    any(grepl("anti-ACME1", markers(inN), fixed = TRUE)), paste(markers(inN), collapse = ", "))

cat("=== the gate ===\n")
stopped <- function(...) {
  e <- tryCatch({ allow(...); NULL }, error = function(e) e)
  if (inherits(e, "error")) conditionMessage(e) else ""
}

chk("a clean panel is allowed", !nzchar(stopped(clean, TRUE, PAT)), stopped(clean, TRUE, PAT))
chk("no patterns configured -> allowed", !nzchar(stopped(inS, TRUE, character(0))),
    stopped(inS, TRUE, character(0)))
chk("titerpubs_export off -> not even checked", !nzchar(stopped(inS, FALSE, PAT)),
    stopped(inS, FALSE, PAT))
chk("a blocked marker in $PnS STOPS the run",
    grepl("must not be published", stopped(inS, TRUE, PAT), fixed = TRUE), stopped(inS, TRUE, PAT))
chk("...and the message names the marker and the pattern",
    grepl("anti-ACME1 idiotype", stopped(inS, TRUE, PAT), fixed = TRUE) &&
      grepl(PAT, stopped(inS, TRUE, PAT), fixed = TRUE), stopped(inS, TRUE, PAT))

cat("-- 1. the marker in $PnN, with no stain name --\n")
chk("a blocked marker in the CHANNEL name STOPS the run",
    grepl("must not be published", stopped(inN, TRUE, PAT), fixed = TRUE), stopped(inN, TRUE, PAT))

cat("-- 2. the back half of a 96-well plate --\n")
## The cap was 50 and truncation was silent. 60 clean files followed by the blocked one is the
## realistic arrangement: the costain is the same all plate long, so the blocked reagent is in
## EVERY file -- but a run where the construct well sits late is exactly the one this missed.
many <- c(rep(clean, 60), inS)
chk("a blocked marker past the old 50-file cap STOPS the run",
    grepl("must not be published", stopped(many, TRUE, PAT), fixed = TRUE),
    substr(stopped(many, TRUE, PAT), 1, 120))
chk("...and exceeding max_files REFUSES rather than scanning part of the set",
    grepl("exceeds max_files", tryCatch({ markers(many, max_files = 10L); "" },
                                        error = function(e) conditionMessage(e)), fixed = TRUE))

cat("-- 3. an FCS the reader cannot parse --\n")
bad <- file.path(root, "truncated.fcs")
writeLines("this is not an FCS file", bad)
chk("an unreadable FCS STOPS the gate rather than contributing silence",
    grepl("could not read the FCS header", stopped(c(clean, bad), TRUE, PAT), fixed = TRUE),
    substr(stopped(c(clean, bad), TRUE, PAT), 1, 160))
chk("...while the non-gate read still tolerates it",
    identical(markers(c(clean, bad)), markers(clean)),
    paste(markers(c(clean, bad)), collapse = ", "))
## The strict reader fires first here, which is the better message of the two -- it names the file.
## The "no parameter names could be read" stop behind it is the backstop for the other route to an
## empty read: files that parse but whose $PnS and $PnN are all blank.
chk("a run where NOTHING could be read is refused, not passed",
    nzchar(stopped(bad, TRUE, PAT)) &&
      grepl("could not read the FCS header", stopped(bad, TRUE, PAT), fixed = TRUE),
    substr(stopped(bad, TRUE, PAT), 1, 160))
## THE "no parameter names could be read" STOP IS NOW UNREACHABLE FROM A READABLE FILE, and that
## is the point of reading $PnN: the FCS standard requires it for every parameter, so a file that
## parses always yields at least the detector names. Pinned here because it is the REASON the
## empty-read branch cannot fire -- if a future change narrows the read back to $PnS, this fails
## and the branch becomes live again. A file with no stain names at all is the probe.
blank <- mkfcs("blank.fcs", c("FSC-A", "SSC-A"), c(NA, NA))
chk("a file with no stain names still yields its parameter names",
    length(markers(blank)) == 2L, paste(markers(blank), collapse = ", "))
chk("...so a readable run is never an empty read, and is judged on its panel",
    !nzchar(stopped(blank, TRUE, PAT)), stopped(blank, TRUE, PAT))

cat("-- 4. a site pattern that will not compile --\n")
## A literal copied out of a construct name -- `ACME-123 (Gen2` with the bracket lost in the paste
## -- is a PCRE compile error. It used to return no hits, which the gate read as "publishable".
chk("an invalid blocked-marker pattern STOPS the run",
    grepl("could not be evaluated", stopped(inS, TRUE, "ACME-123 (Gen2"), fixed = TRUE),
    substr(stopped(inS, TRUE, "ACME-123 (Gen2"), 1, 160))
## The decisive half: the panel here is CLEAN, so the old code returned invisible(NULL) and the
## bundle was built. A broken pattern must make the gate refuse, not shrug.
chk("...and a CLEAN panel is refused too, rather than passing on a pattern that never ran",
    { m <- stopped(clean, TRUE, "ACME-123 (Gen2")
      nzchar(m) && grepl("could not be evaluated", m, fixed = TRUE) },
    substr(stopped(clean, TRUE, "ACME-123 (Gen2"), 1, 160))

cat(if (ok) "\nBLOCKED MARKER TESTS PASSED\n" else "\nFAILURES ABOVE\n")
if (!ok) quit(status = 1)
