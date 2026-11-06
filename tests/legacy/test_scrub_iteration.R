## scrub_confidential must remove the lab's PANEL ITERATION NAMES from everything it publishes,
## while keeping the panel's COMPOSITION, which is not secret and is the point of the submission.
##
## The names used below are the real shape of the problem: a lab names an iteration after the
## programme it serves (shaped like "PRG001 Base Panel_1"), so the iteration name carries a
## programme code
## even when every reagent in it is a catalogue part anyone can buy.
##
## THE XLSX IS DECOMPRESSED BEFORE IT IS SEARCHED. An .xlsx is a zip container; grepping the
## container bytes for a leaked string finds nothing whether or not the string is in there, which
## is how an earlier sweep in this project came out clean while the value was sitting in
## sharedStrings. Every check below reads the unpacked XML.

ok <- TRUE
chk <- function(label, pass, detail = NULL) {
  d <- paste(as.character(detail), collapse = " ")
  if (!isTRUE(pass)) { ok <<- FALSE
    cat(sprintf("  [FAIL] %s%s\n", label, if (nzchar(d)) paste0(" -- ", d) else "")) }
  else cat(sprintf("  [ok]   %s\n", label))
}

suppressMessages(library(ACTA))
applyScrub <- ACTA:::actaApplyScrub
writeInv   <- ACTA:::.tpWriteInventory
pcap       <- ACTA:::actaPanelIterationCaption

SECRETS <- c("Unlisted Panel_4", "PRG001 Base Panel_1")

lay <- data.frame(
  Alias                   = c("Strep", "Strep", "CD19", "CD19"),
  Costain_Panel_Iteration = c(SECRETS[1], SECRETS[1], SECRETS[2], SECRETS[2]),
  Operator                = c("AB", "AB", "CD", "CD"),
  stringsAsFactors = FALSE)

scr <- applyScrub(lay, quiet = TRUE)
iters <- unique(scr$Costain_Panel_Iteration)

chk("iteration names are replaced with placeholders",
    all(grepl("^Iteration[0-9]+$", iters)), paste(iters, collapse = ", "))
chk("two distinct panels stay distinct after scrubbing", length(iters) == 2L,
    paste(iters, collapse = ", "))
chk("no original iteration name survives in the frame",
    !any(vapply(SECRETS, function(s) any(grepl(s, unlist(scr), fixed = TRUE)), TRUE)))

map <- attr(scr, "acta_iteration_map")
chk("the originals are kept so the inventory lookup can still resolve them",
    setequal(as.character(map), SECRETS), paste(map, collapse = ", "))
chk("the map is keyed placeholder -> original", all(names(map) %in% iters),
    paste(names(map), collapse = ", "))

## The bundle artefact. panels is keyed the way ACTA_Script.R keys it: by the SCRUBBED name.
panels <- stats::setNames(lapply(iters, function(i) list(table = data.frame(
  Antigen = c("CD3", "CD8"), Fluorochrome = c("BUV805", "Alexa Fluor 532"),
  Vendor = c("BD", "Thermo"), Catalog = c("612893", "58-0088-42"),
  Clone = c("SK7", "RPA-T8"), Type = c("Base", "Base"), stringsAsFactors = FALSE))), iters)

out <- file.path(tempdir(), "scrubtest_Master_Panel_Inventory.xlsx")
unlink(out)
writeInv(panels, out)
chk("the bundle inventory was written", file.exists(out))

ex <- file.path(tempdir(), "scrubtest_unz"); unlink(ex, recursive = TRUE)
utils::unzip(out, exdir = ex)
xml <- unlist(lapply(list.files(ex, recursive = TRUE, full.names = TRUE),
                     function(f) tryCatch(readLines(f, warn = FALSE), error = function(e) character(0))))
blob <- paste(xml, collapse = "\n")

for (s in SECRETS)
  chk(sprintf("'%s' does not appear in the unpacked bundle inventory", s),
      !grepl(s, blob, fixed = TRUE))
chk("the placeholders DO appear, so the sweep is looking at real content",
    all(vapply(iters, function(i) grepl(i, blob, fixed = TRUE), TRUE)),
    paste(iters, collapse = ", "))
chk("the panel COMPOSITION is still published", grepl("612893", blob, fixed = TRUE))

## The caption names its source file, so under scrub that name must be the bundle's, not the
## lab's. ACTA_Script.R rewrites r$file; this asserts the caption honours it.
r <- list(status = "ok", iteration = "Iteration1", reagents = "Strep",
          file = "Master_Panel_Inventory.xlsx", n_matched = 2, n_total = 2)
cap <- pcap(r)
chk("a scrubbed caption names the bundle inventory",
    grepl("Master_Panel_Inventory.xlsx", cap, fixed = TRUE), cap)
chk("...and carries no original panel name",
    !any(vapply(SECRETS, function(s) grepl(s, cap, fixed = TRUE), TRUE)), cap)

cat(if (ok) "\nSCRUB ITERATION TESTS PASSED\n" else "\nFAILURES ABOVE\n")
quit(status = if (ok) 0L else 1L)
