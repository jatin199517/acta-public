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


cat("=== Info!scrub_eln_placeholder ===\n")
## WHY THIS EXISTS. The default replaces every run's ELN_ID with one constant, which is right for
## hiding it and wrong for a SET of datasets published together: six bundles all reading
## EXP26000001 give a reviewer no way to match a DOI to the dataset in front of them.
##
## Both halves are asserted. The resolver has to DEFAULT when unset -- otherwise every existing
## workbook changes behaviour -- and it has to be the SAME value on the export side and the
## workbook side, because a disagreement would publish an instructions sheet and an export naming
## different experiments, which no structural check looks for.
local({
  pl <- ACTA:::actaScrubElnPlaceholder
  dflt <- unname(ACTA:::ACTA_SCRUB_CONFIDENTIAL[["ELN_ID"]])
  mk <- function(v) data.frame(Variable = "scrub_eln_placeholder", Value = v,
                               stringsAsFactors = FALSE)
  chk("unset keeps the shipped constant",      identical(pl(NULL), dflt))
  chk("blank keeps it too",                    identical(pl(mk("")), dflt))
  chk("the literal NA keeps it too",           identical(pl(mk("NA")), dflt))
  chk("a value overrides it",                  identical(pl(mk("EXP1240")), "EXP1240"))
  chk("...and is trimmed",                     identical(pl(mk("  EXP1460  ")), "EXP1460"))
  chk("a label-length value is REFUSED",
      inherits(try(pl(mk(strrep("x", 65))), silent = TRUE), "try-error"))

  ## The export side must honour it, and must still default without it.
  ## ELN_ID holds a DELIBERATELY UNREALISTIC value. actaApplyScrub() replaces this column by NAME,
  ## never by the shape of what is in it, so a realistic EXP######## here would test nothing extra
  ## -- and it tripped the tracked-text leak sweep in verify_oq_sanitised.R, which flags ELN-shaped
  ## strings precisely because they look real. The declared-fixture allowance there covers one file
  ## only; widening it to license a string that carries no test meaning is the wrong trade.
  df <- data.frame(Operator = "Someone", ELN_ID = "ELN_ID_TO_REPLACE", ELN_Name = "Benchling",
                   stringsAsFactors = FALSE)
  chk("actaApplyScrub() uses the override",
      identical(ACTA:::actaApplyScrub(df, quiet = TRUE, eln = "EXP1240")$ELN_ID, "EXP1240"))
  chk("...and defaults when it is not given",
      identical(ACTA:::actaApplyScrub(df, quiet = TRUE)$ELN_ID, dflt))
  chk("...while Operator and ELN_Name are unaffected by it",
      identical(ACTA:::actaApplyScrub(df, quiet = TRUE, eln = "EXP1240")$Operator, "JD") &&
      identical(ACTA:::actaApplyScrub(df, quiet = TRUE, eln = "EXP1240")$ELN_Name, "ELN"))

  ## THE ONE WAY THIS FEATURE CAN DEFEAT THE SCRUB IT BELONGS TO: a placeholder that is already the
  ## run's own ELN_ID republishes it, and all three downstream checks decline to look (see the
  ## function's comment). Nothing else in the suite would notice, because nothing else is wrong.
  warned <- function(...) {
    w <- FALSE
    withCallingHandlers(pl(...), warning = function(c) { w <<- TRUE; invokeRestart("muffleWarning") })
    w
  }
  chk("a placeholder equal to the real ELN_ID WARNS",
      warned(mk("EXP1240"), current = c("EXP1240", "EXP1240")))
  chk("...and still returns the value, rather than refusing the run",
      identical(suppressWarnings(pl(mk("EXP1240"), current = "EXP1240")), "EXP1240"))
  ## Both values merely have to DIFFER from the placeholder; neither needs to look like a real
  ## ELN id, and a realistic EXP######## here trips the tracked-text sweep in
  ## verify_oq_sanitised.R -- which is the same mistake, in the same file, twice.
  chk("...and is quiet when it differs",
      !warned(mk("EXP1240"), current = c("EXP_1240", "SOME_OTHER_ID")))
  chk("...quiet when no current id is known",   !warned(mk("EXP1240")))
  chk("...quiet when current is all NA/blank",
      !warned(mk("EXP1240"), current = c(NA, "", "  ")))
  ## Trimmed on BOTH sides: a cell storing a trailing space is the commonest way a real value
  ## differs from what it displays, and that is exactly how this check would silently miss.
  chk("...matched after trimming the current id",
      warned(mk("EXP1240"), current = "  EXP1240 "))
  ## The default constant must not start warning for every run that leaves the row blank.
  chk("the shipped constant does not warn against itself",
      !warned(mk(""), current = dflt))

  ## THIS VALUE BECOMES PART OF FILE NAMES. .tpStageFcs() cleans "/" and ":" out of each leaf
  ## BEFORE .tpRenameToken() applies the map, so a placeholder bypasses that guard completely:
  ## "../.." escapes the staging directory, where dir.create(recursive = TRUE) and
  ## write.FCS/file.copy(overwrite = TRUE) then act on the escaped path, and the same value names
  ## the bundle zip. Two sinks, one check.
  refused <- function(v) inherits(try(pl(mk(v)), silent = TRUE), "try-error")
  chk("a traversal placeholder is REFUSED",        refused("../../../../tmp/acta_pwn"))
  chk("a bare slash is refused",                   refused("EXP/1240"))
  chk("a backslash is refused",                    refused("EXP\\1240"))
  chk("a colon is refused (unpackable on Windows)", refused("CD19:v2"))
  chk("a NUL-ish control character is refused",    refused("EXP\n1240"))
  chk("a space is refused",                        refused("EXP 1240"))
  ## ...and the shapes a real site actually uses must still pass, or this is a fail-always gate.
  chk("plain ids still pass",        identical(pl(mk("EXP1240")), "EXP1240"))
  chk("underscores still pass",      identical(pl(mk("EXPOQ_Test1")), "EXPOQ_Test1"))
  chk("dots and hyphens still pass", identical(pl(mk("EXP-1240.2")), "EXP-1240.2"))
  chk("the shipped constant itself still passes",  identical(pl(mk(dflt)), dflt))
})

## ---------------------------------------------------------------------------------------------
## .tpScrubMap() TAKES THE PUBLISHED ID, IT DOES NOT RE-DERIVE IT.
##
## It used to read Info!scrub_eln_placeholder from the workbook on disk. That re-read was wrong
## three ways -- a different col_types from ACTA_Script.R's read (a DATE cell resolved to its
## serial), a stop() swallowed by its own error handler into an empty map that reports success,
## and a `current` comparison against an already-scrubbed export sheet that warned every run.
## These assert the value is honoured and that the Info sheet is no longer consulted.
local({
  cat("\n=== .tpScrubMap() honours the id it is given ===\n")
  ## From the INSTALLED package, so this resolves under both layouts without the test-path helper.
  wb <- system.file("extdata/oq_small/OQ_Test1/OQ_Test1_Ab_Titration_Instructions.xlsx",
                    package = "ACTA")
  if (!nzchar(wb) || !file.exists(wb)) {
    cat("  [skip] the OQ_Test1 workbook does not resolve from the installed package\n")
  } else {
    dflt <- unname(ACTA:::ACTA_SCRUB_CONFIDENTIAL[["ELN_ID"]])
    real <- {
      lp <- suppressMessages(readxl::read_excel(wb, sheet = "Layout_Plate", col_types = "text"))
      names(lp) <- ACTA:::actaStripHeaderAnnotation(names(lp))
      unique(trimws(stats::na.omit(as.character(lp[["ELN_ID"]]))))[1]
    }
    m_given <- ACTA:::.tpScrubMap(wb, eln = "EXP1240")
    m_null  <- ACTA:::.tpScrubMap(wb)
    chk("a supplied id becomes the ELN_ID replacement",
        identical(unname(m_given[[real]]), "EXP1240"))
    chk("...and NULL falls back to the shipped constant",
        identical(unname(m_null[[real]]), dflt))
    ## The old code read Info with col_types="text" and would have resolved whatever that sheet
    ## said. The supplied value must win outright, regardless of the workbook's own Info row.
    chk("the supplied id wins over anything the workbook's Info sheet holds",
        !identical(unname(m_given[[real]]), unname(m_null[[real]])))
    ## A bad placeholder must never reach here as an empty map reported as success: the resolver
    ## is not called on this path at all now, so a map built here is always populated.
    chk("the map is non-empty, so .tpScrubWorkbook cannot read it as 'nothing to do'",
        length(m_given) > 0L)
  }
})

cat(if (ok) "\nSCRUB ITERATION TESTS PASSED\n" else "\nFAILURES ABOVE\n")
quit(status = if (ok) 0L else 1L)
