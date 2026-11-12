## actaRestorePData(): every pData string GatingSet() rewrites comes back, except a renumbering.
##
## WHY THIS FILE EXISTS. A published bundle reported the plate row of its CD62L group as "FALSE".
## The group was the only one sitting entirely in plate row F, so GatingSet() saw a pData column
## whose every value was "F", reparsed it as logical, and handed back "FALSE". No measured value
## was wrong and nothing errored -- the export simply stated a plate row that does not exist.
##
## The repair already existed. It allow-listed ONE mangling (zero-padded identifiers), so it could
## only ever catch the fault somebody had already met. This gate inverts that: it drives the REAL
## flowWorkspace with one column per known mangling shape and asserts the whole table at once, so
## narrowing the guard again fails here rather than in somebody's published dataset.
##
## The two benign cases are asserted just as hard. An earlier blanket restore undid Excel's
## scientific notation being normalised ("3.90625E-2" -> "0.0390625"), which is an IMPROVEMENT for
## anyone reading the export; the fix for one cosmetic problem introduced another, and the
## regression gate caught it. Both directions are pinned below.

ok <- TRUE
chk <- function(label, pass, detail = NULL) {
  d <- paste(as.character(detail), collapse = " ")
  if (!isTRUE(pass)) { ok <<- FALSE
    cat(sprintf("  [FAIL] %s%s\n", label, if (nzchar(d)) paste0(" -- ", d) else "")) }
  else cat(sprintf("  [ok]   %s\n", label))
}

suppressMessages({library(ACTA); library(flowCore); library(flowWorkspace)})
restore <- ACTA:::actaRestorePData

cat(sprintf("flowWorkspace %s\n", as.character(utils::packageVersion("flowWorkspace"))))

## ---- the table ------------------------------------------------------------------------------
## keep = TRUE  -> GatingSet()'s version is an improvement and must SURVIVE the restore
## keep = FALSE -> corruption; the layout's own string must come back
PROBES <- list(
  list(nm = "row_F_all",      v = c("F", "F", "F"),                      keep = FALSE),
  list(nm = "row_T_all",      v = c("T", "T", "T"),                      keep = FALSE),
  list(nm = "zero_padded",    v = c("001", "002", "003"),                keep = FALSE),
  list(nm = "zero_pad_neg",   v = c("-007", "-008", "-009"),             keep = FALSE),
  list(nm = "hex_like",       v = c("0x1A", "0x1B", "0x1C"),             keep = FALSE),
  list(nm = "sci_notation",   v = c("3.90625E-2", "7.8125E-2", "1.5625E-1"), keep = TRUE),
  list(nm = "leading_plus",   v = c("+42", "+43", "+44"),                keep = TRUE),
  ## PAST 2^53 A DOUBLE STOPS BEING ABLE TO TELL THESE APART. GatingSet() hands back a DIFFERENT
  ## digit string, and an equality test on the parsed numbers calls it benign -- so a lot number
  ## of this shape used to publish mangled. Both of these are measured, not invented.
  list(nm = "long_int_17",    v = c("20250101123456789", "20250101123456790",
                                    "20250101123456791"),                keep = FALSE),
  list(nm = "round_1e15",     v = c("1000000000000000", "2000000000000000",
                                    "3000000000000000"),                 keep = FALSE),
  ## untouched by GatingSet(); here so a restore that over-reaches is caught too
  list(nm = "row_mixed",      v = c("A", "B", "F"),                      keep = TRUE),
  list(nm = "well_ids",       v = c("F2", "F3", "F4"),                   keep = TRUE),
  list(nm = "true_false",     v = c("TRUE", "FALSE", "TRUE"),            keep = TRUE),
  list(nm = "lower_t_f",      v = c("t", "f", "t"),                      keep = TRUE),
  list(nm = "plain_int",      v = c("1", "2", "3"),                      keep = TRUE),
  list(nm = "decimal",        v = c("0.625", "1.25", "2.5"),             keep = TRUE),
  list(nm = "cat_num",        v = c("58-0088-42", "58-0088-43", "58-0088-44"), keep = TRUE),
  list(nm = "date_like",      v = c("2026-09-17", "2026-09-18", "2026-09-19"), keep = TRUE))

set.seed(1)
mk <- function() flowFrame(matrix(stats::rnorm(60, 1000, 100), ncol = 2,
                                  dimnames = list(NULL, c("FSC-A", "SSC-A"))))
ffs <- stats::setNames(lapply(1:3, function(i) mk()), paste0("s", 1:3))
fs  <- as(ffs, "flowSet")
pd  <- data.frame(name = names(ffs), stringsAsFactors = FALSE)
for (p in PROBES) pd[[p$nm]] <- p$v
rownames(pd) <- names(ffs)
flowCore::pData(fs) <- pd

gs <- flowWorkspace::GatingSet(fs)
raw <- flowWorkspace::pData(gs)

## ---- the fault is REAL on this flowWorkspace, before anything is asserted about the fix -------
## A gate that passes because the mangling did not happen is worthless. Pin the premise.
mangled <- vapply(PROBES, function(p) !identical(p$v, as.character(raw[[p$nm]])), TRUE)
names(mangled) <- vapply(PROBES, `[[`, "", "nm")
chk("GatingSet() still rewrites the row-F column (the premise of this gate)",
    isTRUE(mangled[["row_F_all"]]), paste(as.character(raw$row_F_all), collapse = ","))
chk("GatingSet() still normalises scientific notation (the premise of the exception)",
    isTRUE(mangled[["sci_notation"]]), paste(as.character(raw$sci_notation), collapse = ","))

gs  <- restore(gs, fs, quiet = TRUE)
got <- flowWorkspace::pData(gs)

for (p in PROBES) {
  a <- p$v
  b <- as.character(got[[p$nm]])
  want <- if (p$keep) as.character(raw[[p$nm]]) else a
  chk(sprintf("%-13s %s", p$nm, if (p$keep) "survives the restore" else "is restored from the layout"),
      identical(b, want),
      sprintf("layout=%s gatingset=%s after=%s", paste(a, collapse = ","),
              paste(as.character(raw[[p$nm]]), collapse = ","), paste(b, collapse = ",")))
}

## ---- THE GATE DISCRIMINATES ------------------------------------------------------------------
## The old guard restored a cell only when the LAYOUT value was zero-padded. Re-apply exactly that
## predicate here: if it would have repaired row F, this file is not testing what it claims to.
old_padded <- function(x) grepl("^\\s*-?0[0-9]", x)
chk("the superseded padded-only guard would NOT have repaired row F",
    !any(old_padded(c("F", "F", "F"))))
chk("...nor the hex-like identifier", !any(old_padded(c("0x1A", "0x1B", "0x1C"))))
chk("...but would have repaired a zero-padded one, so it was not simply inert",
    all(old_padded(c("001", "002", "003"))))

## ---- the 1460 shape end to end ---------------------------------------------------------------
## One group confined to one plate row is the only way this surfaces, which is why six datasets and
## a full regression suite never saw it. Drive that exact shape.
ffs2 <- stats::setNames(lapply(1:2, function(i) mk()), c("w1", "w2"))
fs2  <- as(ffs2, "flowSet")
pd2  <- data.frame(name = c("w1", "w2"), Row = c("F", "F"), Column = c("2", "11"),
                   Alias = c("CD62L", "CD62L"), stringsAsFactors = FALSE)
rownames(pd2) <- c("w1", "w2")
flowCore::pData(fs2) <- pd2
gs2  <- flowWorkspace::GatingSet(fs2)
chk("a single-row group IS mangled before the repair",
    identical(as.character(flowWorkspace::pData(gs2)$Row), c("FALSE", "FALSE")))
gs2 <- restore(gs2, fs2, quiet = TRUE)
chk("a single-row group reports plate row F after the repair",
    identical(as.character(flowWorkspace::pData(gs2)$Row), c("F", "F")),
    paste(as.character(flowWorkspace::pData(gs2)$Row), collapse = ","))
chk("its Column is untouched",
    identical(as.character(flowWorkspace::pData(gs2)$Column), c("2", "11")))

## ---- the repair ALSO reorders the columns, and the export inherits that --------------------
## `pData(gs) <- ` does not preserve the order it is handed: measured on flowWorkspace 4.22.1,
## handing back the exact frame just read still rewrites the order, to a different-but-stable one.
## So a run that needed a repair produced a differently-ordered TitrationExport from one that did
## not -- the stats frame is seeded from pData, and its trailing columns are the sheet's trailing
## columns. Two runs of the same panel differing only in sitting in plate row F diffed as changed.
##
## The setter cannot be made to honour an order, so actaPData() restores it at READ time.
pdOrder <- NULL
ffs3 <- stats::setNames(lapply(1:2, function(i) mk()), c("w1", "w2"))
fs3  <- as(ffs3, "flowSet")
pd3  <- data.frame(name = c("w1", "w2"), Row = c("F", "F"), Lot_Num = c("5279781", "5279782"),
                   Zed = c("x", "y"), Alpha = c("1", "2"), stringsAsFactors = FALSE)
rownames(pd3) <- c("w1", "w2")
flowCore::pData(fs3) <- pd3
gs3 <- flowWorkspace::GatingSet(fs3)
pdOrder <- names(flowWorkspace::pData(gs3))
gs3 <- restore(gs3, fs3, quiet = TRUE)

## THE REORDER IS VERSION-DEPENDENT, AND ASSERTING IT UNCONDITIONALLY WAS WRONG.
##
## Measured on flowWorkspace 4.22.1 (macOS, R 4.5): `pData(gs) <- ` rewrites the column order even
## when handed back the exact frame it just returned. Measured on the version that ships with
## R release (ubuntu-latest and windows-latest, 2026-11-12): it PRESERVES the order.
##
## So this file used to assert that a third-party library misbehaves -- and CI duly went red on
## the two platforms where it behaves. Caught by the public runners on the first run that ever saw
## this code, which is the whole argument for getting CI green before tagging.
##
## THE INVARIANT IS THE SAME EITHER WAY, and it is what actaPData() exists for: the order the
## caller recorded is the order it gets back. Where the setter reorders, that undoes it; where the
## setter behaves, it is a no-op. Both are asserted below, on the same line, deliberately.
##
## That version split is also the better argument for the function than the one originally given:
## WITHOUT it the TitrationExport's column order would depend on which flowWorkspace the operator
## has installed, so two correct runs of the same data could diff as changed.
reordered <- !identical(names(flowWorkspace::pData(gs3)), pdOrder)
cat(sprintf("  [note] flowWorkspace %s here (%s): this build %s the pData column order on assignment\n",
            as.character(utils::packageVersion("flowWorkspace")),
            if (reordered) "reorders" else "preserves",
            if (reordered) "REWRITES" else "keeps"))
chk(sprintf("actaPData() hands back the pre-repair order (%s)",
            if (reordered) "undoing a real reorder" else "a no-op on this build"),
    identical(names(ACTA:::actaPData(gs3, pdOrder)), pdOrder),
    paste(names(ACTA:::actaPData(gs3, pdOrder)), collapse = " "))
## WHERE THE HAZARD EXISTS, PROVE THE FUNCTION IS WHAT CLOSES IT. Skipping this where the setter
## behaves is not a weakened gate: there is nothing to undo, and asserting a difference would be
## asserting the library misbehaves, which is the mistake this block was just corrected for.
## BRACED: at top level R parses `if (x) expr` and a following bare `else` as two statements and
## fails with "unexpected 'else'". It is only legal unbraced inside a block.
if (reordered) {
  chk("...and the raw read does NOT, so the function is doing the work",
      !identical(names(flowWorkspace::pData(gs3)), pdOrder))
} else {
  cat("  [skip] no reorder on this flowWorkspace, so there is nothing for actaPData() to undo\n")
}
chk("...without dropping or duplicating a column",
    setequal(names(ACTA:::actaPData(gs3, pdOrder)), names(flowWorkspace::pData(gs3))) &&
      ncol(ACTA:::actaPData(gs3, pdOrder)) == ncol(flowWorkspace::pData(gs3)))
chk("...and the VALUES are still the repaired ones, not reshuffled with the names",
    identical(as.character(ACTA:::actaPData(gs3, pdOrder)$Row), c("F", "F")) &&
      identical(as.character(ACTA:::actaPData(gs3, pdOrder)$Lot_Num), c("5279781", "5279782")),
    paste(ACTA:::actaPData(gs3, pdOrder)$Row, collapse = ","))
## A column the recorded order does not name must survive at the end rather than vanish -- that is
## what makes this safe to apply unconditionally.
chk("a column absent from the recorded order is kept, at the end",
    identical(names(ACTA:::actaPData(gs3, setdiff(pdOrder, "Zed"))),
              c(setdiff(pdOrder, "Zed"), "Zed")),
    paste(names(ACTA:::actaPData(gs3, setdiff(pdOrder, "Zed"))), collapse = " "))
chk("NULL order is a no-op, so a caller that never recorded one is unaffected",
    identical(names(ACTA:::actaPData(gs3)), names(flowWorkspace::pData(gs3))))

cat(if (ok) "\nPASS test_pdata_restore.R\n" else "\nFAIL test_pdata_restore.R\n")
quit(status = if (ok) 0L else 1L)
