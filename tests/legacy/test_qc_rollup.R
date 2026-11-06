## The QC roll-up, and the one distinction it now rests on: SKIPPED vs N/A.
##
## WHY THIS FILE EXISTS. actaQCStatus() used to be "PASS only if every check passed", with a
## skipped check counting as FAIL on the stated grounds that a check which never ran is not
## evidence of quality. That is still right. But once validateLayoutGroups() stopped demanding a
## Costain well from templates that never read the floor, the two Costain checks began reporting
## SKIPPED on layouts that are perfectly legitimate -- so every such run carried QC_Status = FAIL
## with nothing wrong in it. A permanent red verdict on a supported layout is the kind of failure
## people learn to ignore, which costs more than the check was worth.
##
## N/A is the fix, and it is SAFE ONLY WHILE THE PRE-FLIGHT INVARIANT HOLDS: a missing Costain
## well may be excused as "nothing to check" precisely because the pre-flight still refuses to
## run a *_costain method without one. Excuse it without that guarantee and a genuinely floorless
## gate -- the thing the check was written to catch -- reports N/A and rolls up as PASS.
## Both halves are therefore asserted HERE, together, rather than trusted to stay true separately.
source(file.path(this.path::this.dir(), "acta_test_paths.R"))
vd <- actaTestVersionDir()
cat("version_dir:", basename(vd), "\n")
h <- actaTestFunctions(vd)
ok <- TRUE
chk <- function(what, cond) { if (!isTRUE(cond)) ok <<- FALSE
                              cat(sprintf("  [%s] %s\n", if (isTRUE(cond)) "ok" else "FAIL", what)) }

NA_  <- get(".actaQCNotApplicable", envir = h)
SKIP <- get(".actaQCSkip", envir = h)
V    <- get(".actaQCVerdict", envir = h)

cat("\n=== the N/A constructor ===\n")
n <- NA_("costain_lowest", "a statement", "because there is nothing to check")
chk("sets verdict N/A", identical(n$verdict, "N/A"))
chk("...and applicable FALSE in the SAME constructor (they cannot drift apart)",
    identical(n$applicable, FALSE))
chk("...and passed NA -- it is not a pass", is.na(n$passed))
chk("...and keeps the reason in note, not in verdict", nzchar(n$note))
chk("a SKIPPED check is NOT marked inapplicable",
    !identical(SKIP("x", "s", "why")$applicable, FALSE))

cat("\n=== the roll-up ===\n")
P <- V("a", "s", TRUE); F <- V("b", "s", FALSE); S <- SKIP("c", "s", "why")
N <- NA_("d", "s", "nothing to check")
chk("all pass -> PASS", identical(h$actaQCStatus(list(P, P)), "PASS"))
chk("a FAIL -> FAIL",   identical(h$actaQCStatus(list(P, F)), "FAIL"))
## The original rule, pinned: relaxing N/A must not relax this with it.
chk("a SKIPPED check still -> FAIL", identical(h$actaQCStatus(list(P, S)), "FAIL"))
chk("an N/A check is EXCLUDED -> PASS", identical(h$actaQCStatus(list(P, N)), "PASS"))
## The discriminating case: N/A excuses itself, never its neighbours.
chk("N/A does not rescue a real FAIL", identical(h$actaQCStatus(list(N, F)), "FAIL"))
chk("N/A does not rescue a SKIPPED",   identical(h$actaQCStatus(list(N, S)), "FAIL"))
chk("nothing applicable at all -> FAIL", identical(h$actaQCStatus(list(N, N)), "FAIL"))
chk("no checks at all -> FAIL", identical(h$actaQCStatus(list()), "FAIL"))

cat("\n=== the two Costain checks pick the right verdict ===\n")
## costain_positive reads `percent` as a FRACTION; costain_lowest reads `SI`.
si <- function(types, si_, pct) data.frame(StainType = types, SI = si_, percent = pct,
                                           stringsAsFactors = FALSE)
good <- si(c("Stain","Stain","Costain"), c(10, 8, 1), c(0.5, 0.4, 0.01))
noco <- si(c("Stain","Stain"),           c(10, 8),    c(0.5, 0.4))
nanc <- si(c("Stain","Stain","Costain"), c(10, 8, NaN), c(0.5, 0.4, NaN))
cp <- h$actaQCCostainPositive; cl <- h$actaQCCostainLowest
chk("costain_positive PASSES a clean layout", identical(cp(good)$verdict, "PASS"))
chk("costain_positive is N/A with no Costain well", identical(cp(noco)$verdict, "N/A"))
chk("costain_positive still SKIPS when the Costain percent is not finite",
    identical(cp(nanc)$verdict, "SKIPPED"))
chk("costain_positive still FAILS a Costain that is mostly positive",
    identical(cp(si(c("Stain","Costain"), c(10, 1), c(0.5, 0.46)))$verdict, "FAIL"))
chk("costain_lowest PASSES a clean layout", identical(cl(good)$verdict, "PASS"))
chk("costain_lowest is N/A with no Costain well", identical(cl(noco)$verdict, "N/A"))
chk("costain_lowest still SKIPS when the Costain SI is not finite",
    identical(cl(nanc)$verdict, "SKIPPED"))
chk("costain_lowest still FAILS when the Costain is not the lowest",
    identical(cl(si(c("Stain","Costain"), c(2, 9), c(0.5, 0.01)))$verdict, "FAIL"))
## A whole-run shape: the Costain-free layout must reach PASS, and the export column must say N/A.
runq <- list(V("cells_fsc","s",TRUE), V("live","s",TRUE), cl(noco), cp(noco), V("min_events","s",TRUE))
chk("a Costain-free run rolls up to PASS", identical(h$actaQCStatus(runq), "PASS"))
cols <- h$actaQCColumns(runq)
chk("...and the export says N/A, not SKIPPED",
    identical(cols[["QC_costain_lowest"]], "N/A") &&
      identical(cols[["QC_costain_positive"]], "N/A"))

cat("\n=== THE INVARIANT N/A RESTS ON ===\n")
## If this ever stops being true, every assertion above becomes a way to lose a real failure:
## a *_costain gate running with no floor would report N/A and the run would read PASS.
lay <- data.frame(Dirname = "X", Alias = "X", PlateID = 1, Row = c("A","B","C"), Column = 1,
                  StainType = c("Stain","Stain","Stain"), StainQty = c(1,2,3),
                  stringsAsFactors = FALSE)
gt <- function(meth, pp = NA_character_)
  data.frame(alias = "Reagent", gating_method = meth, preprocessing_method = pp,
             stringsAsFactors = FALSE)
for (m in c("gate_flowmeans_costain", "gate_mindensity_costain", "gate_auto_costain_dip",
            "gate_auto_costain_gmm", "gate_auto2_costain")) {
  req <- h$actaTemplateNeedsCostain(gt(m))
  f <- h$validateLayoutGroups(lay, "X", stop_on_error = FALSE, return_all = TRUE,
                              costain_required = req)$fatal
  chk(sprintf("%s STILL cannot run without a Costain well", m),
      isTRUE(req) && length(f) == 1L)
}
chk("...while a plain template is allowed to",
    isFALSE(h$actaTemplateNeedsCostain(gt("gate_flowmeans"))) &&
      length(h$validateLayoutGroups(lay, "X", stop_on_error = FALSE, return_all = TRUE,
                                    costain_required = FALSE)$fatal) == 0L)

cat(if (ok) "\nQC ROLL-UP TESTS PASS\n" else "\nFAILURES ABOVE\n")
quit(status = if (ok) 0 else 1)
