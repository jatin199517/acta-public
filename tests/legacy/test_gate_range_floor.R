## `gate_range_floor` on gate_mind_fmeans_costain / gate_mind2_fmeans_costain.
##
## WHAT IT IS FOR. The gate's last step is max(min(flowMeans, mindensity), costain_floor). That
## clamp can push a cut UP to the floor but can never move it up to a real second valley: when
## mindensity picks a valley below the floor, every such well reports the floor exactly and the
## shape of its own data is lost. `gate_range_floor=TRUE` hands the floor to mindensity as the
## LOWER END OF ITS SEARCH, so it chooses among the valleys above the floor instead.
##
## WHY IT IS SAFE. openCyto's gate_mindensity falls back to `gate_range[1]` when no valley lies in
## range -- and gate_range[1] IS the floor -- so TRUE returns the old answer wherever there is
## nothing above the floor to find. It can only change a well that had a usable valley up there.
##
## cluster='pos' IN THE BEHAVIOURAL CASES ON PURPOSE. The gate takes min(flowMeans, mindensity),
## so a low flowMeans bound would mask the very thing under test. 'pos' puts flowMeans far above
## both candidate cuts and leaves mindensity as the only term that decides -- which is exactly the
## term `gate_range_floor` acts on. The no-op cases below use the ordinary 'neg'.

ok <- TRUE
chk <- function(label, pass, detail = NULL) {
  d <- paste(as.character(detail), collapse = " ")
  if (!isTRUE(pass)) { ok <<- FALSE
    cat(sprintf("  [FAIL] %s%s\n", label, if (nzchar(d)) paste0(" -- ", d) else "")) }
  else cat(sprintf("  [ok]   %s\n", label))
}

suppressMessages(library(ACTA))
suppressMessages(library(flowCore))
suppressMessages(library(openCyto))
g1 <- ACTA:::gate_mind_fmeans_costain
g2 <- ACTA:::gate_mind2_fmeans_costain
CH <- "FSC-A"

## Three well-separated modes so that, at adjust=2, two valleys survive the smoothing: one BELOW
## the floor and one ABOVE it. Without that pair there is nothing for the argument to change.
set.seed(11)
X  <- c(rnorm(5000, 100, 30), rnorm(5000, 1500, 60), rnorm(5000, 6000, 120))
FF <- flowCore::flowFrame(matrix(X, ncol = 1, dimnames = list(NULL, CH)))
FLOOR <- 2500
## EVERY REFERENCE VALUE IS COMPUTED FROM exprs(FF), NEVER FROM X. flowFrame() stores events at
## reduced precision -- max(abs(exprs - X)) is 2.4e-04 here -- so a valley found on the input
## vector misses the gate's own answer by more than all.equal()'s tolerance and the assertion
## fails on correct code. Compare against what the gate reads.
EX  <- exprs(FF)[, CH]
cut <- function(gate) unname(gate@min[[CH]])
call1 <- function(...) cut(g1(FF, channels = CH, K = 2, quantile = 0.99, adjust = 2, ...))

cat("-- the fixture really does have a valley on each side of the floor --\n")
v <- sort(openCyto:::.find_valleys(EX, adjust = 2))
chk("two valleys, one below the floor and one above",
    length(v) == 2L && v[1] < FLOOR && v[2] > FLOOR, paste(round(v), collapse = ", "))

## flowMeans LOADS tcltk, WHICH NEEDS X11. The macOS CI runner has no XQuartz, so every
## flowMeans call there dies with ".onLoad failed in loadNamespace('tcltk') -- X11 library is
## missing". That is an environment fact, not a defect in the gate: ACTA's own pre-flight
## reports the same thing, and the pipeline cannot run on such a machine either.
##
## SKIPPED LOUDLY, NEVER SILENTLY, and the skip is NARROW -- only the assertions that actually
## need flowMeans stand down, the rest of the file still runs. ubuntu and windows both have X11
## and run these for real, so the coverage is not lost, only relocated. If this banner ever
## appears on a runner that is SUPPOSED to have X11, that is the bug, not the skip.
.X11OK <- isTRUE(requireNamespace("tcltk", quietly = TRUE)) &&
          !inherits(try(tcltk::tclVersion(), silent = TRUE), "try-error")
if (!.X11OK)
  cat("  [skip] no X11/XQuartz on this machine -- the flowMeans assertions below stand down.\n",
      "        ubuntu and windows CI run them; see actaDependencyChecks()'s xquartz row.\n", sep = "")

if (!.X11OK) { cat(if (ok) "\nGATE_RANGE_FLOOR: the X11-free checks passed\n" else "\nFAILURES ABOVE\n"); quit(status = if (ok) 0L else 1L) }

cat("-- FALSE is a no-op: the shipped behaviour is unchanged --\n")
## Assembled from the same primitives but WIRED SEPARATELY, so a plumbing fault (the flag stuck
## on, the floor read from the wrong place) shows up as a mismatch rather than cancelling out.
fm   <- ACTA:::.flowmeans_bound(EX, 2, "neg", 0.99)
mdU  <- cut(openCyto::gate_mindensity(FF, CH, positive = TRUE, adjust = 2))
want <- base::max(base::min(fm, mdU), FLOOR)
chk("explicit FALSE matches the pre-change formula",
    isTRUE(all.equal(call1(pp_res = FLOOR, cluster = "neg", gate_range_floor = FALSE), want)),
    sprintf("got %.1f want %.1f", call1(pp_res=FLOOR, cluster="neg", gate_range_floor=FALSE), want))
## The 'pos' half is what actually pins the default. With cluster='neg' the flowMeans term wins
## whatever mindensity returns, so that comparison holds even if the flag were stuck ON -- it
## checks the formula, not the default. 'pos' is the arm where the two differ.
chk("the argument DEFAULTS to FALSE (older sheets are unaffected)",
    isTRUE(all.equal(call1(pp_res = FLOOR, cluster = "neg"), want)) &&
    isTRUE(all.equal(call1(pp_res = FLOOR, cluster = "pos"), FLOOR)))

cat("-- TRUE pins the search to the floor --\n")
lo <- call1(pp_res = FLOOR, cluster = "pos", gate_range_floor = FALSE)
hi <- call1(pp_res = FLOOR, cluster = "pos", gate_range_floor = TRUE)
chk("FALSE is clamped flat onto the floor", isTRUE(all.equal(lo, FLOOR)), lo)
chk("TRUE finds the valley ABOVE the floor instead",
    hi > FLOOR && isTRUE(all.equal(hi, v[2])), sprintf("got %.1f, valley %.1f", hi, v[2]))

cat("-- it degrades the way openCyto promises --\n")
## No valley inside [floor, 3000]; gate_mindensity falls back to gate_range[1], which is the floor.
chk("an explicit gate_range keeps its UPPER bound, and falls back to the floor",
    isTRUE(all.equal(call1(pp_res = FLOOR, cluster = "pos", gate_range = c(0, 3000),
                           gate_range_floor = TRUE), FLOOR)))
chk("no Costain well (floor -Inf) makes TRUE a no-op",
    isTRUE(all.equal(call1(pp_res = -Inf, cluster = "pos", gate_range_floor = TRUE),
                     call1(pp_res = -Inf, cluster = "pos", gate_range_floor = FALSE))))
big <- call1(pp_res = 1e9, cluster = "pos", gate_range_floor = TRUE)
chk("a floor above every event returns one finite number, not an error",
    length(big) == 1L && is.finite(big) && isTRUE(all.equal(big, 1e9)), big)

cat("-- the mind2 sibling honours it too --\n")
## ITS OWN FIXTURE, and the reason IS the check. On FF above, gate_mindensity2 already selects the
## upper valley unaided, so `gate_range_floor` cannot move it there and any assertion written
## against FF passes whatever the flag does -- this section was vacuous until that was found.
## These modes put the widest gap BELOW the floor, which is where mindensity2 looks first.
set.seed(29)
X2     <- c(rnorm(6000, 100, 30), rnorm(5000, 4000, 70), rnorm(5000, 5200, 70))
FF2    <- flowCore::flowFrame(matrix(X2, ncol = 1, dimnames = list(NULL, CH)))
v2     <- sort(openCyto:::.find_valleys(exprs(FF2)[, CH], adjust = 2))
FLOOR2 <- 3000
m2 <- function(grf) cut(g2(FF2, pp_res = FLOOR2, channels = CH, K = 2, cluster = "pos",
                           quantile = 0.99, adjust = 2, gate_range_floor = grf))
chk("the mind2 fixture also brackets the floor",
    length(v2) == 2L && v2[1] < FLOOR2 && v2[2] > FLOOR2, paste(round(v2, 1), collapse = ", "))
chk("mind2 FALSE is clamped flat onto the floor", isTRUE(all.equal(m2(FALSE), FLOOR2)), m2(FALSE))
chk("mind2 TRUE moves strictly above the floor", m2(TRUE) > FLOOR2 + 1,
    sprintf("FALSE %.1f TRUE %.1f upper valley %.1f", m2(FALSE), m2(TRUE), v2[2]))

cat("-- unchanged contracts --\n")
chk("`positive` is still refused",
    inherits(try(g1(FF, pp_res = FLOOR, channels = CH, positive = TRUE), silent = TRUE), "try-error"))
gg <- g1(FF, pp_res = FLOOR, channels = CH, K = 2, cluster = "pos", quantile = 0.99,
         adjust = 2, gate_range_floor = TRUE)
chk("the gate is still c(bound, Inf) on the first dim",
    is(gg, "rectangleGate") && names(gg@min) == CH && is.infinite(gg@max[[CH]]) && gg@max[[CH]] > 0)

cat(if (ok) "\nGATE_RANGE_FLOOR TESTS PASSED\n" else "\nFAILURES ABOVE\n")
quit(status = if (ok) 0L else 1L)
