## gate_costain: the Costain floor IS the gate.
##
## WHAT MAKES THIS METHOD DIFFERENT, and therefore what this file is really testing. Every other
## *_costain method computes a boundary from the stained data and then refuses to let it fall below
## the co-stain -- max(data_driven, floor). This one has no data-driven half at all. Two properties
## follow, and both are asserted here because both are easy to break by "improving" the function
## into one of its siblings:
##
##   1. THE GATE DOES NOT DEPEND ON THE DATA. Hand it two completely different flowFrames with the
##      same floor and it must return the same boundary. That is the whole meaning of "placed from
##      the co-stain and applied everywhere", and nothing else in the family behaves this way.
##   2. A MISSING FLOOR IS FATAL. Its siblings degrade to the un-floored method underneath them --
##      a wrong gate, but a gate. This one would produce c(-Inf, Inf): every event positive, in
##      every sample, with a Stain Index computed off it and no warning anywhere. So it stops.
##
## If either stops holding, the method has quietly become something else.

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

suppressMessages({ library(ACTA); library(flowCore) })
gc_ <- ACTA:::gate_costain
lo  <- function(g, ch) unname(g@min[[ch]])

fcs <- system.file("extdata/oq_small/OQ_Test3/Titration_FCS/CCR7/OQ_Test3_CCR7_D3_Stain.fcs",
                   package = "ACTA")
if (!nzchar(fcs) || !file.exists(fcs)) {
  cat("  [skip] the OQ_Test3 CCR7 fixture is not installed.\n")
  cat("\nGATE COSTAIN TESTS SKIPPED\n"); quit(status = 0)
}
fr <- read.FCS(fcs, transformation = FALSE, truncate_max_range = FALSE)
ch <- "BUV496-A"
FLOOR <- 12345.678

## ---- 1. the floor is the boundary, exactly ------------------------------------------------------
chk("the boundary IS the floor, not a max() against anything",
    identical(lo(gc_(fr, FLOOR, ch), ch), FLOOR), lo(gc_(fr, FLOOR, ch), ch))
## The discriminating pair: a floor far BELOW and far ABOVE the data must both be taken verbatim.
## A sibling implementation (max with a data-driven bound) passes the "above" case and fails "below".
x <- exprs(fr)[, ch]; x <- x[is.finite(x)]
for (f in c(min(x) - 1e5, stats::median(x), max(x) + 1e5))
  chk(sprintf("...including a floor at %.0f, which no data-driven method would return", f),
      identical(lo(gc_(fr, f, ch), ch), f), lo(gc_(fr, f, ch), ch))

## ---- 2. THE DEFINING PROPERTY: the gate ignores the frame ---------------------------------------
set.seed(7)
other <- flowFrame(matrix(c(rnorm(5000, 1e5, 2e4), rnorm(5000, 50, 5)), ncol = 2,
                          dimnames = list(NULL, c(ch, "FSC-A"))))
chk("two completely different frames with the same floor give the SAME gate",
    identical(lo(gc_(fr, FLOOR, ch), ch), lo(gc_(other, FLOOR, ch), ch)))
chk("...and a one-event frame gives it too (nothing is estimated from the data)",
    identical(lo(gc_(fr[1, ], FLOOR, ch), ch), FLOOR))

## ---- 3. both pp_res shapes ----------------------------------------------------------------------
## pp_costain_floor hands back a bare numeric; pp_auto2_costain a list carrying $floor.
chk("a bare numeric pp_res is read", identical(lo(gc_(fr, FLOOR, ch), ch), FLOOR))
chk("a pp_auto2_costain-shaped list is read",
    identical(lo(gc_(fr, list(floor = FLOOR, use_static = FALSE), ch), ch), FLOOR))
chk("...and use_static is irrelevant -- this method is always static",
    identical(lo(gc_(fr, list(floor = FLOOR, use_static = TRUE), ch), ch),
              lo(gc_(fr, list(floor = FLOOR, use_static = FALSE), ch), ch)))

## ---- 4. shape and identity ----------------------------------------------------------------------
g <- gc_(fr, FLOOR, ch)
chk("the gate is c(bound, Inf)", is.finite(g@min[[ch]]) && is.infinite(g@max[[ch]]) && g@max[[ch]] > 0)
chk("the filterId names the method", identical(g@filterId, "costain"), g@filterId)
chk("a second dim is ignored (1D gate on channels[1])",
    identical(lo(gc_(fr, FLOOR, c(ch, "FSC-A")), ch), FLOOR))

## ---- 5. `positive` is refused, as across the family ---------------------------------------------
err <- tryCatch({ gc_(fr, FLOOR, ch, positive = FALSE); NA_character_ },
                error = function(e) conditionMessage(e))
chk("`positive` is an error, not a silent no-op", !is.na(err), err)
chk("...and the message points at `pop`", !is.na(err) && grepl("pop", err, fixed = TRUE), err)

## ---- 6. A MISSING FLOOR IS FATAL ----------------------------------------------------------------
## Each of these is a way pp_res can arrive empty. Every one must stop, because the alternative is
## c(-Inf, Inf) -- a gate calling 100% of every sample positive, reported as a real result.
for (bad in list(NULL, NA, NA_real_, numeric(0), "", -Inf, Inf, NaN, list(), list(use_static = TRUE))) {
  e <- tryCatch({ gc_(fr, bad, ch); NA_character_ }, error = function(e) conditionMessage(e))
  chk(sprintf("an empty/non-finite floor (%s) is fatal", paste(class(bad), length(bad))), !is.na(e))
}
e <- tryCatch({ gc_(fr, NULL, ch); "" }, error = function(e) conditionMessage(e))
chk("...and the message says WHY there is no fallback",
    grepl("no data-driven", e, fixed = TRUE) && grepl("pp_costain_floor", e, fixed = TRUE), e)
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

## The contrast that justifies the whole design: a sibling given the SAME empty pp_res does not stop.
if (.X11OK) {
  sib <- tryCatch({ ACTA:::gate_flowmeans_costain(fr, NULL, ch); "returned a gate" },
                  error = function(e) "stopped")
  chk("...whereas gate_flowmeans_costain on the same empty pp_res still returns a gate",
      identical(sib, "returned a gate"), sib)
}

## ---- 7. a channel that is not in the frame ------------------------------------------------------
## This is the one plugin that never touches exprs(fr), so without an explicit check a bad dim
## would survive gate construction and fail later, inside flowWorkspace.
e <- tryCatch({ gc_(fr, FLOOR, "NOT-A-CHANNEL"); NA_character_ }, error = function(e) conditionMessage(e))
chk("a channel absent from the frame is a clear error here, not later",
    !is.na(e) && grepl("NOT-A-CHANNEL", e, fixed = TRUE), e)

## ---- 8. reachable from a gating_template --------------------------------------------------------
reg <- tryCatch({ openCyto::register_plugins(fun = gc_, methodName = "gate_costain"); TRUE },
                error = function(e) grepl("already", conditionMessage(e)))
chk("register_plugins accepts the signature", isTRUE(reg))
chk("the shipped pipeline registers it", {
  s <- readLines(system.file("pipeline/ACTA_Script.R", package = "ACTA"), warn = FALSE)
  any(grepl('methodName="gate_costain"', s, fixed = TRUE)) })

## ---- 9. the pre-flight treats it as a Costain method --------------------------------------------
## It matches on the substring "costain", so this should be automatic -- but "should be automatic"
## is exactly the kind of claim that stops being true without anyone noticing.
gt1 <- data.frame(alias = "Reagent", gating_method = "gate_costain",
                  preprocessing_method = "pp_costain_floor",
                  preprocessing_args = "costain_quantile=0.99", stringsAsFactors = FALSE)
gt2 <- gt1; gt2$preprocessing_method <- NA_character_; gt2$preprocessing_args <- NA_character_
chk("a complete gate_costain row passes the args pre-flight",
    length(ACTA:::actaCostainArgIssues(gt1)$fatal) == 0L)
chk("...and one with no preprocessing_method is fatal",
    length(ACTA:::actaCostainArgIssues(gt2)$fatal) == 1L)
chk("...and it still requires a Costain WELL", isTRUE(ACTA:::actaTemplateNeedsCostain(gt1)))

## ---- 10. the template offers it, and the figure can colour it -----------------------------------
tpl <- system.file("pipeline/Template/TEMPLATE_Ab_Titration_Instructions.xlsx", package = "ACTA")
if (nzchar(tpl) && file.exists(tpl)) {
  gt <- suppressMessages(readxl::read_excel(tpl, sheet = "gating_template", col_types = "text"))
  r <- which(gt$gating_method == "gate_costain")
  chk("the template carries a #Reagent row for it", length(r) == 1L, length(r))
  if (length(r) == 1L) {
    chk("...commented out, like the other alternatives", identical(gt$alias[r], "#Reagent"))
    chk("...with preprocessing_method = pp_costain_floor",
        identical(gt$preprocessing_method[r], "pp_costain_floor"))
    chk("...and a costain_quantile for it to read",
        grepl("costain_quantile", gt$preprocessing_args[r] %||% ""))
    ## There is nothing to tune, so an args cell would be a lie about the interface.
    chk("...and NO gating_args, because the method takes none",
        is.na(gt$gating_args[r]) || !nzchar(trimws(gt$gating_args[r])), gt$gating_args[r])
  }
  h <- which(gt$alias == "#gate_costain")
  chk("the help block documents the method", length(h) == 1L && nzchar(gt$help_notes[h] %||% ""))
  chk("...and pp_costain_floor's note lists it among its callers",
      any(grepl("gate_costain", gt$help_notes[which(gt$alias == "#pp_costain_floor")], fixed = TRUE)))
} else cat("  [skip] template not installed; row checks skipped.\n")

chk("the gating-scheme palette has a colour for it",
    "gate_costain" %in% names(ACTA:::ACTA_GATE_METHOD_PAL))
chk("...that is not shared with another method",
    sum(ACTA:::ACTA_GATE_METHOD_PAL == ACTA:::ACTA_GATE_METHOD_PAL[["gate_costain"]]) == 1L)

cat(if (ok) "\nGATE COSTAIN TESTS PASSED\n" else "\nFAILURES ABOVE\n")
quit(status = if (ok) 0L else 1L)
