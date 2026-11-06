## gate_mindensity_costain: openCyto's mindensity cutpoint, floored at the Costain value.
##
## WHAT THIS GATE IS REALLY FOR. The dangerous way to write a "floored" plugin is to return the
## floor, or to take min() instead of max(), or to let the floor stand in for the gate whenever
## pp_res is present. All three produce plausible-looking gates and all three are wrong, and the
## only way to tell them apart from the outside is to run the method twice against the SAME data
## with the floor on either side of the natural boundary. That is what this does.
##
## The second reason it exists is the degeneracy documented on the function: openCyto's mindensity
## cannot report failure. Handed a unimodal channel it finds no valley and silently returns the
## bottom of gate_range -- min(x) -- which is a gate that calls 100% of events positive. The
## Costain floor is what rescues that, so both halves are asserted here: unfloored it really does
## pass everything, floored it really does become a static gate at the Costain value. If either
## half stops being true the comment on the function has become a lie.

ok <- TRUE
## AN ASSERTION THAT ERRORS IS A FAILURE, NOT THE END OF THE RUN -- `pass` is a promise, so
## forcing it inside tryCatch catches a predicate that throws rather than aborting the script
## and hiding every assertion after it.
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
gmc <- ACTA:::gate_mindensity_costain
lo  <- function(g, ch) unname(g@min[[ch]])   # the gate is always c(bound, Inf)

## ---- a real titration frame -------------------------------------------------------------------
fcs <- system.file("extdata/oq_small/OQ_Test3/Titration_FCS/CCR7/OQ_Test3_CCR7_D3_Stain.fcs",
                   package = "ACTA")
if (!nzchar(fcs) || !file.exists(fcs)) {
  cat("  [skip] the OQ_Test3 CCR7 fixture is not installed.\n")
  cat("\nGATE MINDENSITY COSTAIN TESTS SKIPPED\n"); quit(status = 0)
}
fr  <- read.FCS(fcs, transformation = FALSE, truncate_max_range = FALSE)
ch  <- "BUV496-A"
x   <- exprs(fr)[, ch]
## openCyto's own answer, which is what "no floor" must reproduce exactly.
bare <- unname(openCyto::gate_mindensity(fr, channel = ch, positive = TRUE)@min[[ch]])
chk("the fixture has a real density valley to find (not the degenerate min(x))",
    is.finite(bare) && bare > min(x), sprintf("cut=%.1f min=%.1f", bare, min(x)))

## 1. NO pp_res -> plain mindensity. The floor is -Inf and must not shift the boundary.
chk("no pp_res reproduces openCyto::gate_mindensity exactly",
    isTRUE(all.equal(lo(gmc(fr, NULL, ch), ch), bare)),
    sprintf("%.4f vs %.4f", lo(gmc(fr, NULL, ch), ch), bare))
for (empty in list(NULL, NA, NA_real_, numeric(0), ""))
  chk(sprintf("...and so does an empty pp_res (%s)", paste(class(empty), length(empty))),
      isTRUE(all.equal(lo(gmc(fr, empty, ch), ch), bare)))

## 2. A floor ABOVE the valley raises the boundary to the floor.
high <- bare + 10000
chk("a floor above the valley wins", isTRUE(all.equal(lo(gmc(fr, high, ch), ch), high)),
    sprintf("%.1f, expected %.1f", lo(gmc(fr, high, ch), ch), high))

## 3. A floor BELOW the valley changes nothing. THE DISCRIMINATING CASE: a plugin that returned
##    the floor, or took min() instead of max(), passes test 2 and fails only here.
low <- bare - 10000
chk("a floor below the valley is ignored -- it is a FLOOR, not the gate",
    isTRUE(all.equal(lo(gmc(fr, low, ch), ch), bare)),
    sprintf("%.1f, expected the unfloored %.1f", lo(gmc(fr, low, ch), ch), bare))

## 4. The gate is ALWAYS c(bound, Inf): the +/- side is the template `pop` column's job.
for (pp in list(NULL, low, high)) {
  g <- gmc(fr, pp, ch)
  chk(sprintf("the gate is c(bound, Inf) with pp_res=%s",
              if (is.null(pp)) "NULL" else sprintf("%.0f", pp)),
      is.finite(g@min[[ch]]) && is.infinite(g@max[[ch]]) && g@max[[ch]] > 0)
}
chk("the filterId names the method", identical(gmc(fr, NULL, ch)@filterId, "mindensity_costain"),
    gmc(fr, NULL, ch)@filterId)

## 5. `positive` is refused rather than honoured or silently dropped. Honouring it would invert
##    the sheet's `pop`; dropping it silently would do the same thing without saying so.
err <- tryCatch({ gmc(fr, NULL, ch, positive = FALSE); NA_character_ },
                error = function(e) conditionMessage(e))
chk("`positive` is an error, not a silent no-op", !is.na(err), err)
chk("...and the message points at `pop`", !is.na(err) && grepl("pop", err, fixed = TRUE), err)

## 6. Non-finite events are dropped before the density estimate. The guard is load-bearing, so
##    both halves are asserted: openCyto on its own ERRORS here (gate_mindensity reads
##    exprs(fr)[, channel] straight into density()), and the plugin's answer is exactly the one
##    you get by filtering the frame by hand first -- not merely "some finite number".
fr_inf <- fr
e2 <- exprs(fr_inf); e2[1:5, ch] <- c(Inf, -Inf, NaN, Inf, -Inf); exprs(fr_inf) <- e2
raw <- tryCatch({ openCyto::gate_mindensity(fr_inf, channel = ch); NA_character_ },
                error = function(e) conditionMessage(e))
chk("openCyto alone cannot gate a channel holding Inf", !is.na(raw), raw)
pre <- unname(openCyto::gate_mindensity(fr_inf[is.finite(exprs(fr_inf)[, ch]), ],
                                        channel = ch, positive = TRUE)@min[[ch]])
chk("...and the plugin returns exactly the pre-filtered answer",
    isTRUE(all.equal(lo(gmc(fr_inf, NULL, ch), ch), pre)),
    sprintf("%.4f vs %.4f", lo(gmc(fr_inf, NULL, ch), ch), pre))

## 7. Only channels[1] gates; a second dim is the plot's y axis and must not change the answer.
chk("a second dim is ignored (1D gate on channels[1])",
    isTRUE(all.equal(lo(gmc(fr, NULL, c(ch, "FSC-A")), ch), bare)))

## ---- the unimodal degeneracy, and the floor that rescues it -------------------------------------
## Synthetic on purpose: no real titration well is reliably unimodal, and this half of the
## function's documentation has to be checkable without hunting for one.
set.seed(1)
uni <- flowFrame(matrix(rnorm(20000, mean = 1000, sd = 120), ncol = 1,
                        dimnames = list(NULL, "UNI-A")))
ux <- exprs(uni)[, "UNI-A"]
u_bare <- lo(gmc(uni, NULL, "UNI-A"), "UNI-A")
chk("UNFLOORED, a unimodal channel degenerates to min(x) -- everything positive",
    isTRUE(all.equal(u_bare, min(ux))), sprintf("bound=%.2f min(x)=%.2f", u_bare, min(ux)))
chk("...which really does keep 100% of events",
    mean(ux >= u_bare) == 1, sprintf("%.1f%% positive", 100 * mean(ux >= u_bare)))
u_floor <- stats::quantile(ux, 0.99, names = FALSE)
chk("FLOORED, the same unimodal channel becomes a static gate at the Costain value",
    isTRUE(all.equal(lo(gmc(uni, u_floor, "UNI-A"), "UNI-A"), u_floor)))
chk("...which is a usable gate, not a pass-everything one",
    mean(ux >= u_floor) < 0.02, sprintf("%.2f%% positive", 100 * mean(ux >= u_floor)))

## ---- the method is actually reachable from a gating_template ------------------------------------
## Registering is what makes the name usable in the sheet, and a method defined but never
## registered fails only at run time, inside gt_gating, on a real plate.
reg <- tryCatch({ openCyto::register_plugins(fun = gmc, methodName = "gate_mindensity_costain")
                  TRUE }, error = function(e) grepl("already", conditionMessage(e)))
chk("register_plugins accepts the function (signature matches openCyto's contract)", isTRUE(reg))
chk("the shipped pipeline registers it", {
  s <- readLines(system.file("pipeline/ACTA_Script.R", package = "ACTA"), warn = FALSE)
  any(grepl('methodName="gate_mindensity_costain"', s, fixed = TRUE))
})

## ---- the template offers it, with the preprocessing it cannot work without ----------------------
tpl <- system.file("pipeline/Template/TEMPLATE_Ab_Titration_Instructions.xlsx", package = "ACTA")
if (nzchar(tpl) && file.exists(tpl)) {
  gt <- suppressMessages(readxl::read_excel(tpl, sheet = "gating_template", col_types = "text"))
  r <- which(gt$gating_method == "gate_mindensity_costain")
  chk("the template carries a #Reagent row for it", length(r) == 1L, length(r))
  if (length(r) == 1L) {
    chk("...commented out, like the other alternatives", identical(gt$alias[r], "#Reagent"),
        gt$alias[r])
    ## The whole method collapses to "everything positive" on a unimodal well without this.
    chk("...with preprocessing_method = pp_costain_floor",
        identical(gt$preprocessing_method[r], "pp_costain_floor"), gt$preprocessing_method[r])
    chk("...and a costain_quantile for it to read",
        grepl("costain_quantile", gt$preprocessing_args[r] %||% ""), gt$preprocessing_args[r])
  }
  h <- which(gt$alias == "#gate_mindensity_costain")
  chk("the help block documents the method", length(h) == 1L && nzchar(gt$help_notes[h] %||% ""))
  chk("...and pp_costain_floor's own note lists it among its callers",
      any(grepl("gate_mindensity_costain", gt$help_notes[which(gt$alias == "#pp_costain_floor")],
                fixed = TRUE)))
} else cat("  [skip] template not installed; row checks skipped.\n")

## ---- it is drawn as its own method in the gating scheme -----------------------------------------
## An unrecognised method falls through to `other` and is left white, which on the page reads as
## "unclassified" rather than "mindensity".
chk("the gating-scheme palette has a colour for it",
    "gate_mindensity_costain" %in% names(ACTA:::ACTA_GATE_METHOD_PAL))
chk("...that is not shared with another method",
    sum(ACTA:::ACTA_GATE_METHOD_PAL == ACTA:::ACTA_GATE_METHOD_PAL[["gate_mindensity_costain"]]) == 1L)

cat(if (ok) "\nGATE MINDENSITY COSTAIN TESTS PASSED\n" else "\nFAILURES ABOVE\n")
quit(status = if (ok) 0L else 1L)
