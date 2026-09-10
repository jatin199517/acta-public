plotLimits<-function(fs, channel){
  g<-data.frame()
  for(i in seq_along(fs)){
    colsToSelect<-intersect(colnames(exprs(fs[[i]])),channel)
    tempData<-as_tibble(exprs(fs[[i]])) %>% 
      select(all_of(colsToSelect)) %>% 
      summarize(index=i, across(everything(), list(maximum=~quantile(.x, 0.99), minimum=~quantile(.x, 0.01))))
    g<-rbind(g, tempData)    
  }
  limits<-g %>% 
    pivot_longer(cols=contains("_"),
                 names_to=c("channel",".value"),
                 names_pattern="(.*)_(.*)$") %>% 
    group_by(channel) %>% 
    summarize(max=max(maximum), min=min(minimum))
  return(limits)
}


## Shared boundary helper for the flowMeans-based gating plugins. Runs k=K flowMeans
## on x and returns the `quantile`-th percentile of the CHOSEN cluster's values:
## cluster="neg" -> the LOWER-mean cluster, "pos" -> the HIGHER-mean cluster (clusters
## identified by mean). `quantile` is applied DIRECTLY (no 1-quantile flip). This value
## IS the gate boundary; every plugin gates as c(bound, Inf), so RIGHT of the boundary
## is positive and LEFT is negative. e.g. cluster="neg", quantile=0.99 => the top edge
## of the negative population. x must be finite (callers drop non-finite first).
.flowmeans_bound <- function(x, K, cluster, quantile) {
  if (!(cluster %in% c("neg", "pos"))) stop("cluster must be 'neg' or 'pos'")
  fm  <- flowMeans(x = matrix(x, ncol = 1, dimnames = list(NULL, "v")),
                   varNames = "v", MaxN = K, NumC = K)
  cl  <- fm@Labels[[1]]
  mns <- tapply(x, cl, mean)
  target <- as.integer(names(mns)[if (cluster == "neg") which.min(mns) else which.max(mns)])
  unname(stats::quantile(x[cl == target], probs = quantile))
}

## Custom openCyto gating plugin: a 1D flowMeans gate, registered as method
## "gate_flowmeans" for the gating_template sheet. `cluster` + `quantile` set the
## boundary VALUE (see .flowmeans_bound); the gate is ALWAYS c(bound, Inf) so that
## RIGHT of the boundary is positive ("+") and LEFT is negative ("-"). The +/- side is
## chosen by the template `pop` column, NOT by `cluster`. Gating is 1D on channels[1];
## any extra dims (e.g. FSC-A) are ignored.
gate_flowmeans <- function(fr, pp_res, channels, K = 2, cluster = "neg", quantile = 0.99, ...) {
  chnl  <- channels[1]
  x     <- exprs(fr)[, chnl]; x <- x[is.finite(x)]
  bound <- .flowmeans_bound(x, K, cluster, quantile)
  flowCore::rectangleGate(.gate = setNames(list(c(bound, Inf)), chnl), filterId = "flowmeans")
}

## Resolve the Costain floor for the *_costain gating plugins. The floor is
## supplied SOLELY by the pp_costain_floor PREPROCESSING method via pp_res (a
## per-sample numeric fluorescence value). Anything empty/NA/non-numeric -> -Inf,
## which makes the max() floor a no-op (i.e. plain flowMeans / auto behaviour) --
## that is exactly what you get if pp_costain_floor is NOT attached to the row, so
## every *_costain method MUST have preprocessing_method = pp_costain_floor set.
.resolve_costain_floor <- function(pp_res) {
  val <- suppressWarnings(as.numeric(pp_res))
  if (length(val) < 1 || is.na(val[1])) return(-Inf)
  val[1]
}

## openCyto PREPROCESSING plugin (register with type = "preprocessing"), method
## name "pp_costain_floor". Computes the Costain-derived gate floor ONCE per
## groupBy group and feeds it to the *_costain gating plugins via pp_res. This is
## the mechanism that lets a template-driven gate use the Costain control, which a
## gating plugin (which only ever sees one sample's flowFrame `fr`) cannot reach
## on its own.
##
## Template row that uses it must set:
##   preprocessing_method  = pp_costain_floor
##   preprocessing_args    = costain_quantile=0.99   (the Costain percentile; the
##                                        SINGLE source of the costain quantile now)
##   groupBy               = Dirname     (one Costain floor per antibody group)
##   collapseDataForGating = FALSE       (gate stays PER-SAMPLE; the floor is the
##                                        only thing shared across the group)
## and pData must carry `StainType` (== "Costain" flags the control well).
##
## `costain_quantile` here is the PROBABILITY (e.g. 0.99) at which the Costain
## well's fluorescence is read off as the floor -- it is the gating_template's
## replacement for the (retired) layout-sheet Costain_quantile column. It comes from
## preprocessing_args (e.g. "costain_quantile=0.99"); if absent it defaults to 0.99.
## The COMPUTED FLOOR (a fluorescence value) is what travels on via pp_res.
##
## `fs` arrives already gated to the row's PARENT population, so the floor is taken
## at the same parent the child gate runs on. All Costain events in the group are
## pooled for the quantile (robust to >1 Costain well). Returns a list named by
## sample so each per-sample gate gets the shared floor; a group with no Costain well
## -> -Inf (no-op floor). Signature order/names follow the openCyto preprocessing
## template: function(fs, gs, gm, channels, groupBy, isCollapse, ...).
pp_costain_floor <- function(fs, gs, gm, channels, groupBy, isCollapse,
                             costain_quantile = NA, ...) {
  chnl <- channels[1]
  pd   <- pData(fs)
  no_floor <- function() setNames(as.list(rep(-Inf, length(fs))), sampleNames(fs))
  if (!"StainType" %in% colnames(pd)) {
    warning("pp_costain_floor: no 'StainType' column in pData; floor set to -Inf (no-op).")
    return(no_floor())
  }
  costain_idx <- which(isLayoutValue(pd$StainType, "Costain"))
  if (length(costain_idx) == 0) {
    warning("pp_costain_floor: no Costain well in this group; floor set to -Inf (no-op).")
    return(no_floor())
  }
  ## Costain percentile from preprocessing_args `costain_quantile`; default 0.99.
  prob <- suppressWarnings(as.numeric(costain_quantile))
  prob <- if (length(prob) < 1 || is.na(prob[1])) 0.99 else prob[1]
  costain_x <- unlist(lapply(costain_idx, function(i) exprs(fs[[i]])[, chnl]))
  floor_val <- unname(stats::quantile(costain_x, probs = prob))
  setNames(as.list(rep(floor_val, length(fs))), sampleNames(fs))
}

## Preprocessing plugin for gate_auto2_costain, registered as "pp_auto2_costain".
## Like pp_costain_floor it computes the group Costain floor (Costain well's value at
## `costain_quantile`), but ALSO makes the group-level static-vs-flowMeans decision that a
## per-sample gating plugin can't. It applies that floor to every sample and computes each
## sample's % POSITIVE (fraction of the PARENT-pop events ABOVE the floor).
##
## The decision needs BOTH conditions to be true of the SAME well:
##   1. SATURATED  -- its % positive exceeds `sat_threshold`, and
##   2. UNIMODAL   -- Hartigan's dip test cannot reject unimodality (p >= `dip_pval`).
## use_static = TRUE if ANY well satisfies both. Rationale: saturation alone does not make
## clustering wrong. A well that is 95% positive but still clearly BIMODAL has a real
## neg/pos boundary for flowMeans to find, and the old saturation-only rule threw that away
## and pinned the whole group at the Costain floor. It is the saturated-AND-unimodal case
## -- one smeared population with no valley -- where a k=2 split is meaningless and would
## drop the boundary into the middle of the positives, so there the Costain floor is used.
## Because both conditions must hold for one well, this is strictly NARROWER than before:
## groups that previously went static may now use flowMeans.
##
## Scope note: saturation is assessed over EVERY well in the group (any(...)), not only the
## highest-StainQty well -- in a titration the top volume is usually the one that trips it,
## but a saturated mid-series well counts too.
##
## Returns, per sample, list(floor = <numeric>, use_static = <logical>), consumed by
## gate_auto2_costain via pp_res -- the return shape is unchanged, so the gating plugin
## needed no edit. (The Costain well itself sits at ~1-costain_quantile positive by
## construction, so it never trips the threshold.)
##   costain_quantile : percentile of the Costain well used as the floor (default 0.99)
##   sat_threshold    : % positive above which a well counts as saturated. Accepts a
##                      fraction (0.90) or a percent (90); default 0.90.
##   dip_pval         : dip-test threshold; a well is "unimodal" when p >= dip_pval
##                      (default 0.05). Identical knob and direction to gate_auto_costain_dip,
##                      where bimodal <- p < dip_pval -- this is just its complement.
##                      CAUTION, the knob runs opposite to the usual reading: LOWERING
##                      dip_pval makes "unimodal" EASIER to satisfy and so makes the static
##                      fallback MORE likely; raising it makes static rarer.
##                      dip_pval = 0 reproduces the old saturation-only behaviour exactly
##                      (every saturated well counts as unimodal); dip_pval = 1 disables the
##                      static path altogether.
##                      Be aware the dip p-value is n-sensitive: at the event counts typical
##                      here p collapses toward 0, so wells are nearly always called BIMODAL
##                      and static mode may effectively never fire. If you need a modality
##                      test that does not drift with n, gate_auto_costain_gmm's Ashman's D
##                      separation guard is the principled alternative.
pp_auto2_costain <- function(fs, gs, gm, channels, groupBy, isCollapse,
                             costain_quantile = NA, sat_threshold = 0.90,
                             dip_pval = 0.05, ...) {
  chnl <- channels[1]
  pd   <- pData(fs)
  per_sample <- function(floor_val, use_static)
    setNames(lapply(seq_along(fs), function(i) list(floor = floor_val, use_static = use_static)),
             sampleNames(fs))
  if (!"StainType" %in% colnames(pd)) {
    warning("pp_auto2_costain: no 'StainType' column; floor -Inf, use_static FALSE (no-op).")
    return(per_sample(-Inf, FALSE))
  }
  costain_idx <- which(isLayoutValue(pd$StainType, "Costain"))
  if (length(costain_idx) == 0) {
    warning("pp_auto2_costain: no Costain well in this group; floor -Inf, use_static FALSE.")
    return(per_sample(-Inf, FALSE))
  }
  prob <- suppressWarnings(as.numeric(costain_quantile)); prob <- if (length(prob) < 1 || is.na(prob[1])) 0.99 else prob[1]
  sat  <- suppressWarnings(as.numeric(sat_threshold));    sat  <- if (length(sat)  < 1 || is.na(sat[1]))  0.90 else sat[1]
  if (sat > 1) sat <- sat / 100                                  # accept 90 as well as 0.90
  costain_x <- unlist(lapply(costain_idx, function(i) exprs(fs[[i]])[, chnl]))
  floor_val <- unname(stats::quantile(costain_x, probs = prob))
  dpv  <- suppressWarnings(as.numeric(dip_pval));         dpv  <- if (length(dpv)  < 1 || is.na(dpv[1]))  0.05 else dpv[1]
  ## % positive per sample = fraction of parent-pop events above the floor
  pct_pos <- vapply(seq_along(fs), function(i) mean(exprs(fs[[i]])[, chnl] > floor_val, na.rm = TRUE), numeric(1))
  sat_idx <- which(pct_pos > sat)
  ## Second condition: only a saturated well that is ALSO unimodal forces the static gate.
  ## The dip test is run ONLY on the saturated wells -- it is the expensive part, and a
  ## non-saturated well cannot trip the rule however it is shaped.
  use_static <- FALSE
  if (length(sat_idx)) {
    unimodal <- vapply(sat_idx, function(i) {
      x <- exprs(fs[[i]])[, chnl]
      x <- x[is.finite(x)]        ## dip.test errors on Inf/-Inf
      tryCatch(diptest::dip.test(x)$p.value >= dpv,
               error = function(e) {
                 ## Can't assess this saturated well -> treat as unimodal, i.e. fall back to
                 ## the Costain floor. Clustering a population we can't even test is the
                 ## riskier of the two options.
                 warning(sprintf("pp_auto2_costain: dip.test failed on '%s' (%s); treating that saturated well as unimodal.",
                                 sampleNames(fs)[i], conditionMessage(e)))
                 TRUE
               })
    }, logical(1))
    use_static <- any(unimodal)
    if (use_static)
      message(sprintf("pp_auto2_costain: static gate at the Costain floor -- %d saturated well(s) (max %.1f%% positive), %d of them unimodal (dip p >= %.3g).",
                      length(sat_idx), 100 * max(pct_pos, na.rm = TRUE), sum(unimodal), dpv))
    else
      message(sprintf("pp_auto2_costain: %d saturated well(s) (max %.1f%% positive) but all still bimodal -- using flowMeans floored at the Costain value.",
                      length(sat_idx), 100 * max(pct_pos, na.rm = TRUE)))
  }
  per_sample(floor_val, use_static)
}

## Custom openCyto gating plugin: flowMeans gate with a Costain-derived floor,
## registered as method "gate_flowmeans_costain" for use from the gating_template
## sheet. Identical to gate_flowmeans (same fr/pp_res/channels/K/cluster/quantile
## semantics) but the resulting boundary is floored at the Costain value.
##   Floor source: pp_res, supplied by the pp_costain_floor preprocessing method
##   (a fluorescence VALUE, not a probability). ALWAYS attach preprocessing_method
##   = pp_costain_floor; with no pp_res the floor is -Inf (a no-op, i.e. plain
##   flowMeans). There is no numeric costain_quantile arg anymore -- the costain
##   percentile lives solely in pp_costain_floor's preprocessing_args.
## `cluster` + `quantile` set the boundary (see .flowmeans_bound), the costain floor
## raises it, and the gate is ALWAYS c(bound, Inf): RIGHT = positive, LEFT = negative
## (side chosen by the template `pop` column).
gate_flowmeans_costain <- function(fr, pp_res, channels, K = 2, cluster = "neg",
                                   quantile = 0.99, ...) {
  chnl  <- channels[1]
  x     <- exprs(fr)[, chnl]; x <- x[is.finite(x)]
  bound <- max(.flowmeans_bound(x, K, cluster, quantile), .resolve_costain_floor(pp_res))
  flowCore::rectangleGate(.gate = setNames(list(c(bound, Inf)), chnl), filterId = "flowmeans_costain")
}

## NOTE: the former custom `gate_static` method was RETIRED -- openCyto's built-in
## `boundary` method makes the identical fixed rectangleGate (1D or 2D). In the sheet
## use gating_method = boundary with gating_args = "min=c(x,y), max=c(x,y)" (each vector
## ordered as the `dims` column; e.g. min=c(6000,-Inf), max=c(Inf,Inf)). For a static
## quadrant, use four boundary rows (one rectangle per quadrant).

## Custom openCyto gating plugin: automatic method selection by Hartigan's dip test,
## registered as method "gate_auto_costain_dip". Accepts every argument gate_flowmeans_costain
## does (K, cluster, quantile) plus dip_pval:
##   dip_pval : p-value threshold for rejecting unimodality (default 0.05).
## Behaviour on channels[1]:
##   - BIMODAL  (dip test p < dip_pval): behaves like gate_flowmeans_costain -- flowMeans
##     k=K clustering, boundary = `quantile` of the chosen cluster, floored at the
##     Costain value.
##   - UNIMODAL (dip test p >= dip_pval, or dip test errors): falls back to a static gate
##     at the Costain floor itself, i.e. the same fixed costain gate is applied to
##     every frame in the group.
## The Costain floor is a fluorescence value supplied by the pp_costain_floor
## preprocessing method via pp_res (see .resolve_costain_floor); ALWAYS attach
## preprocessing_method = pp_costain_floor -- with no pp_res the floor is -Inf, which
## makes BIMODAL behave like plain flowMeans and UNIMODAL degenerate. cluster = "neg"
## keeps events below the boundary, "pos" keeps events above it. Returns a 1D
## rectangleGate on channels[1].
gate_auto_costain_dip <- function(fr, pp_res, channels, K = 2, cluster = "neg",
                              quantile = 0.99, dip_pval = 0.05, ...) {
  chnl <- channels[1]
  x    <- exprs(fr)[, chnl]
  x    <- x[is.finite(x)]   ## dip.test AND flowMeans error on Inf/-Inf; drop non-finite events

  floor_val <- .resolve_costain_floor(pp_res)

  ## Dip test: reject unimodality when p < dip_pval. On a genuine error (rare now that
  ## non-finite values are dropped) WARN and fall back to unimodal -- previously this was
  ## a silent catch that made an errored test masquerade as "unimodal" on every sample.
  bimodal <- tryCatch(
    diptest::dip.test(x)$p.value < dip_pval,
    error = function(e) {
      warning("gate_auto_costain_dip: dip.test failed (", conditionMessage(e), "); treating as unimodal.")
      FALSE
    })

  if (bimodal) {
    ## Bimodal -> flowMeans boundary (cluster+quantile), floored at the costain value.
    bound <- max(.flowmeans_bound(x, K, cluster, quantile), floor_val)
  } else {
    ## Unimodal -> static costain gate: the costain value itself is the boundary.
    bound <- floor_val
  }
  ## Gate is always c(bound, Inf): RIGHT = positive, LEFT = negative (side via `pop`).
  flowCore::rectangleGate(.gate = setNames(list(c(bound, Inf)), chnl), filterId = "auto_costain")
}

## Custom openCyto gating plugin: same idea as gate_auto_costain_dip, but the
## flowMeans-vs-static decision is made by Gaussian-mixture MODEL SELECTION plus a
## cluster-SEPARATION guard, instead of Hartigan's dip test. This avoids the dip
## p-value's over-sensitivity at high event counts (which otherwise calls almost
## everything "bimodal"). Registered as method "gate_auto_costain_gmm". Only the
## decision rule differs from gate_auto_costain_dip -- the boundary maths is identical --
## so the two are directly A/B-comparable by swapping the method name in the template.
##
## Decision on channels[1]:
##   1. Fit 1- and 2-component Gaussian mixtures (mclust::Mclust, G = 1:2); BIC chooses.
##      G = 1                          -> unimodal.
##   2. If G = 2, require real separation: Ashman's D = sqrt(2)*|mu1-mu2| /
##      sqrt(sd1^2 + sd2^2) must be >= `ashman_D` (D ~ 2 = clear separation of two
##      Gaussians). Below threshold the split is treated as arbitrary -> unimodal.
##   BIMODAL  -> flowMeans k=K boundary at `quantile` of the chosen cluster, floored at
##               the Costain value (same as gate_flowmeans_costain).
##   UNIMODAL -> static gate at the Costain floor itself.
##   (mclust failure -> unimodal, the conservative static fallback.)
##
## Args (settable from the template gating_args):
##   K, cluster ("neg"/"pos"), quantile  -- as gate_flowmeans_costain
##   ashman_D : component-separation threshold to accept a 2-cluster split (default 2).
##              Higher = stricter (fewer flowMeans splits). CALIBRATE on your data.
## The Costain floor is a fluorescence value from the pp_costain_floor preprocessing
## method via pp_res; ALWAYS attach preprocessing_method = pp_costain_floor.
gate_auto_costain_gmm <- function(fr, pp_res, channels, K = 2, cluster = "neg",
                                  quantile = 0.99, ashman_D = 2, ...) {
  if (!(cluster %in% c("neg", "pos"))) stop("gate_auto_costain_gmm: `cluster` must be 'neg' or 'pos'")
  chnl <- channels[1]
  x    <- exprs(fr)[, chnl]
  x    <- x[is.finite(x)]   ## mclust AND flowMeans error on Inf/-Inf; drop non-finite events

  floor_val <- .resolve_costain_floor(pp_res)

  ## GMM model selection (G = 1 vs 2, by BIC) + Ashman's D separation guard. On a genuine
  ## error (rare now that non-finite values are dropped) WARN and fall back to unimodal --
  ## previously this silent catch made an errored fit masquerade as "unimodal" everywhere.
  bimodal <- tryCatch({
    fit <- mclust::Mclust(x, G = 1:2, verbose = FALSE)
    if (is.null(fit) || fit$G < 2) {
      FALSE
    } else {
      mu  <- fit$parameters$mean
      v   <- fit$parameters$variance$sigmasq                # "E": scalar; "V": length-2
      sds <- sqrt(if (length(v) == 1) rep(v, 2) else v)
      D   <- sqrt(2) * abs(mu[1] - mu[2]) / sqrt(sds[1]^2 + sds[2]^2)
      is.finite(D) && D >= ashman_D
    }
  }, error = function(e) {
    warning("gate_auto_costain_gmm: mclust failed (", conditionMessage(e), "); treating as unimodal.")
    FALSE
  })

  if (bimodal) {
    bound <- max(.flowmeans_bound(x, K, cluster, quantile), floor_val)
  } else {
    bound <- floor_val
  }
  ## Gate is always c(bound, Inf): RIGHT = positive, LEFT = negative (side via `pop`).
  flowCore::rectangleGate(.gate = setNames(list(c(bound, Inf)), chnl), filterId = "auto_costain_gmm")
}

## Custom openCyto gating plugin, registered as "gate_auto2_costain". Companion to the
## pp_auto2_costain preprocessing plugin, which makes the group-level decision (see there).
## Per sample, reads floor + use_static from pp_res:
##   use_static == TRUE  -> STATIC gate at the Costain floor, identical for every sample in
##                          the group (>=1 sample had % positive above sat_threshold).
##   use_static == FALSE -> flowMeans gate on THIS sample (like gate_flowmeans), with the
##                          boundary floored at the Costain value: bound = max(fm_bound, floor)
##                          -- the gate never drops below the Costain boundary.
## Accepts the gate_flowmeans args: K (clusters), cluster ("neg"/"pos"), quantile. The floor
## and static decision come from pp_res (set preprocessing_method = pp_auto2_costain).
gate_auto2_costain <- function(fr, pp_res, channels, K = 2, cluster = "neg",
                               quantile = 0.99, ...) {
  if (!(cluster %in% c("neg", "pos"))) stop("gate_auto2_costain: `cluster` must be 'neg' or 'pos'")
  chnl <- channels[1]
  x    <- exprs(fr)[, chnl]
  x    <- x[is.finite(x)]   ## flowMeans errors on Inf/-Inf
  ## Extract floor + use_static from pp_res (list from pp_auto2_costain); tolerate a bare numeric.
  floor_val  <- if (is.list(pp_res)) suppressWarnings(as.numeric(pp_res$floor)) else suppressWarnings(as.numeric(pp_res)[1])
  if (length(floor_val) < 1 || is.na(floor_val)) floor_val <- -Inf
  use_static <- is.list(pp_res) && isTRUE(pp_res$use_static)

  if (use_static) {
    bound <- floor_val
  } else {
    bound <- max(.flowmeans_bound(x, K, cluster, quantile), floor_val)
  }
  ## Gate is always c(bound, Inf): RIGHT = positive, LEFT = negative (side via `pop`).
  flowCore::rectangleGate(.gate = setNames(list(c(bound, Inf)), chnl), filterId = "auto2_costain")
}

## ---- Manual (interactive) marker gating via flowGate ---------------------------------
## `gate_manual` can't run *inside* gt_gating (headless, per-sample), and building the
## +/- populations by hand fails because openCyto boolean filters can't reference a
## "+"-named node. So instead we let openCyto build the +/- split itself: gate_manual is
## registered as an openCyto plugin that returns a PRE-DRAWN gate from a cache, and the
## template row keeps pop = "+/-" (openCyto then makes <alias>+ = inside and <alias>- =
## its complement natively, exactly like the auto methods).
##
## Flow (handled in the script, per antibody):
##   1. gt_gating the AUTO template (gate_manual row excluded) -> builds up to the parent.
##   2. drawManualGates(): interactively draw on the parent (grouped by the row's groupBy),
##      cache + persist.
##   3. set .acta_manual_cache$gates <- <that cache>, then gt_gating the FULL template
##      (gate_manual row included) -> openCyto skips existing nodes and builds <alias>+/-.
##
## Supported only on the LEAF marker row for now (no auto gate may depend on a manual one).

## Per-antibody cache of drawn gates, read by the gate_manual plugin during gt_gating.
## $gates maps EACH sample's flowFrame identifier() -> the gate for that sample: wells in
## the same group share one gate object; per-sample (empty groupBy) each gets its own.
.acta_manual_cache <- new.env(parent = emptyenv())
.acta_manual_cache$gates <- NULL

## openCyto gating plugin "gate_manual": returns the pre-drawn gate for the sample being
## gated (keyed by identifier(fr)). Never interactive.
gate_manual <- function(fr, pp_res, channels, ...) {
  gates <- .acta_manual_cache$gates
  if (is.null(gates))
    stop("gate_manual: no drawn gates cached. drawManualGates() must run and set .acta_manual_cache$gates before gt_gating.")
  sn <- flowCore::identifier(fr)
  g  <- gates[[sn]]
  if (is.null(g)) stop(sprintf("gate_manual: no cached gate for sample '%s'.", sn))
  g
}

## Interactively draw the manual gate(s) with flowGate on `parent`, persist them, and
## return the identifier()->gate cache consumed by the gate_manual plugin. Draws on a
## throwaway node (removed after) purely to capture the flowGate-drawn gate object.
##
## Scope is driven by the marker row's `groupBy` column:
##   groupBy = "" / NA            -> PER-SAMPLE: one gate drawn per well.
##   groupBy = pData column(s)    -> ONE gate per distinct group value (":"-separated for
##                                   multiple columns), drawn on the brightest well of the
##                                   group (max StainQty) and shared by all its wells.
##                                   groupBy = "Dirname" => one gate for the whole antibody.
## dims: length-2 resolved channel names. Gates persist as .rds under gate_dir (keyed by
## group label / sample identifier) and are reused on re-run unless redraw=TRUE. Interactive
## session required only when a gate actually has to be drawn.
drawManualGates <- function(gs, dims, parent, groupBy, gate_dir, redraw = FALSE) {
  if (!requireNamespace("flowGate", quietly = TRUE))
    stop("gate_manual needs the 'flowGate' package (BiocManager::install('flowGate')).")
  dir.create(gate_dir, showWarnings = FALSE, recursive = TRUE)
  dimsList <- as.list(as.character(dims))
  ids <- vapply(seq_along(gs),
                function(i) flowCore::identifier(gh_pop_get_data(gs[[i]], parent)), character(1))
  pd  <- pData(gs)
  groupBy <- if (length(groupBy) == 0 || is.na(groupBy)) "" else trimws(as.character(groupBy))
  if (!nzchar(groupBy)) {
    grpLabel <- ids                                   # per-sample: each well is its own group
  } else {
    cols <- trimws(strsplit(groupBy, ":")[[1]])
    miss <- setdiff(cols, colnames(pd))
    if (length(miss)) stop(sprintf("gate_manual groupBy references pData column(s) not found: %s", paste(miss, collapse = ", ")))
    grpLabel <- apply(pd[, cols, drop = FALSE], 1, function(r) paste(r, collapse = ":"))
  }
  draw_one <- function(sample_idx, label) {           # draw -> capture gate -> remove tmp node
    if (!interactive())
      stop(sprintf("gate_manual needs an interactive session (Source in Positron/RStudio) to draw for '%s'.", label))
    flowGate::gs_gate_interactive(gs, filterId = "__manual_tmp", sample = sample_idx,
                                  dims = dimsList, subset = parent)
    g <- gh_pop_get_gate(gs[[sample_idx]], "__manual_tmp")
    flowWorkspace::gs_pop_remove(gs, "__manual_tmp")
    g
  }
  cache <- setNames(vector("list", length(gs)), ids)
  for (grp in unique(grpLabel)) {
    idx   <- which(grpLabel == grp)
    sv    <- suppressWarnings(as.numeric(pd$StainQty[idx]))
    rep_i <- if (all(is.na(sv))) idx[1] else idx[which.max(sv)]   # draw on the brightest well of the group
    f <- file.path(gate_dir, paste0(make.names(grp), "__manual.rds"))
    g <- if (!redraw && file.exists(f)) readRDS(f) else { gg <- draw_one(rep_i, grp); saveRDS(gg, f); gg }
    for (j in idx) cache[[ ids[j] ]] <- g
  }
  cache
}

## Scatter channel names are NOT standardised, so they cannot be matched literally.
## FCS 3.0 and earlier REQUIRED $PnN to be "FS" for forward and "SS" for side scatter; no
## vendor complied, so FCS 3.1 removed every mandated parameter name except TIME. What each
## platform actually writes:
##   Cytek Aurora / BD / CytoFLEX / Attune   FSC-A, SSC-A, SSC-B-A, VSSC-A
##   Bio-Rad ZE5, Propel YETI (Everest)      FS00-A, SS02-A   ($PnS = "FSC 488/10-A")
##   Guava easyCyte (InCyte)                 FSC-HLin, SSC-HLog
##   legacy Beckman Coulter (.LMD, Summit)   FS Lin, SS Log, FS INT, FS PEAK
##   Apogee (EV work)                        SALS, MALS, LALS -- no FSC/SSC at all
## The digits in the ZE5 names are DETECTOR POSITIONS, not scatter types, which is why side
## scatter can land on SS02 rather than SS00.
##
## Until 2_85 this was an anchored `^(FSC|SSC)` and so recognised only the first row. On a ZE5
## file FS00-A/SS02-A were silently classified as FLUORESCENCE: biexp-transformed (distorting
## the Cells/Single_cells gates), given decade axes, and admitted to the $PnR consensus check
## in resolveInstrumentMaxValue() that exists precisely to EXCLUDE scatter.
##
## Deliberately NOT matched: Union Biometrica COPAS `EXT`/`TOF` (axial light loss on a
## large-particle sorter -- not scatter, not an instrument this assay runs on) and mass
## cytometry, which has no scatter parameter at all (`Event_length` is the size surrogate).
## `[BVUYR]?` covers the laser-prefixed side scatter on spectral instruments (VSSC, BSSC).
## The single conventional subfolder a run may keep its artefacts in. actaOQFinish() collects an
## OQ case's export/report/dashboard into <case>/Outputs/ so the whole folder can be deleted after
## a passing run, which puts the export ONE level below the run root. Both the app's pre-flight and
## the dashboard generator look here in addition to the run root -- and only here. Arbitrary
## recursion is deliberately NOT used: it would sweep in exports from nested experiment folders
## belonging to a different run.
##
## ACTA_Dashboard_*.R duplicates this as OUTPUT_SUBDIR instead of importing it -- that script is
## deliberately standalone and never sources this file. Keep the two in sync.
ACTA_OUTPUT_SUBDIR <- "Outputs"

ACTA_SCATTER_RE <- paste0(
  "^(",
    "[BVUYR]?(FSC|SSC|FS|SS)[0-9]*",   # FSC-A, SSC-B-A, VSSC-A, FS00-A, SS02-A, FS Lin, FSC
    "|[SML]ALS",                       # Apogee small/medium/large-angle light scatter
  ")([-._[:space:]]|$)",
  "|SCATTER")                          # descriptive $PnS, e.g. "Forward Scatter"

## The forward-scatter subset, for the one place that needs a specific scatter channel (the
## pseudocolor y axis) rather than "is this scatter at all".
ACTA_FWD_SCATTER_RE <- paste0(
  "^((FSC|FS)[0-9]*",                  # FSC-A, FSC-HLin, FS00-A, FS Lin, FSC
    "|SALS",                           # Apogee: small-angle is the nearest forward equivalent
  ")([-._[:space:]]|$)",
  "|FORWARD")                          # descriptive $PnS, e.g. "Forward Scatter"

## Matches on the trailing separator (or end of string) so that a marker whose name merely
## STARTS with the same letters is not swallowed: "SSEA-4" and "FSP1" both fail, because after
## "SS"/"FS" the next character is a letter rather than a separator, digit or end.
isScatterChannel <- function(ch) {
  !is.na(ch) & nzchar(ch) & grepl(ACTA_SCATTER_RE, ch, ignore.case = TRUE, perl = TRUE)
}

## Is this a fluorescence channel (i.e. one that gets biexp-transformed and a log-like axis)?
## Scatter stays LINEAR -- biexp-ing it distorts the Cells/Single_cells gates, and FlowJo
## shows scatter linear too.
##
## `desc` is the OPTIONAL parallel vector of $PnS descriptions (parallel to `ch`, recycled if
## length 1). It is the fallback for an instrument whose $PnN is opaque -- a bare "P1" whose
## description reads "Forward Scatter" is scatter, and matching the name alone would miss it.
## Callers that have the descriptions to hand should pass them; the four that only ever see
## channel names do not, and behave exactly as before for FSC-A/SSC-A style names.
## Channels that are NOT fluorescence and must never enter the $PnR consensus or be biexp-ed.
## Matched CASE-INSENSITIVELY: the Aurora writes "Time" but the ZE5 writes "TIME", and an exact
## %in% let the ZE5 one through -- it then joined the fluorescence pool, disagreed with everything
## else on $PnR (2147483647 vs 262144), and made resolveInstrumentMaxValue() fail the whole run.
##
## T0/T1 are the ZE5's pulse-shape channels ($PnS "TLSW"/"TMSW"). Same problem: not scatter, not
## named Time, so they were treated as fluorescence.
ACTA_NON_FLUOR <- c("Time", "INFO", "AF-A", "T0", "T1", "TLSW", "TMSW")
## Case- and whitespace-insensitive equality for the layout's ENUM-like text values (StainType,
## Row). These are matched against literals in code, so a well typed `unstained` used to slip past
## `!= "Unstained"` and be titrated as a real sample -- a silent corruption, not an error.
## Descriptive free-text columns (Vendor, Clone, Test_Material...) are never compared, and the
## gating_template's alias/parent/pop are NOT routed through here: those become openCyto node
## names, which are case-sensitive by design.
## Was this titration part of a COMBINATORIAL run, per the analyst's own declaration?
##
## Layout_Plate carries `Combinatorial_group (n)` -- a number, and every group sharing that number
## was stained together in one well. Blank or non-numeric means no combinatorial run. The column is
## DECLARATIVE: it records what happened at the bench and changes nothing about grouping, gating or
## the folder-derived combinatorial detection in actaTitrationGroups(). The two can therefore
## disagree, and that is deliberate -- one is what the analyst did, the other is what the folder
## layout implies, and a disagreement is worth seeing rather than resolving silently.
##
## NUMERIC-NESS IS TESTED PER VALUE, NOT WITH is.numeric() ON THE COLUMN. readxl types a whole
## column at once: one stray text cell anywhere in the sheet makes the column character, and
## `is.numeric(col)` would then be FALSE for EVERY row -- silently turning off the flag for groups
## that were declared correctly. Coercing each value is the only way to get the per-group answer
## the column is asking for.
##
## REDUCED PER GROUP, because Layout_Plate has one row per WELL while the declaration is per
## titration. `statsForExport` keeps only the maxSI row, so reading the value off that single row
## would report "not combinatorial" whenever the analyst filled the number in on some rows and not
## the one that happened to win -- exactly the silent mislabelling this project keeps finding. So:
## the unique non-blank value across the group's rows. Partial filling is fine by design; two
## DIFFERENT numbers in one group is not, and comes back as `conflict` for the caller to grade
## (pre-flight makes it fatal; the run loop degrades to NA and warns).
actaCombinatorialGroup <- function(values, group_label = NA_character_) {
  raw <- trimws(as.character(values))
  raw <- raw[!is.na(raw) & nzchar(raw) & !toupper(raw) %in% c("NA", "NAN")]
  if (!length(raw))
    return(list(value = NA_character_, is_combo = FALSE, conflict = NA_character_))
  u <- unique(raw)
  if (length(u) > 1L)
    return(list(value = NA_character_, is_combo = FALSE,
                conflict = sprintf(paste0("titration '%s' declares more than one ",
                                          "Combinatorial_group: %s. One titration was stained in ",
                                          "one well, so it belongs to at most one group."),
                                   group_label, paste(sprintf("'%s'", u), collapse = ", "))))
  ## The value is reported VERBATIM -- a non-numeric entry is shown as typed, next to a FALSE, so
  ## the analyst can see what they wrote rather than having it silently blanked.
  list(value = u[[1]], is_combo = actaIsCombinatorialValue(u[[1]]),
       conflict = NA_character_)
}

## The numeric test, in one place so the export column, the pre-flight and the tests cannot drift.
## A whole number is not required: the column groups, it does not count.
actaIsCombinatorialValue <- function(x) {
  v <- suppressWarnings(as.numeric(trimws(as.character(x))))
  length(v) == 1L && !is.na(v) && is.finite(v)
}

## Strip a trailing parenthetical annotation from layout header names.
##
## The sheet's headers document themselves for whoever fills them in -- which FCS slot a column is
## matched against, and what type belongs in it:
##   Markername ($PnS)  Channel ($PnN)   Fix (L)  Perm (L)   StainQty (N)  e6_cells_per_well (N)
## Stripping them ONCE at read time is what keeps that free: the rest of the pipeline goes on
## referring to `StainQty`, `Fix`, `Markername` and so on, so an annotation can be added,
## reworded or removed without touching a line of code. The alternative -- teaching every read site
## about suffixes -- would mean chasing dozens of literal column names, and missing one fails
## silently (a dropped well, a defaulted flag) rather than loudly.
##
## Only a trailing `(...)` is removed, so a name that legitimately contains parentheses mid-string
## survives. A collision after stripping is a hard error: two columns reduced to the same name
## would make every downstream lookup ambiguous and silently pick one.
actaStripHeaderAnnotation <- function(nms) {
  out <- trimws(sub("\\s*\\([^()]*\\)\\s*$", "", nms))
  dup <- unique(out[duplicated(out) & nzchar(out)])
  if (length(dup))
    stop(sprintf(paste0("Layout_Plate headers collide once their type/slot annotations are ",
                        "stripped: %s. Rename so each column has a distinct base name."),
                 paste(sprintf("'%s' <- (%s)", dup,
                               vapply(dup, function(d) paste(nms[out == d], collapse = " + "),
                                      character(1))), collapse = "; ")), call. = FALSE)
  out
}

## Resolve a layout column by any accepted spelling of its name.
##
## Headers carry an FCS-slot suffix as of 2_85 -- `Markername ($PnS)`, `Channel ($PnN)` -- and the
## code must NOT match those literally. The suffix is documentation for whoever fills the sheet,
## so it has to be free to change; the pre-2_85 `Dim` header must keep working; and 20 workbooks
## in the older version folders predate all of it.
##
## Comparison strips everything but letters and digits, so `Markername ($PnS)`, `Markername (PnS)`,
## `markername(pns)` and `Markername` all resolve to the same column. That also makes it immune to
## R mangling the name in passing -- `merge()`/`data.frame()` turn "Markername ($PnS)" into
## "Markername...PnS." and the normalised form is unchanged.
##
## Accepts a data.frame/list (uses names()) or a character vector of names. Returns the ACTUAL
## name as spelled in the sheet, so messages can quote what the analyst actually typed, or NA.
actaLayoutCol <- function(x, aliases) {
  nms <- if (is.character(x)) x else names(x)
  if (!length(nms)) return(NA_character_)
  norm <- function(v) gsub("[^a-z0-9]", "", tolower(v))
  nn <- norm(nms)
  for (a in aliases) {
    hit <- which(nn == norm(a))
    if (length(hit)) return(nms[hit[1]])
  }
  NA_character_
}

## Accepted spellings, most-preferred first. Normalisation makes the `$` and the spacing
## irrelevant, so one entry covers "Markername ($PnS)" and "Markername (PnS)" alike.
ACTA_MARKER_COLS  <- c("Markername ($PnS)", "Markername", "Dim")
ACTA_CHANNEL_COLS <- c("Channel ($PnN)", "Channel")

isLayoutValue <- function(x, value) {
  !is.na(x) & toupper(trimws(as.character(x))) == toupper(trimws(value))
}

isFluorChannel <- function(ch, desc = NULL) {
  scatter <- isScatterChannel(ch)
  if (!is.null(desc) && length(desc)) {
    if (length(desc) == 1L) desc <- rep(desc, length(ch))
    if (length(desc) == length(ch)) scatter <- scatter | isScatterChannel(desc)
  }
  !is.na(ch) & nzchar(ch) & !scatter &
    !(toupper(trimws(ch)) %in% toupper(ACTA_NON_FLUOR))
}

## Pick the channel to use wherever the pipeline needs "the forward scatter one" (currently
## only the pseudocolor y axis, which used a hardcoded "FSC-A"). AREA is preferred over height
## over width, because area is what FlowJo plots and what 2_84/2_85 exports were built on;
## analog-era BD files carry only -H. If the instrument has no forward scatter at all (Apogee)
## the first scatter channel of any kind is returned, and legacy Beckman names that lack an
## A/H/W suffix entirely ("FS Lin") fall through to the same first-match rule.
pickForwardScatter <- function(channels) {
  sc <- channels[isScatterChannel(channels)]
  if (!length(sc))
    stop(sprintf(paste0("pickForwardScatter: no scatter channel recognised among: %s.\n",
                        "Scatter names are vendor-specific -- see ACTA_SCATTER_RE. Add this ",
                        "instrument's pattern there rather than renaming the FCS."),
                 paste(channels, collapse = ", ")), call. = FALSE)
  fwd  <- sc[grepl(ACTA_FWD_SCATTER_RE, sc, ignore.case = TRUE, perl = TRUE)]
  cand <- if (length(fwd)) fwd else sc
  for (sfx in c("A", "H", "W")) {
    hit <- grepl(paste0("(^|[-._[:space:]])", sfx, "(Lin|Log)?$"), cand,
                 ignore.case = TRUE, perl = TRUE)
    if (any(hit)) return(cand[hit][1])
  }
  cand[1]
}

## Resolve the biexp `maxValue` from the INSTRUMENT rather than hardcoding it: every FCS
## declares each parameter's range in the `$PnR` keyword ($PnR = 4194304 on a 22-bit Aurora,
## i.e. 6.62 decades). Hardcoding a smaller value (2_83 and earlier used 522144 = 5.72
## decades) both truncates the display AND pushes the brightest events past `channelRange`,
## so they are transformed off-scale before gating ever sees them.
##
## Only headers are read, so this is cheap. Scatter and Time are excluded -- Time in
## particular has a completely different range and would poison the check.
##
## A DISAGREEMENT IS A HARD ERROR, by design: one maxValue has to apply to the whole run.
## If each antibody group got its own, the biexp mapping would differ between groups and
## their Stain Indices would no longer be comparable *within a single experiment*.
resolveInstrumentMaxValue <- function(fcs_files) {
  if (!length(fcs_files)) stop("resolveInstrumentMaxValue: no FCS files supplied.", call. = FALSE)
  seen <- list()
  for (f in fcs_files) {
    kw <- tryCatch(flowCore::read.FCSheader(f)[[1]], error = function(e) NULL)
    if (is.null(kw)) next
    np <- suppressWarnings(as.integer(kw[["$PAR"]]))
    if (is.na(np) || np < 1) next
    ## Name-membership test, NOT `is.null(kw[[k]])`: read.FCSheader() returns a named CHARACTER
    ## VECTOR, and `[[` on one ERRORS ("subscript out of bounds") for a name that is absent
    ## rather than returning NULL. The old form only survived because $PnN and $PnR are always
    ## present; $PnS frequently is NOT (instruments rarely describe scatter), so reading it
    ## needs the guard.
    getk <- function(i, s) { k <- paste0("$P", i, s); if (k %in% names(kw)) kw[[k]] else NA }
    nm <- vapply(seq_len(np), function(i) { v <- getk(i, "N"); if (is.na(v)) NA_character_ else trimws(v) }, character(1))
    rg <- vapply(seq_len(np), function(i) suppressWarnings(as.numeric(getk(i, "R"))), numeric(1))
    ## $PnS as well as $PnN: this is the one call site reading raw headers, so the descriptions
    ## are free here, and they are what identifies scatter on an instrument with opaque short
    ## names (ZE5 writes $PnN = "FS00-A" but $PnS = "FSC 488/10-A"). A scatter channel wrongly
    ## admitted here can only make the run FAIL -- its $PnR disagrees with the fluorescence
    ## channels' and the consensus check is a hard error -- so widening the exclusion is the
    ## conservative direction.
    ds <- vapply(seq_len(np), function(i) { v <- getk(i, "S"); if (is.na(v)) "" else trimws(v) }, character(1))
    keep <- !is.na(nm) & !is.na(rg) & isFluorChannel(nm, ds)
    if (any(keep)) seen[[basename(f)]] <- sort(unique(rg[keep]))
  }
  vals <- sort(unique(unlist(seen, use.names = FALSE)))
  if (!length(vals))
    stop("resolveInstrumentMaxValue: no fluorescence $PnR found in any FCS header.", call. = FALSE)
  if (length(vals) > 1)
    stop(sprintf(paste0("Instrument range ($PnR) is not consistent across the fluorescence channels/files: %s.\n",
                        "One maxValue must apply to the whole run -- a per-group transform would make Stain ",
                        "Indices incomparable between groups. Per file:\n%s"),
                 paste(format(vals, scientific = FALSE), collapse = " vs "),
                 paste(sprintf("  %-58s %s", names(seen),
                               vapply(seen, function(v) paste(format(v, scientific = FALSE), collapse = ", "),
                                      character(1))), collapse = "\n")),
         call. = FALSE)
  vals
}

## The FlowJo biexponential as a flowWorkspace TRANSFORMER (not a bare transform function),
## applied to the GatingSet with transform(gs, ...). Registering it this way is what lets
## ggcyto relabel the axes in raw units via axis_*_inverse_trans() -- pre-transforming the
## flowSet leaves the GatingSet unaware and the axis renders as plain linear channel numbers.
## The transformed VALUES are identical either way, so this choice alone changes no statistic.
##
## Parameters (2_84; contrast with 2_83 and earlier):
##   maxValue    from $PnR      (was hardcoded 522144)  -- the decade span
##   widthBasis  -10            (was -50)  -- width of the linear region around zero
##   neg          0             (was -2.5) -- displayed negative decades
##   pos          4.5           unchanged  -- already the FlowJo default
##   channelRange 9000          unchanged  -- internal resolution only; with an inverse-
##                                            labelled axis it has no visual effect
## These DO change the transformed values, hence MFI, MAD and Stain Index. 2_84 exports are
## deliberately not comparable with 2_83 and earlier.
## Parse and validate Layout_Plate!flowjo_transformation_arg.
##
## One transform per RUN, not per group: Stain Indices are compared across groups in the same
## report, and a different transform per group would make that comparison meaningless. The column
## sits per row only so it is editable where the rest of the group's settings are -- disagreement
## between groups is therefore an error, not a per-group override.
##
## maxValue is deliberately NOT accepted here: it comes from $PnR, and letting the sheet override
## the instrument's own range is a way to silently mis-scale every statistic.
## ============ WHICH VERSIONS DID THIS RUN USE ============
## Security review of 2_94, finding M-1: every package is installed with a bare install.packages()
## / BiocManager::install() and there is no lockfile, so two analysts running Setup on different
## days can end up on different versions of the code that computes a Stain Index. That is a
## reproducibility problem before it is a security one, and for a GxP-adjacent tool the
## reproducibility half is what an auditor asks about.
##
## FULL renv adoption is a separate decision -- it changes the install path for every user and
## pinning the Bioconductor flow stack is not a small change -- so this is the review's own stated
## MINIMUM: record the exact versions, per run, in a form a later Setup can be checked against.
## The package list is read from the SCRIPT rather than duplicated here, for the same reason
## Setup.R reads it: two copies of the manifest drift, and the copy nobody runs is the wrong one.
actaPackageVersions <- function(code_dir = NULL, packages = NULL) {
  if (is.null(packages)) {
    packages <- character(0)
    sc <- if (!is.null(code_dir))
            list.files(code_dir, pattern = "^ACTA_Script.*[.]R$", full.names = TRUE) else character(0)
    if (length(sc)) {
      txt <- tryCatch(readLines(sc[[1]], warn = FALSE), error = function(e) character(0))
      hit <- grep("^\\s*listOfLibrary\\s*<-\\s*c\\(", txt)
      if (length(hit)) {
        v <- sub(".*c\\(", "", txt[hit[[1]]]); v <- sub("\\).*$", "", v)
        packages <- gsub('^"|"$', "", trimws(strsplit(v, ",")[[1]]))
      }
    }
  }
  packages <- unique(c("base", packages))
  data.frame(package = packages,
             version = vapply(packages, function(p)
               tryCatch(as.character(utils::packageVersion(p)),
                        error = function(e) NA_character_), character(1), USE.NAMES = FALSE),
             stringsAsFactors = FALSE)
}

## Compare a run's versions against the baseline recorded at OQ time. Returns the drift, one line
## per package that moved or went missing -- NOT a pass/fail: versions legitimately move, and a
## check that fails on every routine upgrade gets switched off. What matters is that the drift is
## VISIBLE and recorded next to the verdicts it might explain.
actaVersionDrift <- function(current, baseline) {
  if (is.null(baseline) || !nrow(baseline)) return(character(0))
  m <- merge(baseline, current, by = "package", all.x = TRUE, suffixes = c("_was", "_now"))
  d <- m[is.na(m$version_now) | m$version_was != m$version_now, , drop = FALSE]
  if (!nrow(d)) return(character(0))
  sprintf("%s %s -> %s", d$package, d$version_was,
          ifelse(is.na(d$version_now), "MISSING", d$version_now))
}

## ============ WORKBOOK ARGUMENT CELLS ARE DATA, NEVER CODE ============
## Security review of 2_94, finding H-1: `Layout_Plate!flowjo_transformation_arg` was read straight
## out of the workbook and handed to `eval(parse(text = ...))`. The argument NAMES were validated
## afterwards, which is too late -- by then any R expression in that cell has already run, with the
## analyst's privileges. These workbooks travel: emailed, OneDrive/SharePoint-synced, handed between
## sites and CROs. A tampered one could alter Stain Indices before they reach a report, or do
## anything else the user can do.
##
## THE SAME EXPOSURE EXISTS IN `gating_args` / `preprocessing_args`, which the review missed.
## openCyto's own `.argParser()` only PARSES (it returns language objects, no eval), but
## `.gating_adaptor()` then does `do.call(gFunc, c(list(fr = fr, ...), args))` and R evaluates each
## argument the moment the plugin reads it. Demonstrated 2026-09-04: a cell reading
## `K=2, quantile=(function(){ ... })()` executes as soon as a plugin touches `quantile`, and every
## ACTA plugin reads `quantile`/`K`. It is also why `cluster=neg` once died with
## `object 'neg' not found` AFTER three gates had already run -- that error was evaluation.
##
## We cannot stop openCyto evaluating what we hand it, so the defence is to refuse to hand it
## anything that could do more than name a value. `parse()` is used WITHOUT `eval()` throughout:
## parsing is not executing, and it buys R's own tokenizer instead of a hand-rolled comma splitter
## that would break on `min=c(1.5e6,-Inf)`.

## Calls a workbook argument cell may legitimately contain. Derived from EVERY distinct arg cell in
## every shipped and working workbook (15 of them, surveyed 2026-09-04): `c` for the boundary
## vectors, `list` for flowClust's `prior=list(NA)`, and unary minus for `-Inf`. Nothing else has
## ever appeared, so nothing else is allowed. An allowlist, because the set of harmful functions is
## unbounded and the set of useful ones here is three.
ACTA_ARG_CALL_ALLOW <- c("c", "list", "-", "+")

## Every function name in a parse tree that is NOT on the allowlist. Returns character(0) for a
## tree built only from literals and permitted constructors.
actaDisallowedCalls <- function(x, allow = ACTA_ARG_CALL_ALLOW) {
  bad <- character(0)
  walk <- function(e) {
    ## parse() returns an EXPRESSION, and is.call() on an expression is FALSE -- so a walk that
    ## only recognises calls returns character(0) for every input and the check silently passes
    ## everything. That is exactly how this function first shipped in draft; the hostile-cell
    ## fixtures in tests/test_arg_safety.R are what caught it.
    if (is.expression(e) || is.list(e)) {
      for (i in seq_along(e)) walk(e[[i]])
      return(invisible(NULL))
    }
    if (!is.call(e)) return(invisible(NULL))
    hd <- e[[1]]
    ## A call whose HEAD is itself a call -- `(function(){...})()`, `get("system")()` -- can never
    ## be on a name allowlist, so deparse it and let it fail by name.
    nm <- if (is.symbol(hd)) as.character(hd) else paste(deparse(hd), collapse = "")
    if (!nm %in% allow) bad <<- c(bad, nm)
    for (i in seq_along(e)[-1L]) if (!identical(e[[i]], quote(expr = ))) walk(e[[i]])
    invisible(NULL)
  }
  walk(x)
  unique(bad)
}

## A NUMERIC scalar from a parse tree, without evaluating it. Accepts `4.5` and `-10` (which parses
## as a call to `-`), and nothing else -- these arguments are all numeric scalars. Returns
## NA_real_ for anything it will not convert, so the caller can name the offending argument.
actaNumericLiteral <- function(x) {
  if (is.numeric(x) && length(x) == 1L) return(as.numeric(x))
  if (is.call(x) && length(x) == 2L && is.symbol(x[[1]]) &&
      as.character(x[[1]]) %in% c("-", "+") && is.numeric(x[[2]]) && length(x[[2]]) == 1L)
    return(if (as.character(x[[1]]) == "-") -as.numeric(x[[2]]) else as.numeric(x[[2]]))
  NA_real_
}

actaTransArgs <- function(metadata, groups, col = "flowjo_transformation_arg") {
  hit <- match(tolower(col), tolower(names(metadata)))
  if (is.na(hit)) return(list())                       # column absent -> actaBiexpTrans() defaults
  ## FOLDERS, not titration keys. This is a run-level check -- one transform must cover every group
  ## or Stain Indices are not comparable -- so it reads every row belonging to any group. Handed
  ## keys, `Dirname %in% ...` matched nothing, `v` came back empty, and an empty `v` is the
  ## documented "nothing specified, use defaults" path: the sheet's transform was ignored SILENTLY
  ## and every SI in the run was computed on different arguments than the workbook asked for.
  v <- trimws(as.character(metadata[[hit]][!is.na(metadata$Dirname) &
                                           metadata$Dirname %in% .actaAsGroups(groups)$dirname]))
  v <- v[!is.na(v) & nzchar(v) & toupper(v) != "NA"]
  if (!length(v)) return(list())                       # all blank -> defaults
  u <- unique(gsub("\\s+", "", v))
  if (length(u) > 1)
    stop(sprintf(paste0("Layout_Plate!%s differs between antibody groups: %s. One transform must ",
                        "apply to the whole run, or Stain Indices are not comparable between groups."),
                 col, paste(sprintf("'%s'", unique(v)), collapse = " vs ")), call. = FALSE)
  ## PARSED, NEVER EVALUATED -- see the block above actaDisallowedCalls(). The parse tree is walked
  ## for numeric literals; a cell containing anything else is refused by name.
  expr <- tryCatch(parse(text = paste0("list(", v[1], ")")),
                   error = function(e)
                     stop(sprintf("Layout_Plate!%s is not readable as arguments (%s): %s",
                                  col, conditionMessage(e), v[1]), call. = FALSE))
  parts <- as.list(expr[[1]])[-1L]
  ## `list` is the wrapper this function adds itself, and unary +/- is how a negative literal
  ## parses; `c` is deliberately NOT allowed, because every transform argument is a scalar.
  nogo <- actaDisallowedCalls(expr, allow = c("list", "-", "+"))
  if (length(nogo))
    stop(sprintf(paste0("Layout_Plate!%s must contain only numbers, e.g. 'pos=4.5, ",
                        "widthBasis=-10'. It calls %s, and this cell is never evaluated as code. ",
                        "Value: %s"),
                 col, paste(sprintf("`%s`", nogo), collapse = ", "), v[1]), call. = FALSE)
  args <- lapply(parts, actaNumericLiteral)
  nonnum <- names(args)[vapply(args, function(z) !is.finite(z), logical(1))]
  if (length(nonnum) || (length(args) && !length(names(args))))
    stop(sprintf(paste0("Layout_Plate!%s takes numeric values only; %s could not be read as a ",
                        "number. Value: %s"),
                 col, paste(sprintf("`%s`", if (length(nonnum)) nonnum else "the argument"),
                            collapse = ", "), v[1]), call. = FALSE)
  ok <- names(formals(actaBiexpTrans))
  ok <- setdiff(ok, "maxValue")
  bad <- setdiff(names(args), ok)
  if (length(bad))
    stop(sprintf(paste0("Layout_Plate!%s has unknown argument(s): %s. Accepted: %s. maxValue is ",
                        "taken from $PnR and cannot be set here."),
                 col, paste(bad, collapse = ", "), paste(ok, collapse = ", ")), call. = FALSE)
  if (!length(names(args)) || any(!nzchar(names(args))))
    stop(sprintf("Layout_Plate!%s must be name=value pairs, e.g. 'pos=4.5, widthBasis=-10'.", col),
         call. = FALSE)
  args
}

actaBiexpTrans <- function(maxValue, channelRange = 9000, pos = 4.5, neg = 0, widthBasis = -10) {
  flowjo_biexp_trans(channelRange = channelRange, maxValue = maxValue,
                     pos = pos, neg = neg, widthBasis = widthBasis)
}
actaTransformerList <- function(channels, trans) {
  fluor <- channels[isFluorChannel(channels)]
  if (!length(fluor))
    stop("actaTransformerList: no fluorescence channels found to transform.", call. = FALSE)
  transformerList(fluor, trans)
}

## LOWER limit for a biexp axis, in TRANSFORMED units. Companion to biexpFloorLimits(), which
## only ever raises the CEILING; this is the only thing that bounds the BOTTOM.
##
## THE NEGATIVE ARM OF A BIEXP IS NEAR-LINEAR WHILE THE POSITIVE ARM IS LOG, so the two are not
## commensurable and a percentile ALONE cannot bound the axis. Measured on DATASET-1
## (maxValue 4,194,304, widthBasis -10): trans(0) = 1,000 and trans(maxValue) = 9,001, so EVERY
## positive decade TOGETHER spans 8,001 units -- while trans(-1e4) = -3,750,
## trans(-1e5) = -44,779 and trans(-maxValue) = -1,911,261. A single event at raw -1e5 therefore
## sits 5.7x the entire positive axis below zero.
##
## That is why percentile floors fail here in BOTH directions:
##   * the marker histogram floored at `min` over FILES of each file's p0.01. Pooled, APC-A's p0.01
##     is raw -401 (transformed +749) -- perfectly tame. But ONE well reached raw ~-200,000, and
##     that well's 1st percentile alone put the floor at -90,192, leaving 95.2% of every panel
##     empty and the whole density in a spike at the right edge.
##   * the ConcatPlot's per-facet 0.1% quantile is right for facets that are whole titration
##     SERIES, but on this dataset the APC facet's own q0.001 is -7,149 and would take 64% of a
##     shared panel for 0.14% of the events.
##
## So: a percentile GUARDED BY A SHARE OF THE POSITIVE SPAN. The negative band can never exceed
## share/(1+share) of the panel -- share = 0.6 -> at most 37.5%. Only 0.14% of APC-A and 0.21% of
## BV421-A lie below the guarded floor, so it costs a fraction of a percent and buys back most of
## the panel. Callers report what falls outside the view; this function only picks the number.
##
## `share = Inf` disables the guard and returns `cand` unchanged, for a caller that has already
## bounded its own axis.
actaBiexpFloor <- function(cand, hi, trans, share = 0.6) {
  z <- trans$transform(0)
  if (!is.finite(cand)) return(z)
  if (!is.finite(hi) || !is.finite(share) || hi <= z) return(cand)
  max(cand, z - share * (hi - z))
}

## Floor for a biexp axis: never show less than `min_raw` (10^4 by default), with `expand`
## (10%) of extra headroom past it so the top decade tick is not jammed against the panel
## edge. Without this a dim marker auto-scales to a sliver of the low decades -- the axis then
## carries no decade label at all except "0", and panels are not visually comparable because
## each one has its own scale. Only the UPPER limit is raised; the lower limit, and any axis
## already wider than the floor, are left alone. Linear (scatter) axes are returned unchanged.
## The 10% is applied in TRANSFORMED space, so it is 10% of axis length, not 10% of 10^4.
biexpFloorLimits <- function(lim, ch, trans, min_raw = 1e4, expand = 1.10) {
  if (is.null(trans) || is.null(ch) || length(ch) != 1 || !isFluorChannel(ch)) return(lim)
  if (length(lim) != 2 || !all(is.finite(lim))) return(lim)
  hi <- trans$transform(min_raw) * expand
  c(lim[1], max(lim[2], hi))
}

## Decade breaks for an already-biexp-transformed axis, labelled in RAW units (0, 10^3 ...
## 10^6) FlowJo-style.
##
## Why not ggcyto's axis_*_inverse_trans()? Its break set is fixed and, at these parameters,
## puts "0" at 1000 and "10^2" at 1063 -- 0.7% of the axis apart, so the two labels collide
## into an unreadable blob at any rotation, and its `n` argument does not change the set.
## Here the candidate decades are filtered by a MINIMUM SPACING (`min_gap`, a fraction of the
## full transformed range), which drops whichever labels fall inside the compressed linear
## region near zero. Verified not to disturb ggcyto_par_set(limits=) in either order.
##
## This is a pure relabelling of already-transformed values -- NOT scale_x_flowjo_biexp(),
## which would transform the data a SECOND time (it collapsed a real axis to a single "10^3"
## and shrank the range from 120..8659 to 551..1625 in testing).
biexpScale <- function(trans, maxValue, axis = c("x", "y"), min_gap = 0.10, limits = NULL,
                       expand = ggplot2::waiver()) {
  axis <- match.arg(axis)
  ## Negative decades are labelled too. The biexp is defined below zero and the panel routinely
  ## extends there, so an unlabelled negative strip left the reader unable to tell -10^3 from
  ## -10^5. The minor ticks were mirrored below zero when this function was written; the MAJORS
  ## were not, which is why the sub-zero region had grid but no numbers.
  .dec <- 10^(1:ceiling(log10(maxValue)))
  .dec <- .dec[.dec <= maxValue]
  maj  <- c(-rev(.dec), 0, .dec)
  mpos <- trans$transform(maj)
  ok   <- is.finite(mpos); maj <- maj[ok]; mpos <- mpos[ok]

  ## Spacing is judged against the VISIBLE span, not the full 0..maxValue transform range.
  ## Using the full range over-thins a zoomed axis: on a dim marker the panel may stop before
  ## 10^4, and a gap that is a large fraction of what is shown still looks small next to the
  ## whole 6.6-decade range -- which left some panels labelled with nothing but "0".
  full <- diff(range(trans$transform(c(0, maxValue))))
  span <- if (!is.null(limits) && length(limits) == 2 && all(is.finite(limits)) &&
              diff(range(limits)) > 0) diff(range(limits)) else full
  vis  <- if (!is.null(limits) && length(limits) == 2 && all(is.finite(limits)))
            mpos >= min(limits) & mpos <= max(limits) else rep(TRUE, length(mpos))
  if (!any(vis)) vis <- rep(TRUE, length(mpos))
  maj <- maj[vis]; mpos <- mpos[vis]

  ## THE CULL WALKS OUTWARD FROM ZERO, so 0 can never be the label that gets dropped. Walking
  ## bottom-up (the previous behaviour, harmless while every break was >= 0) keeps whichever break
  ## comes FIRST: with negative decades present that is -10^k, and 0 would then be culled for
  ## sitting too close to it. On DATASET-2 trans(-1e3) = 394 against trans(0) = 1000 -- a gap of
  ## 606 on a ~10,500-unit span, i.e. 6%, under min_gap -- so the axis would have lost its zero.
  keep <- rep(FALSE, length(mpos))
  z <- which(maj == 0)
  if (!length(z)) {                    # zero not visible (a panel zoomed off the origin)
    z <- 1L                            # fall back to the old anchor: the lowest visible break
  }
  z <- z[[1]]; keep[z] <- TRUE
  ## THE INNERMOST NEGATIVE BREAK GETS A RELAXED GAP (min_gap/2); the positive side is untouched.
  ## Without it the negative side stays unlabelled in practice: -10^3 is the only negative decade
  ## that ever lands near zero (trans(-1e3) = 394 against trans(0) = 1000, while
  ## trans(-1e4) = -3,750), and that 606-unit gap is ~6% of a typical panel -- just under min_gap.
  ## This extends the rule the fallback below already states, "always leave at least one labelled
  ## decade beyond 0", to the side that had NO label at all.
  ##
  ## Deliberately asymmetric. Relaxing the positive side too admits 10^3 next to the 0 on every
  ## figure in the project -- arguably a nicer axis, but it re-crowds panels that min_gap was
  ## raised to 10% to fix, and nobody asked for it. The positive arm always had at least one
  ## label; the negative arm had none. Fix what was broken.
  ##
  ## The min_gap/2 floor still holds: two labels printed on top of each other are worse than an
  ## unlabelled strip, so a panel zoomed until -10^3 sits 1% from zero stays unlabelled.
  last <- mpos[z]
  if (z < length(mpos)) for (i in (z + 1L):length(mpos))
    if ((mpos[i] - last) / span >= min_gap) { keep[i] <- TRUE; last <- mpos[i] }
  last <- mpos[z]; .g <- min_gap / 2
  if (z > 1L) for (i in (z - 1L):1L)
    if ((last - mpos[i]) / span >= .g) { keep[i] <- TRUE; last <- mpos[i]; .g <- min_gap }
  ## Always leave at least one labelled decade beyond 0 when the visible range contains one.
  ## Add back the dropped candidate FURTHEST from the anchor, i.e. the best separated from it.
  if (sum(keep) < 2 && length(mpos) >= 2) {
    dropped <- which(!keep)
    if (length(dropped)) keep[dropped[which.max(abs(mpos[dropped] - mpos[z]))]] <- TRUE
  }
  labs <- parse(text = ifelse(maj[keep] == 0, "0",
                              sprintf("%s10^%.0f", ifelse(maj[keep] < 0, "-", ""),
                                      round(log10(abs(maj[keep]))))))

  ## MINOR: 2..9 subdivisions of each decade, drawn as unlabelled ticks (FlowJo-style).
  mnr  <- as.vector(outer(2:9, 10^(0:ceiling(log10(maxValue)))))
  mnr  <- sort(unique(mnr[mnr <= maxValue]))
  ## NEGATIVE counterparts too. The biexp is defined below zero -- that is the whole point of it --
  ## and FlowJo draws minor ticks there, so the strip left of 0 should not be blank.
  mnr  <- sort(unique(c(-mnr, mnr)))
  npos <- trans$transform(mnr)
  ## Clip to the VISIBLE window only when the caller supplied one. Clipping to the MAJOR break range
  ## otherwise (min(mpos)..max(mpos)) was why gridlines stopped dead at 0 and again at the top
  ## labelled decade: majors run 0..1e5 here while the panel extends below 0 and above 1e5, so both
  ## margins were blank by construction. ggplot silently drops minor breaks outside the panel, so
  ## leaving them unclipped costs nothing and lets the grid reach the panel edges.
  if (!is.null(limits) && length(limits) == 2 && all(is.finite(limits)))
    npos <- npos[is.finite(npos) & npos >= min(limits) & npos <= max(limits)]
  else
    npos <- npos[is.finite(npos)]
  gd   <- guide_axis(minor.ticks = TRUE)
  ## `expand` widens the DRAWN range without touching `limits`, so it buys margin for labels that
  ## sit at the very edge of a panel without changing what data is shown or where a gate falls.
  ## waiver() = ggplot's default 5% both sides.
  if (axis == "x") scale_x_continuous(breaks = mpos[keep], labels = labs,
                                      minor_breaks = npos, guide = gd, expand = expand)
  else             scale_y_continuous(breaks = mpos[keep], labels = labs,
                                      minor_breaks = npos, guide = gd, expand = expand)
}



## Apply the decade axis PER AXIS, only where that channel is fluorescence, so scatter axes
## (pseudocolor y = FSC-A, the Cells FSC/SSC layout) keep plain linear ticks.
## Adds to the plot and returns it rather than returning layers: ggcyto's `+` SILENTLY IGNORES
## a list() of layers -- `p + list(scale)` leaves the axis untouched with no warning.
## Locate the marker +/- populations, from the MARKER ROW rather than by scanning the tree.
##
## This replaces `grep("[+]$", basename(gs_get_pop_paths(gs)))[1]`, whose comment claimed that "only
## the +/- template row yields suffixed names, so this is unambiguous". That is false the moment any
## INTERMEDIATE gate is also pop = "+/-": the marker gate is the deepest node, so a "+/-" ancestor
## always precedes it in tree order and [1] picks the ancestor.
##
## Measured 2026-08-18 on an internal run: a template with CD3, MarkerB and Reagent all pop = "+/-" gave
## the tree root, Cells, Single_cells, Live, CD3+, CD3-, MarkerB+, MarkerB-, FSC-A+, FSC-A-, and the old code
## selected CD3+/CD3-. It surfaced as `Error: NA not found!` from openCyto:::.getNode -- but the crash
## was the LUCKY outcome. When the wrong pair resolves cleanly, the Stain Index, MFI and every marker
## plot are computed on the WRONG GATE with no error at all. Neither OQ case caught it, because in
## both of them Reagent is the only "+/-" row and [1] is right by accident.
##
## Note openCyto names "+/-" nodes from the FIRST `dims` TOKEN, not the alias -- it says so:
## "alias 'Reagent' is ignored since pop names are auto-generated from 'dims' when pop = '+/-'". The
## Reagent row's dims is FSC-A by ACTA convention (the real marker channel comes from the layout), so
## its nodes are FSC-A+/FSC-A-. Hence the names cannot be predicted from the alias, and this resolves
## by STRUCTURE instead: the children of the marker row's own parent.
## Split in two so the LOGIC is testable without a GatingSet: the resolution depends only on the
## population PATHS and the template, and building a real gs to exercise it would make the guard slow
## enough that nobody runs it.
## Which POPULATIONS a gating_template row will produce, by name.
##
## One row does not always mean one population, and the names are not always the alias. openCyto
## documents the expansion (doc/HowToWriteCSVTemplate.Rmd, "pop = +/-+/-"): a `+/-+/-` row with a 1D
## gating method is rewritten by openCyto into SIX rows -- two 1D gates on the individual dims, then
## four refGate quadrants -- and every one of them is named from the DIMS, not the alias:
##
##   alias      pop  parent  dims      gating_method  gating_args
##   CD4+       +    MarkerB-    CD4       gate_flowmeans
##   CD8+       +    MarkerB-    CD8       gate_flowmeans
##   CD4+CD8+   ++   MarkerB-    CD4, CD8  refGate        MarkerB-/CD4+:MarkerB-/CD8+
##   CD4-CD8+   -+   MarkerB-    CD4, CD8  refGate        MarkerB-/CD4+:MarkerB-/CD8+
##   CD4+CD8-   +-   MarkerB-    CD4, CD8  refGate        MarkerB-/CD4+:MarkerB-/CD8+
##   CD4-CD8-   --   MarkerB-    CD4, CD8  refGate        MarkerB-/CD4+:MarkerB-/CD8+
##
## Those are ordinary populations, so `CD4-CD8+` can be a PARENT -- which is the whole point, and
## the reason this is worth supporting rather than rejecting. (Contrast the `gate_quad_*` methods:
## they return a single quadGate, so openCyto auto-names their output from the CHANNELS
## -- "BUV496-A-Alexa Fluor 532-A+" -- and registers the row as `*`, which nothing can be parented
## on. That is openCyto's behaviour, not something ACTA can route around.)
##
## Returns character(0) when the names cannot be known ahead of gating (`pop = "*"`, where the count
## and order of gates is up to the gating function). Callers must treat that as "nothing to scaffold",
## not as an error -- the gating still happens.
##
## The order here matches openCyto's own expansion, and tests/test_quad_pops.R pins it against
## openCyto:::.preprocess_row() so a change upstream is caught rather than silently mis-plotted.
## A knitr chunk label for a population name.
##
## The report emits one child chunk per population and knitr requires the labels to be UNIQUE. The
## previous sanitiser was gsub("[^A-Za-z0-9]", "-", name), which throws away exactly the characters
## that distinguish quadrant populations: CD4+CD8+ and CD4-CD8- both became "CD4-CD8-", and the
## render died with "Duplicate chunk label 'lp1-CD4-CD8--1'". Measured on the same run the moment a
## `+/-+/-` row was used -- the plots were all built correctly and only the report failed.
##
## So the signs survive, as letters: + -> p, - -> n. CD4+CD8+ / CD4-CD8+ / CD4+CD8- / CD4-CD8- become
## CD4pCD8p / CD4nCD8p / CD4pCD8n / CD4nCD8n, still readable in a knitr error message. Anything else
## non-alphanumeric becomes "_". Callers must STILL de-duplicate: two different names can in principle
## sanitise alike (an "A+B" and a literal "ApB"), and a duplicate label is a failed render.
actaChunkTag <- function(x) {
  x <- gsub("[+]", "p", as.character(x))
  x <- gsub("-",   "n", x)
  gsub("[^A-Za-z0-9]", "_", x)
}

## `channels` is the row's dims RESOLVED to detector names. Supply it and the quadrant candidates
## cover both naming schemes openCyto uses, because which one applies depends on the METHOD:
##   * a 1D method  -> the refGate expansion, named from the dims as authored  (CD4-CD8+)
##   * a gate_quad_* method -> one quadGate, auto-named from the CHANNELS      (BUV496-A-Alexa Fluor 532-A+)
## Only one of the two sets will exist in the tree, so the caller filters; offering both is what lets
## a tmix quadrant be plotted instead of skipped with "no population named 'CD4-CD8+'".
## WHICH of a multi-population row's nodes actually get DRAWN on its one plot.
##
## A `+/-+/-` quadrant row expands to six populations: the four quadrants plus two 1D intermediates,
## `<dim>+` for each dim, whose cutpoints are already the edges of the four rectangles. Drawing those
## adds two redundant lines and two marginal percentages to an otherwise readable panel, so they are
## excluded -- they remain real populations in the tree and still feed StatsExport.
##
## THE EXCEPTION IS THE WHOLE POINT. A `pop = "+/-"` row's own pair is `<alias>+` / `<alias>-`, and
## when the alias equals a dim token -- alias CD3 with dims "CD3, FSC-A", which is how anybody writes
## it -- `paste0(dims, "+")` builds the string "CD3+" and the exclusion silently removes a population
## the user asked for. Reported 2026-08-28: such a row gated correctly, appeared correctly in the
## tree and in StatsExport, and plotted only CD3-. The row's own pair is therefore never excluded.
##
## Pulled out of the script so the rule is named and testable; it was inline in the layout scaffold.
actaDrawnNodes <- function(have, dims, alias) {
  tk  <- trimws(strsplit(paste0(as.character(dims), ""), ",")[[1]])
  tk  <- tk[nzchar(tk)]
  own <- paste0(trimws(as.character(alias)), c("+", "-"))
  d   <- setdiff(have, setdiff(paste0(tk, "+"), own))
  if (!length(d)) have else d
}

actaTemplateRowNodes <- function(alias, pop, dims, channels = NULL) {
  a <- trimws(as.character(alias))[1]
  p <- trimws(as.character(pop))[1]
  if (is.na(p) || !nzchar(p)) return(a)
  if (identical(p, "*")) return(character(0))
  if (identical(p, "+/-+/-")) {
    quad <- function(x1, x2) c(paste0(x1, "+", x2, "+"), paste0(x1, "-", x2, "+"),
                               paste0(x1, "+", x2, "-"), paste0(x1, "-", x2, "-"))
    tk <- trimws(strsplit(paste0(as.character(dims)[1], ""), ",")[[1]])
    tk <- tk[nzchar(tk) & !is.na(tk)]
    out <- character(0)
    ## The 1D intermediates come first, matching openCyto's own expansion order -- see the test.
    if (length(tk) >= 2)
      out <- c(paste0(tk[1], "+"), paste0(tk[2], "+"), quad(tk[1], tk[2]))
    ch <- trimws(as.character(channels)); ch <- ch[nzchar(ch) & !is.na(ch)]
    if (length(ch) >= 2) out <- c(out, quad(ch[1], ch[2]))
    return(unique(out))
  }
  ## A "+/-" row yields <alias>+ and <alias>- -- the marker row's shape, and equally true of any
  ## other row written that way.
  if (identical(p, "+/-")) return(paste0(a, c("+", "-")))
  ## "+", "-" and a single quadrant ("++", "+-", "-+", "--", the refGate form) are all named by the
  ## alias.
  a
}

## ---------------------------------------------------------------------------------------------
## MARKER NAMES FOR QUADRANT POPULATIONS.
##
## openCyto names a quadrant's four children from the GATE's dimensions. With a 1D gating method and
## pop = "+/-+/-" those dimensions are the dims tokens, so the nodes come out as "CD4+CD8-" and read
## the way a person would write them. With a gate_quad_* method -- one 2D quadGate instead of two 1D
## cutpoints -- the gate is built AFTER dims have been resolved to channels, so the same four
## populations come out as "BUV496-A+Alexa Fluor 532-A-": correct, and unreadable. Those names are
## what the layout plot, StatsExport and the dashboard then show.
##
## The fix is a RENAME, not a different gate: the quadrant stays a 2D t-mixture, which is the whole
## reason gate_quad_tmix was chosen over two independent cutpoints, and only the labels change.
## openCyto's expansion ORDER is fixed and pinned in tests/test_quad_pops.R, so the channel-named and
## dims-named sets zip together element by element -- no parsing of a composite name, and no guessing
## which of the four quadrants is which.
## ---------------------------------------------------------------------------------------------
## PARENT POPULATION LABELS, PER ANTIBODY GROUP.
##
## `parentPop` is per group -- it is the marker row's parent, and different groups can legitimately
## gate under different populations. The RUN-LEVEL figures (SIPlot, SatPlot, ConcatPlot) are one
## figure across every group, so they cannot name one parent unless every group agrees; they used to
## read whatever the group loop happened to leave behind, which meant a three-group run labelled
## everything with the LAST group's parent, in subtitles AND in `_parent_<pop>_` filenames.
##
## Given the per-group values, this returns what is actually true: the shared parent when they agree,
## and an explicit per-group list when they do not. `for_file = TRUE` returns a short token instead,
## because a filename is not the place to spell out three groups.
actaParentLabel <- function(byGroup, for_file = FALSE) {
  v <- byGroup[!is.na(byGroup) & nzchar(as.character(byGroup))]
  if (!length(v)) return(if (for_file) "NA" else "not recorded")
  u <- unique(as.character(v))
  if (length(u) == 1L) return(u[[1]])
  if (isTRUE(for_file)) return("mixed")
  ## Named so a reader can tell WHICH group sits under which parent -- "mixed" alone would raise the
  ## question without answering it.
  paste0("by group -- ", paste(sprintf("%s: %s", names(v), as.character(v)), collapse = "; "))
}

## Run- or group-level label for the ELN (Benchling) IDs. `unique(metadata_file$ELN_ID)` assumed ONE
## entry per run. A layout that legitimately draws its wells from TWO notebook entries -- reported by
## the user on 2026-08-26, where one FCS folder came from a different entry than the other three --
## made `BID` a length-2 VECTOR, and each consumer then failed in its own way:
##   * the export died at the very END of the run, inside openxlsx::saveWorkbook(), as
##     `file.exists(file) && !overwrite` -> "'length = 2' in coercion to logical(1)";
##   * the run-level PNGs were written from element [1] ONLY, so the wells belonging to the other
##     entry were silently misattributed in the filename -- the worse half, since it looks fine;
##   * three plot titles (`paste0("[Saturation] ", unique(stats$ELN_ID))`) took element [1] too.
##
## Deliberately the same shape and defaults as actaParentLabel(): both answer "one label for
## something that is per group", both have a filename variant, and they are called side by side.
## for_file joins with "+" instead of ", " and strips anything outside [A-Za-z0-9._+-]: a comma and a
## space survive every filesystem here but make the name awkward to read and to quote in a shell.
## A single ID -- the ordinary case, and every OQ case -- comes back UNCHANGED either way.
actaElnLabel <- function(x, for_file = FALSE) {
  v <- unique(trimws(as.character(x[!is.na(x)])))
  v <- v[nzchar(v)]
  if (!length(v)) return(if (isTRUE(for_file)) "NA" else "not recorded")
  if (isTRUE(for_file)) return(paste(gsub("[^A-Za-z0-9._+-]", "_", v), collapse = "+"))
  paste(v, collapse = ", ")
}

actaQuadNodeRenames <- function(gating_template, boundary) {
  out <- data.frame(row = integer(0), alias = character(0), from = character(0), to = character(0),
                    stringsAsFactors = FALSE)
  if (is.null(gating_template) || !nrow(gating_template)) return(out)
  if (!all(c("alias", "pop", "dims") %in% names(gating_template))) return(out)
  live <- .actaTemplateLive(gating_template$alias)
  quad <- function(a, b) c(paste0(a, "+", b, "+"), paste0(a, "-", b, "+"),
                           paste0(a, "+", b, "-"), paste0(a, "-", b, "-"))
  for (i in seq_len(nrow(gating_template))) {
    if (!isTRUE(live[i])) next
    if (!identical(trimws(as.character(gating_template$pop[i])), "+/-+/-")) next
    tk <- trimws(strsplit(paste0(as.character(gating_template$dims[i]), ""), ",")[[1]])
    tk <- tk[nzchar(tk) & !is.na(tk)]
    if (length(tk) < 2) next
    ch <- resolveDimsToChannels(as.character(gating_template$dims[i]), boundary)
    ## dims written AS channels resolve to themselves; there is nothing to rename, and emitting the
    ## identity pair would read like a failure in the run log.
    if (length(ch) < 2 || identical(toupper(ch[1:2]), toupper(tk[1:2]))) next
    out <- rbind(out, data.frame(row = i, alias = trimws(as.character(gating_template$alias[i])),
                                 from = quad(ch[1], ch[2]), to = quad(tk[1], tk[2]),
                                 stringsAsFactors = FALSE))
  }
  out
}

## Applies that map to a GatingSet, IN PLACE (a GatingSet is a reference object, so there is nothing
## to assign back). Returns only the renames actually made, so the caller can report them once
## instead of once per node.
actaRenameQuadNodes <- function(gs, gating_template, boundary, group = "") {
  map  <- actaQuadNodeRenames(gating_template, boundary)
  done <- map[0, , drop = FALSE]
  if (!nrow(map)) return(done)
  for (k in seq_len(nrow(map))) {
    paths <- flowWorkspace::gs_get_pop_paths(gs)
    hit   <- paths[basename(paths) == map$from[k]]
    ## A 1D method already produced the dims-named nodes, so `from` is simply absent. That is the
    ## common case, not a problem, and it is why this is silent.
    if (length(hit) != 1L) next
    ## Never overwrite an existing population: two rows could in principle quadrant the same pair of
    ## markers, and a duplicate node name would corrupt every lookup keyed on names.
    if (map$to[k] %in% basename(paths)) {
      message(sprintf(paste0("[ACTA] a population named '%s' already exists%s -- leaving the ",
                             "quadrant node '%s' named after its channel."), map$to[k],
                      if (nzchar(group)) sprintf(" for '%s'", group) else "", map$from[k]))
      next
    }
    flowWorkspace::gs_pop_set_name(gs, hit, map$to[k])
    done <- rbind(done, map[k, , drop = FALSE])
  }
  done
}

actaMarkerNodes <- function(gs, gating_template, group = "") {
  actaMarkerNodesFromPaths(flowWorkspace::gs_get_pop_paths(gs), gating_template, group)
}

actaMarkerNodesFromPaths <- function(paths, gating_template, group = "") {
  nm <- basename(paths)
  mk <- which(tolower(trimws(as.character(gating_template$alias))) %in% c("reagent", "antibody"))
  if (length(mk) != 1)
    stop(sprintf(paste0("gating_template has %d marker rows (alias 'Reagent'/'Antibody')%s; ",
                        "exactly one is required."), length(mk),
                 if (nzchar(group)) sprintf(" for '%s'", group) else ""), call. = FALSE)

  parentVal <- trimws(as.character(gating_template$parent[mk]))
  parentPath <- if (identical(tolower(parentVal), "root")) "root" else {
    hit <- paths[nm == parentVal]
    if (length(hit) != 1)
      stop(sprintf(paste0("The marker row's parent '%s' matches %d populations%s (expected exactly ",
                          "one). Populations present: %s"), parentVal, length(hit),
                   if (nzchar(group)) sprintf(" for '%s'", group) else "",
                   paste(nm, collapse = ", ")), call. = FALSE)
    hit
  }
  ## Children of the marker row's parent. dirname("/Cells") is "/", so root needs the special case.
  want <- if (identical(parentPath, "root")) "/" else parentPath
  kid  <- which(dirname(paths) == want)
  kidNm <- nm[kid]

  ## NAME the pair rather than pattern-match the siblings. A "+"/"-" suffix scan among the children
  ## is only unambiguous while the marker row is the parent's ONLY "+/-" row -- and it is not: a
  ## quadrant row sharing the same parent contributes four more suffixed children, so
  ## Live/{Reagent+, Reagent-, BUV496-A+Alexa Fluor 532-A+, ...} gave "found 3 '+' and 3 '-'" and the
  ## run stopped. Measured 2026-08-19 on an internal run with a gate_quad_tmix row under Live.
  ##
  ## Candidates, most specific first:
  ##   1. <alias>+ / <alias>-      what openCyto uses when it honours the alias (Reagent+/Reagent-)
  ##   2. <first dims token>+/-    what it uses when it auto-generates from dims instead, which it
  ##                               announces as "alias 'Reagent' is ignored since pop names are
  ##                               auto-generated from 'dims'" (FSC-A+/FSC-A- on ACTA's marker row)
  ##   3. the suffix scan, unchanged, as a last resort for a naming scheme neither rule predicts --
  ##      still requiring exactly one of each, because at that point a guess is all it would be.
  aliasVal <- trimws(as.character(gating_template$alias[mk]))
  dim1 <- trimws(strsplit(paste0(as.character(gating_template$dims[mk]), ""), ",")[[1]])[1]
  cand <- list(paste0(aliasVal, c("+", "-")))
  if (!is.na(dim1) && nzchar(dim1)) cand[[length(cand) + 1L]] <- paste0(dim1, c("+", "-"))
  hit <- NULL
  for (cc in cand) if (all(cc %in% kidNm)) { hit <- cc; break }

  if (!is.null(hit)) {
    pos <- kid[match(hit[[1]], kidNm)]
    neg <- kid[match(hit[[2]], kidNm)]
  } else {
    pos <- kid[grepl("[+]$", kidNm)]
    neg <- kid[grepl("[-]$", kidNm)]
  }
  if (length(pos) != 1 || length(neg) != 1)
    stop(sprintf(paste0("Could not identify the marker +/- populations under '%s'%s. Looked for %s, ",
                        "then %s, then a single '+'/'-' pair among the children -- found %d '+' and ",
                        "%d '-' (%s). The gating_template marker row must have pop = '+/-'."),
                 parentVal, if (nzchar(group)) sprintf(" for '%s'", group) else "",
                 paste(sprintf("'%s'", cand[[1]]), collapse = "/"),
                 if (length(cand) > 1) paste(sprintf("'%s'", cand[[2]]), collapse = "/") else "(no dims)",
                 length(pos), length(neg),
                 if (length(kid)) paste(kidNm, collapse = ", ") else "none"), call. = FALSE)
  list(pos = nm[pos], neg = nm[neg], pos_path = paths[pos], neg_path = paths[neg],
       pos_index = pos, neg_index = neg, parent_path = parentPath)
}

addBiexpAxes <- function(p, xch = NULL, ych = NULL, trans = NULL, maxValue = NULL,
                         xlim = NULL, ylim = NULL,
                         x_expand = ggplot2::waiver(), y_expand = ggplot2::waiver()) {
  if (is.null(trans) || is.null(maxValue)) return(p)
  if (!is.null(xch) && length(xch) == 1 && isFluorChannel(xch))
    p <- p + biexpScale(trans, maxValue, "x", limits = xlim, expand = x_expand)
  if (!is.null(ych) && length(ych) == 1 && isFluorChannel(ych))
    p <- p + biexpScale(trans, maxValue, "y", limits = ylim, expand = y_expand)
  p
}


annotate_changeParam_pdWrite<-function(fs, metadata){
  list_of_frames <- list()
  newPData<-data.frame()
  for(i in seq_along(fs)){
    fcs<-fs[[i]]
    old<-fcs@description
    wellrow<-substr(fcs@description$`$WELLID`,1,1)
    wellcol<-substr(fcs@description$`$WELLID`,2,3)
    plate<-which_plate(fcs)
    new<-metadata %>% 
      ## Row compared case-insensitively: $WELLID reports an upper-case letter, so a layout
      ## typed `a` matched nothing, the well silently got no layout row, and the !is.na(Dirname)
      ## guards downstream quietly dropped it from the titration.
      dplyr::filter(`PlateID`==plate & isLayoutValue(`Row`, wellrow) & `Column`==wellcol) %>% 
      append(old)
    new_ff<-new(Class="flowFrame", exprs=fcs@exprs, parameters=fcs@parameters, description=new)
    eln <- new_ff@description$ELN_ID
    pid <- new_ff@description$`PlateID`
    wid <- new_ff@description$`$WELLID`
    ## `Markername` as of 2_85, `Dim` before it -- whichever header the layout carries ends up as
    ## a description field here, so both are tried. NA rather than NULL if neither: sprintf() on a
    ## zero-length value yields character(0), and the list assignment below then fails with the
    ## opaque "attempt to select less than one element".
    tidCol <- actaLayoutCol(new_ff@description, ACTA_MARKER_COLS)
    tid <- if (!is.na(tidCol)) new_ff@description[[tidCol]] else NULL
    if (is.null(tid) || !length(tid)) tid <- NA
    sptid <- new_ff@description$`StainType`
    sv <- new_ff@description$StainQty
    oid <- new_ff@description$`Operator`
    ##temp<-sprintf("%s_P%s_%s_tgt%s_%s_%s_Op%s.fcs",eln, pid, wid, tid, sptid, sv, oid)
    new_ff@description$FILENAME<-sprintf("%s_P%s_%s_tgt%s_%s_%s_Op%s.fcs",eln, pid, wid, tid, sptid, sv, oid)
    list_of_frames[[new_ff@description$FILENAME]]<-new_ff
    newPData<-metadata %>%
      dplyr::filter(`PlateID`==plate & isLayoutValue(`Row`, wellrow) & `Column`==wellcol) %>%
      mutate(newFileName=new_ff@description$FILENAME,
             ## Per-well provenance off THIS file's keywords; travels via pData into stats
             ## and out to the export's Acquisition_date / Equipment columns.
             Acquisition_date=fcsAcquisitionDate(fcs),
             Equipment=fcsEquipment(fcs)) %>%
      rbind(newPData)
  }
  
  fsfinal<-as(list_of_frames, "flowSet")
  merged<-merge(pData(fsfinal), newPData, by.x="name", by.y="newFileName")
  rownames(merged)<-merged$name
  pData(fsfinal)<-merged
  return(fsfinal)
}

## Plain-text FCS keyword for ONE flowFrame, trimmed to a length-1 character. Per-file for
## the same reason as fcsAcquisitionDate(): a titration spread across two instruments stays
## visible well-by-well instead of being collapsed to one experiment-level value. Building
## block for fcsEquipment() below; `$INST` is available the same way if ever wanted.
## Missing/blank -> NA_character_ with a warning, never a silent "".
fcsKeyword <- function(ff, keyword) {
  id <- tryCatch(flowCore::identifier(ff), error = function(e) "<unknown>")
  v  <- ff@description[[keyword]]
  if (is.null(v) || length(v) < 1 || is.na(v[1]) || !nzchar(trimws(as.character(v)[1]))) {
    warning(sprintf("fcsKeyword: no %s keyword in '%s'; value is NA.", keyword, id))
    return(NA_character_)
  }
  trimws(as.character(v)[1])
}

## Instrument label for the export's Equipment column, formatted "$CYT ($CYTSN)" --
## e.g. "Aurora (SN1234)". Both keywords are used so the model AND the individual machine are
## recorded: a lab with two Auroras needs the serial to tell runs apart, and the model alone
## is not traceable. Degrades rather than failing: model only -> "Aurora"; serial only ->
## "(SN1234)"; neither -> NA with a warning.
## The per-keyword warnings from fcsKeyword() are suppressed here on purpose -- a missing
## $CYTSN is a benign degradation, and warning once per FCS file would drown the run log.
## Only the both-absent case (where Equipment carries no information at all) warns.
fcsEquipment <- function(ff) {
  cyt <- suppressWarnings(fcsKeyword(ff, "$CYT"))
  sn  <- suppressWarnings(fcsKeyword(ff, "$CYTSN"))
  if (is.na(cyt) && is.na(sn)) {
    warning(sprintf("fcsEquipment: neither $CYT nor $CYTSN in '%s'; Equipment is NA.",
                    tryCatch(flowCore::identifier(ff), error = function(e) "<unknown>")))
    return(NA_character_)
  }
  if (is.na(sn))  return(cyt)
  if (is.na(cyt)) return(sprintf("(%s)", sn))
  sprintf("%s (%s)", cyt, sn)
}

## Acquisition date of ONE flowFrame, read from its FCS `$DATE` keyword. Deliberately
## per-file: every well carries the date it was actually acquired on, so a titration split
## across days is visible in the export. This is NOT the render date -- the `lubridate::today()`
## in the export FILENAME is a separate thing and stays as it is.
## Formats: SpectroFlo/BD write dd-Mon-yyyy ("20-May-2026"); the month name is resolved from
## a lookup rather than `%b`, which follows LC_TIME and would silently fail in a non-English
## locale. ISO and slash-separated forms are then tried. NOTE the m/d vs d/m ambiguity: a
## slash date is read US-first (%m/%d/%Y), which is what US instruments emit -- if a cytometer
## ever writes d/m/Y, reorder that vector. Unparseable/absent -> NA plus a warning, never a
## silent guess.
fcsAcquisitionDate <- function(ff) {
  id  <- tryCatch(flowCore::identifier(ff), error = function(e) "<unknown>")
  raw <- ff@description[["$DATE"]]
  if (is.null(raw) || !nzchar(trimws(as.character(raw)[1]))) {
    warning(sprintf("fcsAcquisitionDate: no $DATE keyword in '%s'; Acquisition_date is NA.", id))
    return(as.Date(NA))
  }
  d <- trimws(as.character(raw)[1])
  MON <- c(JAN=1,FEB=2,MAR=3,APR=4,MAY=5,JUN=6,JUL=7,AUG=8,SEP=9,OCT=10,NOV=11,DEC=12)
  m <- regmatches(d, regexec("^([0-9]{1,2})[-/ ]([A-Za-z]{3,9})[-/ ]([0-9]{2,4})$", d))[[1]]
  if (length(m) == 4) {
    mm <- MON[toupper(substr(m[3], 1, 3))]
    if (!is.na(mm)) {
      yy <- as.integer(m[4]); if (yy < 100) yy <- yy + 2000L
      return(as.Date(sprintf("%04d-%02d-%02d", yy, mm, as.integer(m[2]))))
    }
  }
  for (fmt in c("%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y", "%d/%m/%Y", "%Y%m%d")) {
    out <- as.Date(d, format = fmt)
    if (!is.na(out)) return(out)
  }
  warning(sprintf("fcsAcquisitionDate: could not parse $DATE '%s' in '%s'; Acquisition_date is NA.", d, id))
  as.Date(NA)
}

## Locate one antibody group's FCS under Titration_FCS, returning paths RELATIVE to that root.
##
## Two supported layouts, deliberately both:
##   Titration_FCS/<antibody>/*.fcs            <- flat (2_85 onward; PLATE_* is gone from the sheet)
##   Titration_FCS/PLATE_1/<antibody>/*.fcs    <- legacy, still read so existing folders keep working
##
## Any OTHER PLATE_* folder is ignored, exactly as before: the layout is a single `Layout_Plate`
## sheet, so there are no plate-2 metadata rows for those wells to match, and reading them would
## silently double the series. actaPlateExtras() names them for the caller to warn about.
##
## Shared by the loader and actaPrevalidate() on purpose. These were separate copies of the same
## prefix match, which is how a pre-flight check passes and the loader then finds nothing.
## ---------------------------------------------------------------------------------------------
## THE UNIT OF ANALYSIS: ONE TITRATED MARKER, NOT ONE FCS FOLDER.
##
## `Dirname` has always meant two things at once -- WHERE the events live (the Titration_FCS
## subfolder) and WHICH titration series this is. Combinatorial titration separates them: several
## markers can be titrated in the same wells, so one Dirname carries several series, each with its
## own marker, channel, reagent metadata and stain quantities.
##
## The layout expresses that in LONG form -- one row per well per titrated marker, same Dirname,
## different Markername / Channel / Alias / StainQty. This function turns those rows into the loop's
## unit of work.
##
## THE KEY IS `Dirname` WHENEVER IT IS UNAMBIGUOUS, which is every workbook written before today.
## That is deliberate and not just conservatism: the key is what names plot files, keys `qcList`, and
## labels the dashboard, and `Alias` is NOT interchangeable with `Dirname` even now -- OQ_Test1 is
## Dirname "Strep_PE" with Alias "Strep". Keying on Alias unconditionally would rename that case's
## artefacts and move a baseline for no reason. So: one Alias in a Dirname -> the key is the Dirname,
## byte-identical to today. Several -> the key is the Alias, because that is what distinguishes them.
##
## Returns a data.frame of dirname / alias / key, one row per titration, with attribute
## "combinatorial" set when any Dirname carries more than one.
actaTitrationGroups <- function(metadata, alias_col = "Alias", channel_col = NULL) {
  if (is.null(metadata) || !nrow(metadata) || !("Dirname" %in% names(metadata)))
    return(structure(data.frame(dirname = character(0), alias = character(0), key = character(0),
                                stringsAsFactors = FALSE), combinatorial = FALSE))
  d <- trimws(as.character(metadata$Dirname))
  keep <- !is.na(d) & nzchar(d)
  ## A legacy sheet may have no Alias column at all; and a blank Alias on an otherwise good row is a
  ## partially filled workbook, not an error. Both fall back to the Dirname, which is what the
  ## pipeline did before Alias existed.
  a <- if (!is.na(alias_col) && alias_col %in% names(metadata))
         trimws(as.character(metadata[[alias_col]])) else rep(NA_character_, nrow(metadata))
  a[is.na(a) | !nzchar(a)] <- NA_character_
  a <- ifelse(is.na(a), d, a)

  ## FIRST-APPEARANCE ORDER, not sorted. `unique(metadata_file$Dirname)` used to set this order and
  ## it is load-bearing: it decides the report's section order, the row order of the exports, and
  ## which group's representative FCS supplies the MiFlowCyt $PnV table. Sorting alphabetically
  ## reordered OQ_Test2's groups from CD19/CD7/CD56 to CD19/CD56/CD7, which the OQ caught. A user
  ## orders the sheet deliberately; honour it.
  g <- unique(data.frame(dirname = d[keep], alias = a[keep], stringsAsFactors = FALSE))
  n <- table(g$dirname)
  ## as.character() is not decoration: n[g$dirname] subsets a table, so it carries a `dim`, and
  ## ifelse() copies its test's attributes onto the result -- the key came back as a 1-D ARRAY whose
  ## values print identically to a character vector. It passed an eyeball check and failed
  ## identical() in the tests, which is where a stray dim would have surfaced downstream instead.
  g$key <- as.character(ifelse(n[g$dirname] > 1L, g$alias, g$dirname))
  rownames(g) <- NULL

  ## A key that repeats would silently merge two series -- their wells would land in one `stats`
  ## block, one QC entry and one set of plot filenames. Refuse it and say which.
  if (anyDuplicated(g$key)) {
    dup <- unique(g$key[duplicated(g$key)])
    stop(sprintf(paste0("Layout_Plate: the titration key is not unique (%s). A Dirname with several ",
                        "Alias values is keyed by Alias, so those Alias values must not collide with ",
                        "each other or with another Dirname's name. Rename one."),
                 paste(sprintf("'%s'", dup), collapse = ", ")), call. = FALSE)
  }

  ## The key names FILES -- Plots/<key>_<ELN>_parent_<pop>_histogram.png and friends. A Dirname was
  ## safe by construction, being a real folder name; an Alias is free text, so a "/" or ":" in it
  ## would produce a path rather than a filename and the save would fail well after the analysis.
  ## Leading/trailing space is NOT checked here: the alias is trimws()'d above, so that condition
  ## could never fire, and a check that cannot fail reads as protection that is not there.
  bad <- grepl('[/\\\\:*?"<>|]', g$key)
  if (any(bad))
    stop(sprintf(paste0("Layout_Plate: titration key(s) %s cannot be used in a filename. The key is ",
                        "the Alias when a Dirname carries several markers, and it names the plot ",
                        "files -- so avoid / \\ : * ? \" < > | and leading or trailing spaces."),
                 paste(sprintf("'%s'", g$key[bad]), collapse = ", ")), call. = FALSE)

  ## One channel per titration, and no two titrations sharing a channel inside one Dirname. Both
  ## mistakes are copy-paste artefacts of the duplicated rows, and both are silent: the first makes
  ## the marker gate ambiguous, the second measures the same detector twice and reports it as two
  ## independent series.
  if (!is.null(channel_col) && channel_col %in% names(metadata)) {
    ch <- trimws(as.character(metadata[[channel_col]]))
    for (i in seq_len(nrow(g))) {
      sel <- keep & d == g$dirname[i] & a == g$alias[i]
      u <- unique(ch[sel & !is.na(ch) & nzchar(ch)])
      if (length(u) > 1)
        stop(sprintf("Layout_Plate: titration '%s' (Dirname '%s') names %d channels (%s); expected one.",
                     g$key[i], g$dirname[i], length(u), paste(u, collapse = ", ")), call. = FALSE)
    }
    for (dn in unique(g$dirname)) {
      sub <- g[g$dirname == dn, , drop = FALSE]
      if (nrow(sub) < 2) next
      chs <- vapply(seq_len(nrow(sub)), function(i) {
        u <- unique(ch[keep & d == dn & a == sub$alias[i] & !is.na(ch) & nzchar(ch)])
        if (length(u) == 1) u else NA_character_ }, "")
      if (anyDuplicated(chs[!is.na(chs)]))
        stop(sprintf(paste0("Layout_Plate: Dirname '%s' has two titrations on the same channel (%s). ",
                            "Co-titrated markers must be on different detectors."), dn,
                     paste(chs[duplicated(chs)], collapse = ", ")), call. = FALSE)
    }
  }
  structure(g, combinatorial = any(n > 1L))
}

## Which layout rows belong to one titration. Every per-titration lookup goes through this, so the
## Alias fallback (blank Alias -> the Dirname) is defined once here rather than restated at each call
## site -- three of which used to filter on Dirname alone and would have silently taken the first
## marker's value once a Dirname carried two.
## Accept either the actaTitrationGroups() frame or a bare character vector of Dirnames. The vector
## form is what every caller passed before combinatorial titration existed, and treating it as
## "one titration per folder, keyed by the folder" is exactly what it used to mean -- so an
## un-migrated caller keeps working instead of silently validating the wrong thing.
.actaAsGroups <- function(x) {
  if (is.data.frame(x) && all(c("dirname", "alias", "key") %in% names(x))) return(x)
  ## alias = NA means "the WHOLE folder, whatever its Alias". Setting alias = dirname here looked
  ## harmless and was not: actaTitrationRowMask() would then require the sheet's Alias to EQUAL the
  ## Dirname, which is false in real workbooks -- OQ_Test1 is Dirname "Strep_PE", Alias "Strep" -- so
  ## an un-migrated caller matched zero rows and produced the very "0 Costain wells" it was meant to
  ## avoid. Verified against that workbook: 0 rows matched, 12 expected.
  v <- unique(as.character(x)); v <- v[!is.na(v) & nzchar(v)]
  data.frame(dirname = v, alias = NA_character_, key = v, stringsAsFactors = FALSE)
}

actaTitrationRowMask <- function(metadata, dirname, alias, alias_col = "Alias") {
  if (is.null(metadata) || !nrow(metadata) || !("Dirname" %in% names(metadata)))
    return(logical(0))
  d <- trimws(as.character(metadata$Dirname))
  a <- if (!is.na(alias_col) && alias_col %in% names(metadata))
         trimws(as.character(metadata[[alias_col]])) else rep(NA_character_, nrow(metadata))
  a[is.na(a) | !nzchar(a)] <- NA_character_
  a <- ifelse(is.na(a), d, a)
  ## An NA `alias` asks for the whole folder -- every titration in it -- which is what a caller that
  ## still passes a bare vector of Dirnames means, and what the pipeline did before combinatorial
  ## titration existed.
  if (is.na(alias)) return(!is.na(d) & nzchar(d) & d == dirname)
  !is.na(d) & nzchar(d) & d == dirname & a == alias
}

actaFindAntibodyFcs <- function(root, ab, drop_unstained = TRUE) {
  if (!dir.exists(root)) return(character(0))
  rel   <- list.files(root, pattern = "\\.fcs$", recursive = TRUE, full.names = FALSE)
  if (!length(rel)) return(character(0))
  first <- sub("/.*$", "", rel)
  isPl  <- grepl("^PLATE_", first, ignore.case = TRUE)
  ## legacy tree: keep PLATE_1 only. flat tree: keep everything.
  usable <- !isPl | toupper(first) == "PLATE_1"
  ## Compare against the path with any leading PLATE_1/ stripped, so one rule covers both layouts.
  relAb <- ifelse(isPl, sub("^[^/]+/", "", rel), rel)
  sel   <- usable & startsWith(toupper(relAb), toupper(paste0(ab, "/")))
  out   <- rel[sel]
  if (drop_unstained) out <- out[!grepl("Unstain", out, ignore.case = TRUE)]
  as.character(out)
}

## Find the instructions workbook(s) in a folder. Returns every match -- 0, 1 or several -- and
## leaves "exactly one" to the caller, because the two callers want different messages: the script
## names the 2_87 Titration_Layout rename, the OQ runner names the case.
##
## THIS EXISTS BECAUSE THE PATTERN WAS WRITTEN TWICE AND THE TWO COPIES DRIFTED. The OQ runner
## anchored the extension (`[.]xlsx$`); the script did not (`.*.xlsx`), where `.` is any character
## and nothing is anchored -- so `<name>.xlsx.orig` counted as a second workbook and the run died
## with "found 2" naming a file that is not a workbook. One definition, two callers.
##
## TWO FILENAMES ARE EXCLUDED BY NAME, and they are the reason this is more than an anchoring fix:
##   * `~$<name>.xlsx` -- Excel writes this lock file WHILE THE WORKBOOK IS OPEN. It ends in .xlsx,
##     so an anchored pattern still matches it. Filling the sheet in, leaving it open in Excel and
##     pressing Run would fail on a hidden file the analyst cannot see and did not create.
##   * `._<name>.xlsx` -- the AppleDouble resource fork macOS writes beside a file on OneDrive/SMB,
##     which is exactly where this project lives.
## Excluded by NAME rather than by a hidden-file attribute: the attribute test differs per platform,
## and these two spellings are the entire problem.
actaFindInstructions <- function(dir = getwd(), full.names = FALSE) {
  f <- list.files(dir, pattern = "[.]xlsx$")
  f <- f[!grepl("^([~][$]|[.]_)", f)]
  f <- grep("Titration_Instructions", f, value = TRUE)
  if (isTRUE(full.names) && length(f)) file.path(dir, f) else f
}

## PLATE_* folders under `root` that are NOT read (i.e. anything but PLATE_1).
actaPlateExtras <- function(root) {
  if (!dir.exists(root)) return(character(0))
  d <- list.dirs(root, recursive = FALSE, full.names = FALSE)
  d <- d[grepl("^PLATE_", d, ignore.case = TRUE)]
  d[toupper(d) != "PLATE_1"]
}

which_plate<-function(fcs, default = 1L)
{
  path<-fcs@description$FILENAME
  ## `default` (1) rather than 0 when the path carries no PLATE_n token. From 2_85 the FCS may sit
  ## directly under Titration_FCS, and the layout's PlateID is matched against this value -- so
  ## returning 0 for a flat tree would match no layout row, and every well would be silently
  ## dropped by the !is.na(Dirname) guards downstream. A single-plate flat run IS plate 1.
  r<-case_when(grepl("PLATE_1", toupper(path)) ~ 1,
               grepl("PLATE_2", toupper(path)) ~ 2,
               grepl("PLATE_3", toupper(path)) ~ 3,
               TRUE ~ as.numeric(default))
  return(r)
}


## ------------------------------------------------------------------------------------------------
## PLATE MAP. Turn Layout_Plate back into the thing the analyst was looking at while filling it in:
## a plate, rows down the side, columns across the top, the dose in the well.
##
## WHY IT EARNS A SECTION OF ITS OWN. Every other table in the report is keyed on the titration, and
## none of them can show a TRANSCRIPTION error -- a dose typed into the wrong well is a perfectly
## valid row, and stays valid all the way through to the export. On a plate map it is visible at a
## glance, because the gradient breaks or a reagent turns up in a well it does not belong in. This is
## the only place the sheet is displayed in the geometry the sheet is describing.
##
## BUILT ON ggplate (MIT, so it composes with this project's AGPL-3 without friction), which draws
## the plate frame and resolves well IDs to positions. It exposes ONE data aesthetic, so the three
## carried here -- fill = Dirname+Alias, alpha = StainQty, ring colour = StainType -- are beyond its
## API; what makes it work anyway is that it keeps the whole input frame in `p$data`, adds `col` and
## `row_num` position columns, and draws the wells as GeomPoint shape 21, which has both a fill and
## a ring.
##
## THE RING IS A SEPARATE LAYER, and it has to be. ggplot2's `alpha` on a point dims the ring as
## well as the fill, so mapping both on one layer faded the StainType ring out exactly where the
## dose was lowest -- i.e. on the controls, the wells the ring exists to identify. ggplate does have
## a native `border`/`border_colour` pair and it is unusable here for the same reason: it maps onto
## the same layer as the fill.
##
## THIS REPLACED A HAND-BUILT LaTeX TABLE. That version worked, but colortbl paints a cell colour
## panel over the `|` rules beside it, so every attempt to shade the plate erased the grid inside
## the shaded block; the workaround (`!{\color{}\vrule}` separators, no \multicolumn anywhere) was
## carrying more LaTeX knowledge than a figure should. A ggplot also gives the report and the
## Plots/ export the same object, rather than a table for one and nothing for the other.
## ------------------------------------------------------------------------------------------------

## Standard plate geometries, and the sizes ggplate will accept.
ACTA_PLATE_GEOMETRY <- list(`6` = c(2, 3), `12` = c(3, 4), `24` = c(4, 6), `48` = c(6, 8),
                            `96` = c(8, 12), `384` = c(16, 24), `1536` = c(32, 48))

## Row letters are BIJECTIVE BASE-26, not a single character: 96 and 384 stop at H and P, but a
## 1536-well plate runs A..Z then AA..AF, so "AA" is row 27 and a `match(x, LETTERS)` would give NA
## and silently drop the bottom fifth of the plate.
actaRowIndex <- function(x) {
  x <- toupper(trimws(as.character(x)))
  vapply(x, function(s) {
    if (is.na(s) || !nzchar(s) || grepl("[^A-Z]", s)) return(NA_integer_)
    ch <- utf8ToInt(s) - utf8ToInt("A") + 1L
    as.integer(sum(ch * 26L^rev(seq_along(ch) - 1L)))
  }, integer(1), USE.NAMES = FALSE)
}

## "A01", "H12", "P24", "AF48" -> row index and column number. Vendors differ on zero-padding and
## on case; neither changes the answer.
actaParseWellId <- function(x) {
  x <- toupper(trimws(as.character(x)))
  m <- regmatches(x, regexec("^([A-Z]+)([0-9]+)$", x))
  rc <- lapply(m, function(p) if (length(p) == 3L) c(actaRowIndex(p[2]), as.integer(p[3]))
                              else c(NA_integer_, NA_integer_))
  list(row = vapply(rc, `[`, integer(1), 1L), col = vapply(rc, `[`, integer(1), 2L))
}

## Which plate the run was laid out on.
##
## THE LAYOUT SHEET IS THE AUTHORITY, NOT $WELLID, and the reason is directional: the sheet
## describes the whole plate the analyst laid out, whereas $WELLID only reports the wells that were
## ACQUIRED. A 96-well plate with one row run would look like a 12-well plate from the FCS alone.
## $WELLID is still read, because it can only ever ADD wells the sheet failed to mention -- so the
## union of the two is never smaller than either.
##
## NEVER SMALLER THAN THE FLOOR, GROW ONLY WHEN THE DATA DEMANDS IT. "Smallest plate that fits" was
## the first rule and it is wrong, which OQ_Test3 proved: its wells reach only row E and column 8,
## so it was detected as a 48-well plate when it is physically a 96 with most of it unused. A
## PARTIALLY FILLED PLATE IS THE NORMAL CASE -- a titration is one or two rows of a 96 -- so
## shrinking to fit makes the common case render wrong, and wrong QUIETLY, as a plausible 6 x 8
## grid. Growing is safe in a way shrinking is not: too small is a LOUD failure, because ggplate
## rejects a position outside the plate rather than dropping it.
actaPlateSize <- function(metadata = NULL, wellid = NULL, declared = NULL, floor_size = 96L) {
  sizes <- as.integer(names(ACTA_PLATE_GEOMETRY))
  if (!is.null(declared) && length(declared) && !is.na(declared[1]) &&
      nzchar(trimws(as.character(declared[1])))) {
    dv <- suppressWarnings(as.integer(trimws(as.character(declared[1]))))
    if (is.na(dv) || !(dv %in% sizes))
      stop(sprintf("Info!plate_size is '%s'; ggplate supports only %s.",
                   declared[1], paste(sizes, collapse = ", ")), call. = FALSE)
    return(list(size = dv, rows = ACTA_PLATE_GEOMETRY[[as.character(dv)]][1],
                cols = ACTA_PLATE_GEOMETRY[[as.character(dv)]][2],
                source = "declared in Info", max_row = NA_integer_, max_col = NA_integer_))
  }
  mr <- mc <- 0L
  if (!is.null(metadata) && is.data.frame(metadata) && nrow(metadata)) {
    rn <- actaLayoutCol(metadata, c("Row"))
    cn <- actaLayoutCol(metadata, c("Column", "Col"))
    if (!is.na(rn)) mr <- max(c(0L, actaRowIndex(metadata[[rn]])), na.rm = TRUE)
    if (!is.na(cn)) mc <- max(c(0L, suppressWarnings(as.integer(trimws(as.character(metadata[[cn]]))))),
                              na.rm = TRUE)
  }
  if (!is.null(wellid) && length(wellid)) {
    w <- actaParseWellId(wellid)
    mr <- max(c(mr, w$row), na.rm = TRUE); mc <- max(c(mc, w$col), na.rm = TRUE)
  }
  if (!is.finite(mr) || !is.finite(mc) || mr < 1L || mc < 1L)
    stop("Could not read any well position from the layout or from $WELLID.", call. = FALSE)
  fits <- sizes[vapply(as.character(sizes), function(s)
                  ACTA_PLATE_GEOMETRY[[s]][1] >= mr && ACTA_PLATE_GEOMETRY[[s]][2] >= mc, logical(1))]
  if (!length(fits))
    stop(sprintf("The layout reaches row %d, column %d -- larger than a 1536-well plate (32 x 48).",
                 mr, mc), call. = FALSE)
  sz <- suppressWarnings(min(fits[fits >= floor_size]))
  if (!is.finite(sz)) sz <- min(fits)
  list(size = sz, rows = ACTA_PLATE_GEOMETRY[[as.character(sz)]][1],
       cols = ACTA_PLATE_GEOMETRY[[as.character(sz)]][2],
       source = if (sz == floor_size) sprintf("default %d-well; the layout fits inside it", floor_size)
                else sprintf("grown past the %d-well default to fit the layout", floor_size),
       max_row = mr, max_col = mc)
}

## ---------------------------------------------------------------------------------------------
## GEOMETRY IS CALIBRATED AGAINST THE RENDER, NOT ASSUMED. Three compounding errors made every
## earlier label size wrong, each of them silent and each in the direction that hides overflow:
##
##  1. `size` IS A FONT SIZE, NOT A DIAMETER. A point drawn at size 10.6 renders a circle 8.00 mm
##     across, not 10.6 -- measured off the PNG. pch 21's glyph fills about 75.5% of its em box.
##  2. TEXT METRICS DEPEND ON THE DEVICE. grid returns 6.47 mm for ".00977" with no device open and
##     7.01 mm inside the device the plot is actually drawn on -- 8% narrower.
##  3. THE RING EATS THE CLEARANCE. It is drawn inward from the fill edge, so a guessed padding
##     factor claimed 0.16 mm that does not exist.
## Together those put the usable radius at 4.66 mm when it was 3.10, and the label at 2.28 when
## 1.65 was the most that fitted. The clipping was real and the arithmetic said it was fine.
##
## THE LEGEND SITS BELOW THE PLATE, and that is what makes the wells big enough to read. With it on
## the right the panel width -- and so the column pitch -- moved with the legend TEXT: 13.63 mm on
## OQ_Test1 against 9.42 mm on OQ_Test2, a 31% swing that left the well at 85% of the pitch in the
## tightest case and no room to grow. Below the plate the panel is full width for every case, the
## pitch is identical everywhere, and the label goes from 1.65 to about 4.
ACTA_PLATE_FIG        <- list(width = 10, height = 7.6, dpi = 200)  # inches / dpi
ACTA_PLATE_FILL_RATIO <- 0.755   # measured: rendered fill diameter / nominal point size
ACTA_PLATE_STROKE     <- 1.6     # ring width, in ggplot2 stroke units
ACTA_PLATE_TEXT_PAD   <- 0.10    # mm of clear space between the glyphs and the inner edge of the ring
ACTA_PLATE_PANEL_IN   <- 9.3     # usable panel width, inches, once margins are taken
ACTA_PLATE_WELL_FRAC  <- 0.85    # well diameter as a fraction of the column pitch
ACTA_PLATE_KEY_SIZE   <- 3.6
ACTA_PLATE_LABEL_WRAP <- 26L
ACTA_PLATE_ALPHA_FLOOR <- 0.05   # a control at zero dose is still a WELL, not an empty position
ACTA_PLATE_SIGFIG     <- 3L

## Ring colours: black / grey50 / grey75-ish. Neither reading of a "10% grey" worked -- R's literal
## grey10 is #1A1A1A, all but identical to Stain's black, and a true 10% printing grey vanished
## against the white plate. #E6E6E6 is the chosen compromise; it is visible but sits close to
## ggplate's own grey90 well frames, so an Unstained well and an unused one read similarly.
ACTA_PLATE_STROKE_COL <- c(Stain = "#000000", Costain = "#7F7F7F", Unstained = "#E6E6E6")

## Fill palette, passed EXPLICITLY rather than left to ggplate's default.
##
## Two reasons, one of them a hard failure. ggplate loads its own palette with
## `utils::data("protti_colours", envir = environment())`, which only resolves when the package is
## ATTACHED -- called through `ggplate::plate_plot()` from a namespace it does not find the dataset
## and dies with "Unknown colour name: placeholder". The pipeline calls into namespaces rather than
## attaching, so the default was never going to work here. The second reason is that the fills are
## then ours to choose.
##
## Okabe-Ito, which is designed to stay distinguishable under every common colour-vision deficiency.
## Black and yellow are left out on purpose: black is the Stain ring, and yellow disappears once the
## alpha floor pales a low-dose well. Hues are assigned in this fixed order and NEVER cycled -- a
## ninth reagent makes ggplate stop with "more categories than provided colours", which is the right
## outcome: silently reusing a hue would say two different reagents were the same one.
ACTA_PLATE_FILL_COL <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7",
                         "#E69F00", "#56B4E9", "#7F3C8D", "#11A579")

actaPlateWellSize <- function(cols = 12L)
  ((ACTA_PLATE_PANEL_IN * 25.4) / cols * ACTA_PLATE_WELL_FRAC) / ACTA_PLATE_FILL_RATIO

actaWellTextRadius <- function(cols = 12L) {
  d    <- actaPlateWellSize(cols) * ACTA_PLATE_FILL_RATIO           # rendered fill diameter, mm
  ring <- ACTA_PLATE_STROKE * ggplot2::.stroke * 25.4 / 96          # lwd 1 = 1/96 inch
  d / 2 - ring / 2 - ACTA_PLATE_TEXT_PAD
}

## Largest geom_text size whose box fits inside that radius. The constraint is not "text narrower
## than the well": the label is a BOX centred in a CIRCLE, so it is the box's CORNER that has to
## stay inside, (w/2)^2 + (h/2)^2 <= r^2. Sizing on width alone lets a tall glyph clip top and
## bottom. Measured inside a device matching the output, per note 2 above.
actaFitLabelSize <- function(ref, r = actaWellTextRadius(), lo = 0.3, hi = 10, tol = 1e-3) {
  f <- tempfile(fileext = ".png")
  grDevices::png(f, width = ACTA_PLATE_FIG$width, height = ACTA_PLATE_FIG$height,
                 units = "in", res = ACTA_PLATE_FIG$dpi)
  on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)
  fits <- function(sz) {
    g <- grid::textGrob(ref, gp = grid::gpar(fontsize = sz * ggplot2::.pt))
    w <- grid::convertWidth(grid::grobWidth(g),   "mm", valueOnly = TRUE)
    h <- grid::convertHeight(grid::grobHeight(g), "mm", valueOnly = TRUE)
    (w / 2)^2 + (h / 2)^2 <= r^2
  }
  if (!fits(lo)) return(lo)
  while (hi - lo > tol) { m <- (lo + hi) / 2; if (fits(m)) lo <- m else hi <- m }
  lo
}

## LEADING ZEROS ARE STRIPPED from the well labels -- ".00244", not "0.00244". One character, worth
## about 17% of label size, and unlike cutting significant figures it costs nothing: at 2 sigfigs a
## 1.25 uL well would print "1.2", and this figure exists to catch a dose typed into the wrong well,
## so a displayed value that disagrees with the sheet is the one thing it must not do.
.actaStripLeadZero <- function(x) sub("^(-?)0[.]", "\\1.", x)

## Escapes a workbook-supplied string for LaTeX. The report feeds Dirname, Alias, Markername and
## check text into kable(escape = FALSE) and into \textbf{%s} by hand, so this is the only thing
## standing between a cell and the TeX stream -- a Dirname of "CD4\input{/etc/passwd}" would
## otherwise be executed by the compiler rather than printed.
##
## THE BACKSLASH MUST GO FIRST, AND VIA A PLACEHOLDER. Rewriting it in place emits
## \textbackslash{}, whose own braces the brace rule -- which has to run too -- would then escape a
## second time, printing a literal \textbackslash\{\} to the page. The three placeholders are
## control characters: no Excel cell can hold one, so they cannot collide with real content.
## The report defined its own one-line version of this THREE times, none of which handled the
## backslash; they now all call here, so there is one implementation and one place to fix.
.actaTexEsc <- function(x) {
  x <- as.character(x)
  x <- gsub("\\", "\x01", x, fixed = TRUE)
  x <- gsub("([#$%&_{}])", "\\\\\\1", x)
  x <- gsub("~", "\x02", x, fixed = TRUE)
  x <- gsub("^", "\x03", x, fixed = TRUE)
  x <- gsub("\x01", "\\textbackslash{}",    x, fixed = TRUE)
  x <- gsub("\x02", "\\textasciitilde{}",   x, fixed = TRUE)
  x <- gsub("\x03", "\\textasciicircum{}",  x, fixed = TRUE)
  x
}

## The design envelope, stated: a 6-character stripped label, alone or stacked. That covers a
## FOURTEEN-step 2-fold series from 5 uL (bottom ".00061") against OQ_Test1's ten steps. The STACKED
## pair binds, never the single dose -- two 6-character lines allow ~4.0 and two 7-character lines
## only ~3.3, so the pair sets the size.
ACTA_PLATE_LABEL_REFS <- c(".00061", ".00061\n.00061")

## Does an ACTUAL label fit? Tested against the real geometry rather than a reference string's
## width, which was only a proxy: a two-line label is narrower than a one-line label of the same
## character count and still overflows.
.actaLabelOverflows <- function(x, size) {
  r <- actaWellTextRadius()
  f <- tempfile(fileext = ".png")
  grDevices::png(f, width = ACTA_PLATE_FIG$width, height = ACTA_PLATE_FIG$height,
                 units = "in", res = ACTA_PLATE_FIG$dpi)
  on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)
  vapply(x, function(v) {
    g <- grid::textGrob(v, gp = grid::gpar(fontsize = size * ggplot2::.pt))
    w <- grid::convertWidth(grid::grobWidth(g),   "mm", valueOnly = TRUE)
    hh <- grid::convertHeight(grid::grobHeight(g), "mm", valueOnly = TRUE)
    (w / 2)^2 + (hh / 2)^2 > r^2
  }, logical(1), USE.NAMES = FALSE)
}

## Format a dose as the analyst typed it rather than as R stores it.
##
## StainQty arrives CHARACTER from some sheets and numeric from others -- OQ_Test1's column holds
## "7.8125E-2" as text because Excel typed the column off its first cells -- so the coercion happens
## here and only here. format() is called per element on purpose: given a vector it picks ONE common
## format for all of them, which pads 5 to "5.0000000" to line up with 0.009765625.
## drop0trailing keeps 2.5 from rendering "2.500000"; scientific = FALSE keeps 0.0078125 out of
## exponent form, which on a plate map reads as a different quantity entirely.
.actaDose <- function(x) {
  vapply(x, function(v) {
    if (is.na(v) || !nzchar(trimws(as.character(v)))) return("")
    n <- suppressWarnings(as.numeric(v))
    if (is.na(n)) trimws(as.character(v))
    else format(n, trim = TRUE, drop0trailing = TRUE, scientific = FALSE)
  }, character(1), USE.NAMES = FALSE)
}

## One row per WELL, from the long Layout_Plate.
##
## A combinatorial well has SEVERAL layout rows -- OQ_Test3 row E is Dirname "CD14+CCR7" carrying
## CCR7_combo and CD14_combo -- and ggplate takes one value per position, so the arms are collapsed
## here: the fill level becomes the SET of aliases in the well.
##
## A WELL WITH NO DIRNAME IS NOT ON THE MAP. OQ_Test1's sheet carries a StainQty for 45 wells that
## carry no Dirname, left behind by filling the template down the plate. The rest of the pipeline
## already ignores those rows -- every read site filters on !is.na(Dirname) -- and drawing them
## would put four phantom series on the map of a single-reagent run.
actaPlateWells <- function(metadata) {
  if (is.null(metadata) || !is.data.frame(metadata) || !nrow(metadata)) return(NULL)
  cn <- list(row     = actaLayoutCol(metadata, c("Row")),
             col     = actaLayoutCol(metadata, c("Column", "Col")),
             plate   = actaLayoutCol(metadata, c("PlateID", "Plate")),
             dirname = actaLayoutCol(metadata, c("Dirname")),
             alias   = actaLayoutCol(metadata, c("Alias")),
             dose    = actaLayoutCol(metadata, c("StainQty")),
             unit    = actaLayoutCol(metadata, c("StainUnit")),
             type    = actaLayoutCol(metadata, c("StainType")),
             eln     = actaLayoutCol(metadata, c("ELN_ID")))
  if (anyNA(c(cn$row, cn$col, cn$dirname, cn$dose))) return(NULL)
  g <- function(k, default = NA_character_)
    if (is.na(cn[[k]])) rep(default, nrow(metadata)) else trimws(as.character(metadata[[cn[[k]]]]))
  dirn <- g("dirname"); ali <- g("alias")
  d <- data.frame(
    plate = { p <- g("plate", "1"); p[is.na(p) | !nzchar(p)] <- "1"; p },
    ## `Row` is UPPER-CASED because $WELLID reports an upper-case letter: a layout typed `a` has
    ## already cost one class of silently dropped well elsewhere in the pipeline.
    row   = toupper(g("row")),
    col   = suppressWarnings(as.integer(g("col"))),
    dirn  = dirn,
    alias = ifelse(is.na(ali) | !nzchar(ali), dirn, ali),    # the pipeline's documented fallback
    qty   = suppressWarnings(as.numeric(metadata[[cn$dose]])),
    unit  = g("unit", ""),
    type  = g("type", "Stain"),
    eln   = g("eln", ""),
    stringsAsFactors = FALSE)
  keep <- !is.na(d$dirn) & nzchar(d$dirn) & !is.na(d$row) & nzchar(d$row) & !is.na(d$col) & d$col >= 1L
  d <- d[keep, , drop = FALSE]
  if (!nrow(d)) return(NULL)
  d$well <- sprintf("%s%d", d$row, d$col)

  ## SPLIT ON PLATE **AND** WELL. Splitting on the well alone merged A1 of plate 1 with A1 of
  ## plate 2 into a single row -- a two-plate run came back with one plate's worth of wells, each
  ## carrying the union of two plates' reagents. Caught by the multi-plate test, which is the only
  ## thing that would have: with one plate the two keys are equivalent.
  do.call(rbind, lapply(split(d, list(d$plate, d$well), drop = TRUE), function(x) {
    a <- unique(x$alias)                                     # first-appearance order, per the sheet
    v <- .actaStripLeadZero(.actaDose(signif(x$qty[match(a, x$alias)], ACTA_PLATE_SIGFIG)))
    data.frame(
      well  = x$well[1], plate = x$plate[1], eln = x$eln[1],
      compo = paste(paste(unique(x$dirn), collapse = "+"), paste(a, collapse = " + "), sep = " / "),
      ## ONE NUMBER WHEN THE ARMS MATCH, STACKED WHEN THEY DO NOT. "0.625/0.625" overflowed its
      ## circle and collided with its neighbours to say nothing -- OQ_Test3's arms are titrated
      ## together, so the repeat is noise. When they differ the reader must see both, and STACKING
      ## beats a slash: the worst realistic pair is 15.89 mm across as "a/b" (forcing size 1.23) but
      ## 7.65 x 4.82 mm stacked, which fits at full size. A circle is the wrong shape for a long line.
      dose  = if (length(unique(v)) == 1L) unique(v) else paste(v, collapse = "\n"),
      ## Arm doses survive rbind() as a string; alpha needs them ranked globally, in actaPlateMap().
      arms  = paste(x$qty[match(a, x$alias)], collapse = "|"),
      unit  = x$unit[1],
      ctrl  = unique(ifelse(x$type %in% c("Unstained", "Costain"), x$type, "Stain"))[1],
      stringsAsFactors = FALSE)
  }))
}

## One ggplot per PlateID. Returns a NAMED list so a caller can title files by plate, and an empty
## list when there is nothing to draw -- never a half-map.
##
## `eln` overrides the layout's own ELN_ID, which is how the script passes the run-level Benchling
## label it has already resolved rather than having this re-derive it.
make_plate_layout_great_again <- function(metadata, plate = NULL, eln = NULL) {
  if (!requireNamespace("ggplate", quietly = TRUE)) {
    warning("Plate map skipped: the 'ggplate' package is not installed.", call. = FALSE)
    return(list())
  }
  w <- actaPlateWells(metadata)
  if (is.null(w) || !nrow(w)) return(list())
  ps  <- actaPlateSize(metadata)
  ids <- unique(w$plate)
  if (!is.null(plate)) ids <- ids[ids %in% as.character(plate)]
  out <- lapply(ids, function(pid) actaPlateMap(w[w$plate == pid, , drop = FALSE], ps, eln))
  names(out) <- ids
  out[!vapply(out, is.null, logical(1))]
}

actaPlateMap <- function(w, ps, eln = NULL) {
  size  <- actaPlateWellSize(ps$cols)
  label <- min(vapply(ACTA_PLATE_LABEL_REFS, actaFitLabelSize, numeric(1)))

  ## alpha on the RANK of the dose, not the dose itself: a titration series is 2-fold, so a
  ## proportional alpha puts everything below the top two doses at the floor and the gradient
  ## disappears. RANKED OVER EVERY ARM AND AVERAGED PER WELL -- taking the well's MAXIMUM overclaims,
  ## rendering a well that holds 5 uL and 0.156 as dark as a 5 uL well. There is no single honest
  ## answer for one well with two doses, so no arm is claimed and the label carries the exact values.
  arms  <- lapply(strsplit(w$arms, "|", fixed = TRUE), as.numeric)
  u     <- sort(unique(unlist(arms)))
  w$a   <- vapply(arms, function(q)
             mean(if (length(u) > 1) (match(q, u) - 1) / (length(u) - 1) else 1), numeric(1))
  mixed <- vapply(arms, function(q) length(unique(q)) > 1L, logical(1))
  w$ctrl <- factor(w$ctrl, levels = names(ACTA_PLATE_STROKE_COL))
  ## Wrapped so the legend cannot run away with the panel width.
  w$compo <- vapply(w$compo, function(x)
    paste(strwrap(x, width = ACTA_PLATE_LABEL_WRAP), collapse = "\n"), character(1), USE.NAMES = FALSE)

  over <- unique(w$dose[.actaLabelOverflows(w$dose, label)])
  if (length(over))
    warning(sprintf(paste0("Plate map: %d label(s) do not fit a well at size %.2f and will be ",
                           "clipped: %s. Three combinatorial arms, or two arms both at eight ",
                           "characters, are past the design envelope."),
                    length(over), label, paste(gsub("\n", "/", over), collapse = ", ")),
            call. = FALSE)

  unit <- unique(w$unit[!is.na(w$unit) & nzchar(w$unit)])
  cap  <- wrapToWidth(paste0("Numbers inside the well show StainQty",
                             if (length(unit)) sprintf(" (%s)", paste(unit, collapse = ", ")) else "",
                             ## Stated only when it applies: a reader seeing two numbers stacked in
                             ## one well has no way to know which the shading refers to.
                             if (any(mixed)) sprintf(paste0(". %d well(s) hold reagents at different ",
                                                            "quantities; their shading is the mean ",
                                                            "of the two."), sum(mixed)) else ""),
                      ACTA_PLATE_FIG$width, font_size = actaCaptionSize(ACTA_PLATE_FIG$width),
                      frac = 0.86)
  ttl <- if (!is.null(eln) && length(eln) && nzchar(eln[1])) eln[1] else
           paste(unique(w$eln[nzchar(w$eln)]), collapse = ", ")

  p <- ggplate::plate_plot(data = w, position = well, value = compo, label = dose,
                           plate_size = ps$size, plate_type = "round", show_legend = TRUE,
                           colour = ACTA_PLATE_FILL_COL, scale = 1.8, silent = TRUE)
  ## Overwrite every size ggplate derived from the device AND from max_label_length -- layer 1 is the
  ## empty plate frame, 2 the wells, 3 the dose labels. All three, or the frame circles and the
  ## filled wells stop being concentric.
  p$layers[[1]]$aes_params$size <- size
  p$layers[[2]]$aes_params$size <- size
  p$layers[[3]]$aes_params$size <- label
  ## THE LABEL COLOUR HAS TO ACCOUNT FOR ALPHA, and ggplate's does not. It picks black or white per
  ## well from the RAW fill's luminance, which is right for an opaque well and wrong for every well
  ## here: a dose two steps down the series is drawn at low alpha, so a dark fill that earned white
  ## text renders pale and the white text disappears into it. Visible in the first render as an
  ## invisible "0" on a costain well.
  ##
  ## The fill is blended over the WHITE panel -- c' = 1 - a(1 - c) -- and the decision is made on
  ## the luminance of what is actually drawn.
  p$layers[[3]]$aes_params$colour <- vapply(seq_len(nrow(p$data)), function(i) {
    rgb  <- grDevices::col2rgb(p$data$colours[i]) / 255
    a    <- p$data$a[i]
    lum  <- sum(c(0.2126, 0.7152, 0.0722) * (1 - a * (1 - rgb)))
    if (lum < 0.5) "#FFFFFF" else "#000000"
  }, character(1))
  p$layers[[2]]$mapping <- utils::modifyList(p$layers[[2]]$mapping, ggplot2::aes(alpha = a))
  ## "transparent", NOT NA. ggplot2's remove_missing() treats an NA colour as a MISSING aesthetic
  ## and silently drops the row -- NA here deleted every well and left only the rings.
  p$layers[[2]]$aes_params$colour <- "transparent"
  ring <- ggplot2::geom_point(data = p$data,
                              ggplot2::aes(x = col, y = row_num, colour = ctrl), shape = 21,
                              fill = NA, size = size, stroke = ACTA_PLATE_STROKE,
                              inherit.aes = FALSE)
  p$layers <- c(p$layers[1:2], list(ring), p$layers[3])      # over the fill, under the labels

  hs <- max(min(16, length(unique(w$compo)) * 5.3), 14)      # ACTA_Script's headerSize rule
  p +
    ggplot2::scale_alpha_continuous(range = c(ACTA_PLATE_ALPHA_FLOOR, 1), guide = "none") +
    ## drop = TRUE (the default): with drop = FALSE the scale keeps all three levels, so an absent
    ## control appeared as a LABEL with no key beside it -- the glyph comes from the layer's key
    ## data, which only covers observed levels. Naming a control the plate does not contain is noise.
    ggplot2::scale_colour_manual(name = "StainType", values = ACTA_PLATE_STROKE_COL) +
    ggplot2::labs(title = ttl, subtitle = sprintf("PlateID: %s", w$plate[1]),
                  fill = "Dirname / Alias", caption = cap) +
    ## ONE key size for both guides -- ggplate sets its own for the fill guide off max_label_length.
    ## The fill key carries NO ring: it is about fill, and an outline on it reads as a fourth
    ## StainType. Its size gets the stroke allowance back, because ggplot2 sizes a point as
    ## `size * .pt + stroke * .stroke / 2` and a ring therefore renders wider than an equal disc.
    ggplot2::guides(
      fill   = ggplot2::guide_legend(order = 1, override.aes = list(
                 alpha = 1, colour = NA,
                 size = ACTA_PLATE_KEY_SIZE + ACTA_PLATE_STROKE * (ggplot2::.stroke / 2) / ggplot2::.pt)),
      colour = ggplot2::guide_legend(order = 2, override.aes = list(
                 fill = NA, alpha = 1, size = ACTA_PLATE_KEY_SIZE, stroke = ACTA_PLATE_STROKE))) +
    ## Title/subtitle/caption formatting taken from ConcatPlot (which SIPlot and SatPlot share):
    ## title headerSize-1 bold, subtitle headerSize-3 italic gray50, caption italic hjust 0 at
    ## actaCaptionSize(). The per-antibody figures use a different title/subtitle scale; captions are
    ## the one thing all six figures already agree on.
    ggplot2::theme(
      plot.title    = ggplot2::element_text(size = hs - 1, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = hs - 2 - 1, colour = "gray50", face = "italic"),
      plot.caption  = ggplot2::element_text(face = "italic",
                                            size = actaCaptionSize(ACTA_PLATE_FIG$width), hjust = 0),
      legend.position   = "bottom",
      legend.box        = "horizontal",
      legend.direction  = "horizontal",
      legend.key.height = ggplot2::unit(1.15, "lines"),
      legend.key.width  = ggplot2::unit(1.0,  "lines"),
      legend.text       = ggplot2::element_text(size = 7, margin = ggplot2::margin(l = 2)),
      legend.title      = ggplot2::element_text(size = 8),
      legend.spacing.y  = ggplot2::unit(0.35, "lines"),
      legend.box.spacing = ggplot2::unit(0.4, "lines"))
}

## Read one per-population opt-in column (layout_report / layout_export / stats_export) off the
## gating_template sheet and return a logical vector NAMED BY ALIAS.
##
## The column name is matched CASE-INSENSITIVELY. These shipped capitalised and the sheet was
## later lower-cased; because an unmatched name falls through to `default` (TRUE) rather than
## erroring, an exact match would turn a renamed column into a silent "everything on".
##
## DEFAULT IS TRUE, in three senses, so no existing layout file changes behaviour when these
## columns are introduced: an ABSENT column, a BLANK cell, and an "NA" cell all mean TRUE.
## Only an explicit FALSE turns a population off.
##
## Type-looseness is deliberate. Excel round-trips these badly: a column of real TRUE/FALSE
## booleans with a single cell typed as the TEXT "NA" comes back from readxl as CHARACTER
## c("TRUE","FALSE","NA"), not logical -- so both shapes must parse, plus the 1/0 and yes/no
## spellings an analyst may reasonably type.
##
## attr(,"explicit") marks the cells that said TRUE *out loud*. Callers use it to tell an
## intentional TRUE from a defaulted one: a flag on a row that cannot produce a population
## node (the marker "+/-" row) is worth naming only if the user actually asked for it.
actaTemplateFlag <- function(gt, col, default = TRUE) {
  n <- nrow(gt)
  hit <- match(tolower(col), tolower(names(gt)))
  if (!is.na(hit)) col <- names(gt)[hit]
  if (!col %in% names(gt)) {
    out <- setNames(rep(default, n), gt$alias)
    attr(out, "explicit") <- setNames(rep(FALSE, n), gt$alias)
    return(out)
  }
  chr   <- trimws(toupper(as.character(gt[[col]])))
  isYes <- !is.na(chr) & chr %in% c("TRUE","T","YES","Y","1")
  isNo  <- !is.na(chr) & chr %in% c("FALSE","F","NO","N","0")
  out   <- ifelse(isYes, TRUE, ifelse(isNo, FALSE, default))   # blank / NA / "NA" -> default
  out   <- setNames(as.logical(out), gt$alias)
  attr(out, "explicit") <- setNames(isYes, gt$alias)
  out
}

## MeFI and robustSD are computed on LINEAR fluorescence, not on the biexp values the GatingSet
## holds. The Stain Index in the literature (Maecker et al. 2004, Cytometry A 62A:169-173) is a
## linear-scale statistic, and computing it on a non-linear scale makes the value depend on the
## transform's parameters -- which is why 2_84's biexp change silently made its Stain Indices
## incomparable with 2_83's.
##
## Back-transforming the EVENTS is exact, not an approximation: the gate decided which events are
## in the population, and that membership does not change. Only the values are returned to the
## units they were acquired in. (Doing it via the medians alone would not work for robustSD -- a
## median is equivariant under a monotonic transform, a MAD is not.)
##
## `gs_pop_get_stats(type=)` calls them with the flowFrame and NOTHING else, so the reagent channel
## and the transform cannot arrive as arguments -- flowWorkspace fixes the signature at function(fr).
## They used to be read out of the CALLER'S SCOPE. That works under source() and breaks under a
## package namespace, in the worst possible way: `exists("actaTrans", inherits = TRUE)` searches the
## namespace, its imports and the global environment, but NOT the script's own environment (run_acta()
## evaluates the script in `new.env(parent = globalenv())`). So the lookup would quietly return FALSE
## and every MeFI / robustSD would be computed on BIEXP-TRANSFORMED values instead of linear ones --
## a plausible-looking number, silently wrong, in the two statistics the Stain Index is built from.
## `channel` was worse still: absent it errors, but a stale value left in the global environment by an
## earlier run in the same session would be picked up instead.
##
## So the context is now set EXPLICITLY, once per antibody group, by actaSetStatContext(). Missing
## context is a hard error, not a wrong answer. This is also what makes the pair safe to move into a
## package namespace unchanged.
.acta_stat_ctx <- new.env(parent = emptyenv())

## Called from the per-antibody loop, immediately after `channel` is resolved. `trans` is the biexp
## transform whose $inverse takes the data back to linear; pass NULL for untransformed data.
actaSetStatContext <- function(channel, trans = NULL) {
  if (!is.character(channel) || length(channel) != 1L || is.na(channel) || !nzchar(channel))
    stop("actaSetStatContext(): `channel` must be a single non-empty channel name.", call. = FALSE)
  assign("channel", channel, envir = .acta_stat_ctx)
  assign("trans",   trans,   envir = .acta_stat_ctx)
  invisible(TRUE)
}

## NOTE: linear-scale statistics are more fragile near zero than biexp ones. A dim marker's negative
## population can have a very small linear MAD, which inflates SI; on unmixed spectral data the
## values can be negative. That sensitivity is the cost of matching the literature.
.actaLinear <- function(fr) {
  ch <- get0("channel", envir = .acta_stat_ctx, ifnotfound = NULL)
  if (is.null(ch))
    stop("pop.MeFI()/pop.rsd() were called before actaSetStatContext(): the reagent channel is unset. ",
         "Call actaSetStatContext(channel, actaTrans) once the channel for this antibody is known.",
         call. = FALSE)
  v  <- exprs(fr)[, ch]
  tr <- get0("trans", envir = .acta_stat_ctx, ifnotfound = NULL)
  if (!is.null(tr) && !is.null(tr$inverse)) tr$inverse(v) else v
}
pop.rsd<-function(fr){
  res<-mad(.actaLinear(fr))
  names(res)<-"robustSD"
  res
}
pop.MeFI<-function(fr){
  res<-quantile(.actaLinear(fr), 0.5)
  names(res)<-"MeFI"
  res
}

## Write the titration export as a FORMATTED sheet using TWO colour families, so the block the
## analyst reads and fills in is unmistakable against the derived data:
##   PRIMARY   columns (named in `primary`) -> MAROON header, white/light-pink bands
##   SECONDARY columns (everything else)    -> BLUE   header, white/light-blue bands
## Replaces writexl::write_xlsx(), which cannot style cells, hence the openxlsx dependency.
##
## Palettes (headers are white-text-on-dark, values black-on-light, so both stay legible).
## All hex values are arguments, so if any is a shade off, correct it at the call site rather
## than editing this function:
##   primary    #93398F header / #ECCFED band   -- plum/purple header (rgb 147,57,143) over a
##              light lavender row band (rgb 236,207,237), both supplied by the operator to
##              match the reference Excel table. The band is a light tint of the same hue
##              family, and white bold text on the header clears the WCAG AA 4.5:1 floor.
##   secondary  #1F4E79 header / #DCE9F5 band
## EVERY cell (header and body, both families) gets the SAME thin black border and is centred
## horizontally and vertically, per the reference formatting -- the borders deliberately do not
## follow the block colour.
##
## Font: the whole sheet is set to `font_name` / `font_size` via modifyBaseFont() AND on each
## style, so it applies to filler cells too. Excel shows the Office body font as
## "Aptos Narrow (Body)"; the name written into the file is plain "Aptos Narrow" -- the "(Body)"
## is only Excel's UI label for the theme slot. If the font is not installed, Excel substitutes
## silently.
##
## Also: header frozen and filterable, per-column widths capped at 40 so the long multi-line
## `label` cannot blow the sheet out, and Date columns given an explicit yyyy-mm-dd format
## (Excel would otherwise render the serial date per locale).
## Values are untouched -- this is presentation only. Columns that are entirely NA (e.g. the
## operator-fillable Titer / Titration_Status) come out as genuinely EMPTY cells, still styled,
## ready to be typed into.
writeTitrationExport <- function(df, path, sheet = "TitrationExport",
                                 audit = NULL,
                                 primary = character(),
                                 primary_palette   = c(header = "#93398F", band = "#ECCFED"),
                                 secondary_palette = c(header = "#1F4E79", band = "#DCE9F5"),
                                 border_col = "#000000",
                                 font_name = "Aptos Narrow", font_size = 12) {
  if (!requireNamespace("openxlsx", quietly = TRUE))
    stop("writeTitrationExport() needs the 'openxlsx' package (install.packages('openxlsx')).")
  n <- nrow(df); p <- ncol(df)
  idx_primary   <- which(names(df) %in% primary)
  idx_secondary <- setdiff(seq_len(p), idx_primary)

  wb <- openxlsx::createWorkbook()
  ## Base font first, so anything not explicitly styled (e.g. cells the analyst types into
  ## later) still comes out in the same face and size.
  openxlsx::modifyBaseFont(wb, fontSize = font_size, fontName = font_name)
  openxlsx::addWorksheet(wb, sheet)
  openxlsx::writeData(wb, sheet, df, withFilter = TRUE)   # header styled per block below

  hdrStyle <- function(pal)
    openxlsx::createStyle(fontName = font_name, fontSize = font_size,
                          fontColour = "#FFFFFF", fgFill = pal[["header"]],
                          textDecoration = "bold", halign = "center", valign = "center",
                          wrapText = TRUE, border = "TopBottomLeftRight",
                          borderColour = border_col, borderStyle = "thin")
  bodyStyle <- function(fill)
    openxlsx::createStyle(fontName = font_name, fontSize = font_size, fgFill = fill,
                          halign = "center", valign = "center",
                          border = "TopBottomLeftRight",
                          borderColour = border_col, borderStyle = "thin")

  odd  <- if (n > 0) seq(2, n + 1, by = 2) else integer(0)   # data starts on sheet row 2
  even <- if (n > 1) seq(3, n + 1, by = 2) else integer(0)
  for (grp in list(list(idx = idx_primary,   pal = primary_palette),
                   list(idx = idx_secondary, pal = secondary_palette))) {
    if (!length(grp$idx)) next
    openxlsx::addStyle(wb, sheet, hdrStyle(grp$pal), rows = 1, cols = grp$idx, gridExpand = TRUE)
    if (length(odd))
      openxlsx::addStyle(wb, sheet, bodyStyle("#FFFFFF"), rows = odd, cols = grp$idx, gridExpand = TRUE)
    if (length(even))
      openxlsx::addStyle(wb, sheet, bodyStyle(grp$pal[["band"]]), rows = even, cols = grp$idx, gridExpand = TRUE)
  }
  ## Date columns: keep the banding/borders (stack=TRUE) and add an unambiguous ISO format.
  ## Font and centring are restated so the stacked style cannot drop them.
  if (n > 0) for (j in which(vapply(df, function(x) inherits(x, "Date"), logical(1))))
    openxlsx::addStyle(wb, sheet,
                       openxlsx::createStyle(numFmt = "yyyy-mm-dd", fontName = font_name,
                                             fontSize = font_size, halign = "center",
                                             valign = "center"),
                       rows = 2:(n + 1), cols = j, gridExpand = TRUE, stack = TRUE)

  ## Width from the widest cell, capped at 40. An all-NA column falls back to its header width.
  widths <- vapply(seq_len(p), function(j) {
    v   <- df[[j]]
    txt <- if (inherits(v, "Date")) format(v) else as.character(v)
    min(max(c(nchar(names(df)[j]), nchar(txt[!is.na(txt)]))) + 2, 40)
  }, numeric(1))
  openxlsx::setColWidths(wb, sheet, cols = 1:p, widths = widths)
  openxlsx::freezePane(wb, sheet, firstActiveRow = 2)

  ## ---- audit sheets, AFTER the data sheet -------------------------------------------------------
  ## Verbatim copies of the instructions workbook, carried along so the export is self-contained:
  ## whoever reads it a year later can see the plate map, the gating template and the run settings
  ## that produced these numbers without needing the original file.
  ##
  ## FOR AUDIT ONLY. Nothing reads them back -- they are deliberately NOT styled like the data
  ## sheet, so nobody mistakes them for output, and they are appended after it so the data sheet
  ## stays the one that opens first.
  if (length(audit)) {
    hdr <- openxlsx::createStyle(fontName = font_name, fontSize = font_size,
                                 textDecoration = "bold", border = "bottom",
                                 borderColour = border_col, borderStyle = "thin")
    for (nm in names(audit)) {
      d <- audit[[nm]]
      if (is.null(d) || !is.data.frame(d)) next
      ## Sheet names are capped at 31 characters by the format, and must be unique. The source
      ## sheet name is used as-is: an "audit_" prefix added nothing a reader needs, and it made the
      ## sheets read as a different artefact rather than as copies of the instructions file.
      sn <- substr(nm, 1, 31)
      openxlsx::addWorksheet(wb, sn)
      if (ncol(d)) {
        openxlsx::writeData(wb, sn, d)
        openxlsx::addStyle(wb, sn, hdr, rows = 1, cols = seq_len(ncol(d)), gridExpand = TRUE)
        openxlsx::setColWidths(wb, sn, cols = seq_len(ncol(d)), widths = "auto")
        openxlsx::freezePane(wb, sn, firstActiveRow = 2)
      }
    }
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  ## openxlsx leaves a dangling drawing/vmlDrawing relationship in every sheet it writes, which is
  ## what makes Excel offer to "recover" the file. See actaOpcScrub().
  actaOpcScrub(path)
  ## AND THAT IS AS FAR AS ACTA CAN GUARANTEE IT. Traced 2026-09-08: the export leaves this function
  ## as a well-formed 30-part package (instrumented -- one save, one scrub, 0 findings), and ~40 s
  ## later, with no ACTA code having touched it, the file on disk has 45 parts. The extra 15 are
  ## 9 customXml/*, docProps/custom.xml and 5 [trash]/000N.dat: SharePoint document-library
  ## metadata. The ONEDRIVE SYNC CLIENT is writing them into every Office file in the synced
  ## library, and the [trash] blobs it adds have no content type, so the package it leaves behind is
  ## the one that fails a strict reader. Reproduced with a control: an identical workbook written to
  ## /tmp in the same second stayed at 12 parts while the copy inside the library went to 27.
  ## Nothing here can prevent that -- re-scrubbing later in the run would just lose the same race --
  ## so it is documented rather than papered over, and tests/test_workbook_package.R tells the two
  ## causes apart instead of blaming ACTA for this one.
  invisible(path)
}

## Remove the OPC damage openxlsx leaves in every workbook it saves.
##
## THE DEFECT, reproduced from a one-sheet workbook built from scratch with openxlsx 4.2.8.1: every
## sheet's .rels declares a `drawing` -> ../drawings/drawingN.xml and a `vmlDrawing` ->
## ../drawings/vmlDrawingN.vml whether or not those parts exist, and they do not exist unless the
## sheet actually has a drawing. A relationship whose target part is absent is a malformed package,
## which is what makes Excel offer to recover the file instead of opening it. Nothing in the sheet
## body references the relationship, so removing it cannot change what the workbook contains.
##
## WHY THIS RUNS AFTER EVERY SAVE rather than being a one-off repair of the files in the repo: the
## titration export is WRITTEN BY US on every run, so a workbook we hand a colleague carries the
## defect freshly each time. Repairing the shipped workbooks and not this would fix the copy nobody
## edits and leave the copy everybody receives.
##
## `[trash]/` parts are removed for the same reason -- they have no content type at all, so they are
## not addressable parts, and the part name begins with a `[`, which OPC reserves. They arrived with
## the original workbook, hold Windows printer DEVMODE blobs, and are referenced by nothing.
##
## Deliberately narrow: A RELATIONSHIP IS REMOVED ONLY IF ITS TARGET IS MISSING, and only for the
## three types openxlsx invents. Anything else dangling is left in place and warned about rather
## than guessed at.
##
## [Content_Types].xml CARRIES A MATCHING OVERRIDE for each of those absent drawings, and missing
## that cost a test failure: with the relationships gone the package still declared a content type
## for /xl/drawings/drawing1.xml, which is the same fault stated in the other file. An Override
## naming a part that is not in the archive can never be referenced by anything, so removing it is
## unconditionally safe -- unlike a relationship, which something might point at.
##
## printerSettings is NOT part of this, which is worth writing down because it looks as though it
## should be: openxlsx declares a printerSettings relationship AND `<pageSetup r:id>` AND writes the
## .bin part AND declares a `.bin` Default. That chain is complete and is left alone.
##
## Every other part is copied byte for byte, so this can never alter a cell -- unlike re-saving
## through openxlsx, which corrupts xml:space="preserve" shared strings in sheets it was not even
## asked to touch.
##
## Failure is a WARNING, never an error: the export is written by then and a cosmetic package fault
## is not worth losing a completed run over.
## Resolve a relationship Target against the folder its .rels sits beside. normalizePath() cannot be
## used: whether the part EXISTS is the question being asked, so "../" is collapsed by hand.
.actaOpcResolve <- function(base, target) {
  p <- gsub("^\\./", "", file.path(base, target))
  while (grepl("[^/]+/\\.\\./", p)) p <- sub("[^/]+/\\.\\./", "", p)
  sub("^\\./", "", p)
}

## An OPC conformance check for one .xlsx, in base R plus utils::unzip. Returns a character vector
## of findings -- empty means the package is well formed. Lived in tests/test_workbook_package.R
## until 2026-09-08; it is here now so that actaOpcScrub() can VERIFY its own repair with the same
## definition of "valid" the gate uses, rather than the two drifting apart.
##
## Relationship targets are resolved by collapsing "../" BY HAND. normalizePath() cannot be used:
## the whole question is whether the part exists, and normalizePath(mustWork = FALSE) on a missing
## path returns something that then compares unequal for the wrong reason.
actaOpcFindings <- function(path) {
  nm   <- utils::unzip(path, list = TRUE)$Name
  find <- character(0)
  read1 <- function(p) paste(readLines(unz(path, p), warn = FALSE), collapse = "\n")

  ct <- read1("[Content_Types].xml")
  defaults  <- tolower(gsub('.*Extension="([^"]*)".*', "\\1",
                            regmatches(ct, gregexpr('<Default Extension="[^"]*"', ct))[[1]]))
  overrides <- gsub('.*PartName="/([^"]*)".*', "\\1",
                    regmatches(ct, gregexpr('<Override PartName="/[^"]*"', ct))[[1]])

  for (p in setdiff(overrides, nm)) find <- c(find, sprintf("Override names a missing part: /%s", p))
  for (p in setdiff(nm, "[Content_Types].xml")) {
    ext <- tolower(sub(".*\\.", "", p))
    if (!(p %in% overrides) && !(ext %in% defaults))
      find <- c(find, sprintf("part with no content type: %s", p))
  }

  for (r in grep("\\.rels$", nm, value = TRUE)) {
    txt  <- read1(r)
    base <- dirname(dirname(r))
    for (el in regmatches(txt, gregexpr("<Relationship[^>]*?/>", txt))[[1]]) {
      if (grepl('TargetMode="External"', el)) next
      tgt <- sub('.*\\sTarget="([^"]*)".*', "\\1", el)
      if (identical(tgt, el) || grepl("^(https?:|mailto:|file:|/)", tgt)) next
      p <- gsub("^\\./", "", file.path(base, tgt))
      while (grepl("[^/]+/\\.\\./", p)) p <- sub("[^/]+/\\.\\./", "", p)
      if (!(p %in% nm))
        find <- c(find, sprintf("%s: dangling %s -> %s",
                                r, sub('.*\\sId="([^"]*)".*', "\\1", el), tgt))
    }
  }
  ## Every XML part must parse. A truncated sheet is the other way a package fails to open, and it
  ## costs nothing to check here. xml2 is a knitr/rmarkdown dependency, so it is already present;
  ## if it somehow is not, skip this leg rather than fail the whole check.
  if (requireNamespace("xml2", quietly = TRUE))
    for (p in grep("\\.(xml|rels)$", nm, value = TRUE))
      if (inherits(try(suppressWarnings(xml2::read_xml(read1(p))), silent = TRUE), "try-error"))
        find <- c(find, sprintf("malformed XML: %s", p))
  find
}

actaOpcScrub <- function(path) {
  if (!file.exists(path)) return(invisible(FALSE))
  ## ABSOLUTE, before anything else: this function setwd()s into its own temp folder to build the
  ## replacement archive, and knitr moves the working directory around underneath a chunk while the
  ## report renders. Nothing is known to have broken because of it -- it is cheap insurance on a
  ## function that changes directory and then copies to a caller-supplied path.
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  ok <- tryCatch({
    names_in <- utils::unzip(path, list = TRUE)$Name
    trash <- grep("^\\[trash\\]/", names_in, value = TRUE)
    rels  <- grep("\\.rels$", names_in, value = TRUE)
    tmp   <- file.path(tempdir(), sprintf("actaopc_%s", basename(tempfile(""))))
    dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    utils::unzip(path, exdir = tmp, setTimes = TRUE)

    changed <- length(trash) > 0L
    for (r in rels) {
      f <- file.path(tmp, r)
      if (!file.exists(f)) next
      txt  <- paste(readLines(f, warn = FALSE), collapse = "\n")
      base <- dirname(dirname(r))
      ## The part this .rels belongs to -- xl/worksheets/_rels/sheet1.xml.rels -> sheet1.xml -- which
      ## is where a reference to a dropped id would live.
      owner <- file.path(tmp, file.path(base, sub("\\.rels$", "", basename(r))))
      drop <- character(0); ids <- character(0)
      for (el in regmatches(txt, gregexpr("<Relationship[^>]*?/>", txt))[[1]]) {
        tgt <- sub('.*\\sTarget="([^"]*)".*', "\\1", el)
        typ <- sub('.*\\sType="([^"]*)".*',   "\\1", el)
        if (identical(tgt, el) || grepl('TargetMode="External"', el)) next
        if (grepl("^(https?:|mailto:|file:|/)", tgt)) next
        if (.actaOpcResolve(base, tgt) %in% names_in) next
        if (!grepl("/(drawing|vmlDrawing)$", typ)) {
          ## Not one of the two openxlsx invents: say so and leave it, rather than guessing at a
          ## relationship whose removal might be the wrong repair.
          warning(sprintf("%s: leaving a dangling %s relationship alone (%s)", basename(path),
                          sub(".*/", "", typ), tgt), call. = FALSE)
          next
        }
        drop <- c(drop, el)
        ids  <- c(ids, sub('.*\\sId="([^"]*)".*', "\\1", el))
      }
      if (!length(drop)) next
      for (el in drop) txt <- sub(el, "", txt, fixed = TRUE)
      writeLines(txt, f, useBytes = TRUE)
      ## In every workbook seen so far the sheet does NOT reference these ids -- openxlsx declares
      ## the relationship and then never uses it. If one ever does, dropping the relationship while
      ## leaving the reference would trade this fault for its mirror image, so the element goes too.
      if (file.exists(owner)) {
        body <- paste(readLines(owner, warn = FALSE), collapse = "\n")
        was  <- body
        for (id in ids)
          body <- gsub(sprintf('<(drawing|legacyDrawing)\\b[^>]*\\br:id="%s"[^>]*/>', id), "", body)
        if (!identical(body, was)) writeLines(body, owner, useBytes = TRUE)
      }
      changed <- TRUE
    }

    ## An Override for a part that is not in the archive.
    ctf <- file.path(tmp, "[Content_Types].xml")
    if (file.exists(ctf)) {
      ct <- paste(readLines(ctf, warn = FALSE), collapse = "\n")
      was <- ct
      for (el in regmatches(ct, gregexpr("<Override[^>]*?/>", ct))[[1]]) {
        part <- sub('.*\\sPartName="/([^"]*)".*', "\\1", el)
        if (identical(part, el) || part %in% setdiff(names_in, trash)) next
        ct <- sub(el, "", ct, fixed = TRUE)
      }
      if (!identical(ct, was)) { writeLines(ct, ctf, useBytes = TRUE); changed <- TRUE }
    }

    if (!changed) return(invisible(FALSE))

    keep <- setdiff(names_in, trash)
    old  <- setwd(tmp); on.exit(setwd(old), add = TRUE)
    ## -X drops the extra-attribute fields, which is what keeps the archive reproducible across
    ## machines; -q so a run's log is not filled with 30 part names.
    utils::zip(zipfile = "rebuilt.xlsx", files = keep, flags = "-Xq9")
    setwd(old)
    if (!file.copy(file.path(tmp, "rebuilt.xlsx"), path, overwrite = TRUE))
      stop("the rebuilt package could not be copied over the original", call. = FALSE)
    ## VERIFY, and say so if it did not take. A repair that silently fails is worse than no repair:
    ## it is indistinguishable from a clean file at every later gate.
    left <- actaOpcFindings(path)
    if (length(left))
      warning(sprintf("%s: still not a well-formed package after the scrub -- %s", basename(path),
                      paste(utils::head(left, 4L), collapse = "; ")), call. = FALSE)
    TRUE
  }, error = function(e) {
    warning(sprintf("Could not clean the workbook package for '%s': %s", basename(path),
                    conditionMessage(e)), call. = FALSE)
    FALSE
  })
  invisible(isTRUE(ok))
}

## Validate the LAYOUT, per antibody group, BEFORE any FCS is read -- a layout mistake should
## surface in seconds with the offending wells named, not minutes later as something cryptic.
## Severity is matched to what each condition actually does downstream:
##
##  ERROR   duplicate StainQty among the `Stain` wells. The titration series is assumed to
##          have one well per volume: the histogram/pseudocolor plots facet on StainQty (so
##          duplicates silently overlay in one panel) and maxSI_StainQty picks the volume by
##          SI, which is ambiguous when two wells share a volume.
##          NOTE the check covers `Stain` wells ONLY -- Unstained and Costain are both
##          StainQty 0 by construction, so including them would flag every group.
##  ERROR   no Costain well. This is the one that used to fail obscurely: costainPosition
##          becomes NA, boundaryIndex becomes NA, and gs[[NA]] throws
##          "The data to be assigned is missing sample: NA". It also makes pp_costain_floor
##          hand back a -Inf floor, silently degrading a *_costain gate to un-floored flowMeans.
##  WARNING more than one Costain well -- supported on purpose (the pp_* plugins pool all
##          Costain events for the quantile, and the script's channel lookup takes [1]), but
##          usually a layout slip worth seeing.
##  WARNING more than one Unstained well -- harmless, they are all dropped before gating.
##
## Groups whose Dirname is NA are ignored, matching how `antibody` is derived.
validateLayoutGroups <- function(metadata, groups, digits = 6, stop_on_error = TRUE,
                                 return_all = FALSE) {
  ## Sliced per TITRATION, not per folder. `metadata$Dirname == ab` was right only while the two
  ## meant the same thing: handed a titration KEY it matches nothing, and every group came back
  ## empty as "0 Costain wells -- exactly one is required" naming an Alias. Handed a folder that
  ## carries two co-titrated markers it does the opposite -- pools both series, so their stain
  ## quantities read as duplicates of each other.
  groups <- .actaAsGroups(groups)
  fatal <- character(0); notes <- character(0)
  for (.g in seq_len(nrow(groups))) {
    ab   <- groups$key[.g]
    ## Name the folder too when it differs from the key, so a message about "CD4" says where to look.
    where <- if (identical(groups$dirname[.g], ab)) ab
             else sprintf("%s (Dirname '%s')", ab, groups$dirname[.g])
    g    <- metadata[actaTitrationRowMask(metadata, groups$dirname[.g], groups$alias[.g]), ,
                     drop = FALSE]
    well <- paste0(g$Row, g$Column)
    st   <- as.character(g$StainType)
    idx  <- which(isLayoutValue(st, "Stain"))
    v    <- round(suppressWarnings(as.numeric(g$StainQty[idx])), digits)
    dup  <- unique(v[duplicated(v) & !is.na(v)])
    if (length(dup))
      fatal <- c(fatal, sprintf("  %s: duplicate StainQty %s (wells %s)", where,
                                paste(dup, collapse = ", "),
                                paste(well[idx][!is.na(v) & v %in% dup], collapse = ", ")))
    ## EXACTLY one Costain per group. Zero leaves the gate floor at -Inf (no floor at all, which
    ## looks like a successful run); more than one silently POOLS their events into one floor, so
    ## the number the gate is built on belongs to no single well. Both are now fatal.
    nCo <- which(isLayoutValue(st, "Costain"))
    if (length(nCo) != 1)
      fatal <- c(fatal, sprintf("  %s: %d Costain wells%s -- exactly one is required (it sets the gate floor)",
                                where, length(nCo),
                                if (length(nCo)) sprintf(" (%s)", paste(well[nCo], collapse = ", ")) else ""))
    nUn <- which(isLayoutValue(st, "Unstained"))
    if (length(nUn) > 1)
      notes <- c(notes, sprintf("  %s: %d Unstained wells (%s); all are dropped before gating",
                                where, length(nUn), paste(well[nUn], collapse = ", ")))
  }
  if (length(notes))
    warning("Instructions check:\n", paste(notes, collapse = "\n"), call. = FALSE)
  ## stop_on_error = FALSE lets actaPrevalidate() gather these alongside its own findings and
  ## report every problem in one message, instead of the analyst fixing one and re-running.
  if (length(fatal) && stop_on_error)
    stop("Instructions check failed:\n", paste(fatal, collapse = "\n"),
         "\nFix the instructions sheet(s), or comment the offending rows out by blanking Dirname.",
         call. = FALSE)
  ## return_all hands BOTH severities back so actaPrevalidateRecords() can render the notes as
  ## warn records. The warning() above still fires either way, so the console path is unchanged;
  ## the default return value is still the fatal vector, for any caller that expects it.
  if (return_all) return(invisible(list(fatal = fatal, notes = notes)))
  invisible(fatal)
}

## Single pre-flight guard over the whole workbook: gating_template, Info and Layout_Plate, plus
## the FCS tree the layout points at. Runs BEFORE a single FCS is read, because every check here is
## cheap and the failures it catches otherwise surface minutes later as something obscure -- a
## "missing sample: NA", an -Inf gate floor, an empty flowSet.
##
## Collects EVERY problem and reports them together. Fixing one layout mistake per run, each time
## paying the gating cost to find the next, is the thing this is meant to avoid.
##
## TWO ENTRY POINTS, one implementation:
##   actaPrevalidateRecords()  structured records -- what a UI renders
##   actaPrevalidate()         the console path; stops with the same message it always has
##
## Records exist because a tick/cross panel needs to show checks that PASSED and checks that were
## SKIPPED, not only the failures a stop() can carry. Each record is
##   id      stable key, so a renderer can style or order by check
##   label   what was checked, in the analyst's terms
##   status  "pass" | "fail" | "warn" | "skip"
##   detail  for a fail, the EXACT string the console message has always used
##   where   the sheet or folder to go and edit
##
## A SKIP IS NOT A PASS. A check whose input was absent reports "skip" and must never be
## rendered as green -- the same rule the report QC section follows.
## `groups` is the actaTitrationGroups() frame -- one row per TITRATED MARKER. A bare character
## vector of Dirnames is still accepted and means what it always did, one titration per folder.
actaPrevalidateRecords <- function(metadata, gating_template, info, groups, fcs_root,
                                   marker_col = NA_character_, channel_col = NA_character_) {
  groups <- .actaAsGroups(groups)
  recs <- list()
  add <- function(id, label, status, detail = NA_character_, where = NA_character_)
    recs[[length(recs) + 1L]] <<- list(id = id, label = label, status = status,
                                       detail = detail, where = where)
  ## Emit one record per failure, or a single pass when a check found nothing wrong. Keeping the
  ## failures as separate records preserves the original one-problem-per-line console output.
  resolve <- function(id, label, fails, where, pass_detail = NA_character_) {
    if (length(fails)) for (f in fails) add(id, label, "fail", f, where)
    else add(id, label, "pass", pass_detail, where)
  }

  LP <- "Layout_Plate sheet"; GT <- "gating_template sheet"; IN <- "Info sheet"

  need <- c("Dirname", "Row", "Column", "PlateID", "StainType", "StainQty")
  miss <- setdiff(need, names(metadata))
  resolve("layout_columns", "Layout_Plate has the required columns",
          if (length(miss)) sprintf("  Layout_Plate is missing required column(s): %s",
                                    paste(miss, collapse = ", ")) else character(0),
          LP, sprintf("all present: %s", paste(need, collapse = ", ")))

  resolve("marker_column", "Layout_Plate names a marker or a channel column",
          if (is.na(marker_col) && is.na(channel_col))
            "  Layout_Plate has neither a marker column (`Markername ($PnS)`) nor `Channel ($PnN)`"
          else character(0), LP,
          if (!is.na(marker_col)) sprintf("using %s", marker_col) else sprintf("using %s", channel_col))

  resolve("dirname_populated", "Layout_Plate has at least one populated Dirname",
          if (!nrow(groups)) "  Layout_Plate has no populated Dirname values -- nothing to titrate"
          else character(0), LP,
          ## Say both counts when they differ: "3 titration(s) across 1 FCS folder(s)" is the line
          ## that tells somebody their combinatorial sheet was understood as combinatorial.
          if (nrow(groups) == length(unique(groups$dirname)))
            sprintf("%d antibody group(s)", nrow(groups))
          else sprintf("%d titration(s) across %d FCS folder(s)", nrow(groups),
                       length(unique(groups$dirname))))

  ## Fix/Perm are read by fixPermLabel() for the export; absent, the run dies at the very end,
  ## after all the gating work.
  miss2 <- setdiff(c("Fix", "Perm"), names(metadata))
  resolve("fix_perm_columns", "Layout_Plate has Fix and Perm",
          if (length(miss2))
            sprintf("  Layout_Plate is missing %s -- the titration export needs %s for Fix_Perm",
                    paste(miss2, collapse = " and "), if (length(miss2) > 1) "both" else "it")
          else character(0), LP)

  ## Combinatorial_group: declarative, so the checks are about the DECLARATION being unambiguous,
  ## not about the data. Absent column skips -- it is optional by design, and every workbook written
  ## before 2_99 lacks it.
  if (!("Combinatorial_group" %in% names(metadata))) {
    add("combinatorial_group", "Layout_Plate Combinatorial_group is unambiguous", "skip",
        "no `Combinatorial_group` column to look in", LP)
  } else if (!nrow(groups)) {
    add("combinatorial_group", "Layout_Plate Combinatorial_group is unambiguous", "skip",
        "no titration groups to check", LP)
  } else {
    .cgs <- lapply(seq_len(nrow(groups)), function(i)
              actaCombinatorialGroup(
                metadata$Combinatorial_group[actaTitrationRowMask(metadata, groups$dirname[i],
                                                                  groups$alias[i])],
                groups$key[i]))
    ## FATAL: one titration was stained in one well, so two numbers for it cannot both be true, and
    ## the export would have to pick one silently.
    resolve("combinatorial_group", "Layout_Plate Combinatorial_group is unambiguous",
            Filter(Negate(is.na), vapply(.cgs, function(x) x$conflict, character(1))), LP,
            {
              .dec <- vapply(.cgs, function(x) isTRUE(x$is_combo), logical(1))
              if (!any(.dec)) "declared on no titration -- no combinatorial run recorded"
              else sprintf("%d of %d titration(s) declared, group(s) %s",
                           sum(.dec), length(.dec),
                           paste(sort(unique(vapply(.cgs[.dec], function(x) x$value,
                                                    character(1)))), collapse = ", "))
            })
    ## WARN, not fail: a non-numeric entry MEANS "no combinatorial run", so it cannot be an error --
    ## but "1 " with a stray character, or the word "yes", reads as a deliberate declaration to the
    ## person who typed it and as silence to the pipeline. Naming the value is the whole point.
    for (i in seq_along(.cgs))
      if (!is.na(.cgs[[i]]$value) && !isTRUE(.cgs[[i]]$is_combo))
        add("combinatorial_group", "Layout_Plate Combinatorial_group is unambiguous", "warn",
            sprintf(paste0("titration '%s' has Combinatorial_group '%s', which is not numeric, so ",
                           "it is recorded as NO combinatorial run. Use a number, or leave it ",
                           "blank."), groups$key[i], .cgs[[i]]$value), LP)
  }

  if (is.null(gating_template) || !nrow(gating_template)) {
    add("gating_template_present", "gating_template is populated", "fail",
        "  gating_template sheet is empty", GT)
    add("gating_template_columns", "gating_template has the required columns", "skip",
        "gating_template is empty, nothing to check", GT)
    add("marker_row", "gating_template has exactly one marker row (alias 'Reagent' or 'Antibody')", "skip",
        "gating_template is empty, nothing to check", GT)
  } else {
    add("gating_template_present", "gating_template is populated", "pass",
        sprintf("%d row(s)", nrow(gating_template)), GT)
    gtNeed <- c("alias", "pop", "parent", "dims", "gating_method")
    gtMiss <- setdiff(gtNeed, names(gating_template))
    resolve("gating_template_columns", "gating_template has the required columns",
            if (length(gtMiss)) sprintf("  gating_template is missing column(s): %s",
                                        paste(gtMiss, collapse = ", ")) else character(0), GT)
    if ("alias" %in% names(gating_template)) {
      mk <- which(tolower(trimws(gating_template$alias)) %in% c("reagent", "antibody"))
      resolve("marker_row", "gating_template has exactly one marker row (alias 'Reagent' or 'Antibody')",
              if (length(mk) != 1)
                sprintf("  gating_template has %d marker rows (alias 'Reagent'/'Antibody') -- exactly one is required",
                        length(mk)) else character(0), GT,
              sprintf("method: %s", if (length(mk) == 1) gating_template$gating_method[mk] else NA_character_))
      resolve("template_pop", "gating_template pop values are supported",
              actaUnsupportedPops(gating_template$pop, gating_template$alias), GT,
              sprintf("%d live row(s)", sum(.actaTemplateLive(gating_template$alias))))
      ## Quadrant rows are reported BEFORE the run, because the alternative is openCyto's
      ## "subscript out of bounds" after a minute of gating -- see actaQuadPopFix().
      .qf <- actaQuadPopFix(gating_template)
      if (length(.qf$notes)) for (n in .qf$notes)
        add("template_quad", "gating_template quadrant rows", "warn", n, GT)
      else if (any(.actaTemplateLive(gating_template$alias) &
                   .actaIsQuadMethod(gating_template$gating_method)))
        add("template_quad", "gating_template quadrant rows", "pass",
            "quadrant row(s) carry a pop openCyto can parse", GT)
      ## Single-dim rows, for the same reason as the quadrant rows above: the failure they cause
      ## lands after the whole tree has been gated, and it reads "subscript out of bounds" rather
      ## than naming the row -- see actaDimsNot2D().
      if ("dims" %in% names(gating_template)) {
        .d2 <- actaDimsNot2D(gating_template$dims, gating_template$alias)
        resolve("template_dims_2d", "gating_template dims name two channels (x, y)", .d2$fatal, GT,
                sprintf("%d live row(s)", sum(.actaTemplateLive(gating_template$alias))))
        for (n in .d2$notes)
          add("template_dims_2d", "gating_template dims name two channels (x, y)", "warn", n, GT)
      } else {
        add("template_dims_2d", "gating_template dims name two channels (x, y)", "skip",
            "no `dims` column to look in", GT)
      }
      ## Both arg columns, same treatment: a typo here is otherwise only discovered by openCyto,
      ## minutes into a run, in a message that names an R object rather than a row of the sheet.
      for (.ac in c("gating_args", "preprocessing_args")) {
        if (.ac %in% names(gating_template))
          resolve(paste0("template_", .ac), sprintf("%s parses as R", .ac),
                  actaArgsNotParseable(gating_template[[.ac]], gating_template$alias, .ac), GT,
                  sprintf("%d live row(s)", sum(.actaTemplateLive(gating_template$alias))))
        else
          add(paste0("template_", .ac), sprintf("%s parses as R", .ac), "skip",
              sprintf("no `%s` column to look in", .ac), GT)
      }
    } else {
      add("marker_row", "gating_template has exactly one marker row (alias 'Reagent' or 'Antibody')", "skip",
          "no `alias` column to look in", GT)
      add("template_pop", "gating_template pop values are supported", "skip",
          "no `alias` column to look in", GT)
      add("template_dims_2d", "gating_template dims name two channels (x, y)", "skip",
          "no `alias` column to look in", GT)
    }
  }

  ## ELN_Name / ELN_ID / Operator are LAYOUT columns -- per well, so a run can legitimately carry more
  ## than one operator. They used to be required in the Info sheet as well, which meant one fact in
  ## two places with no rule for which wins; the pipeline only ever read the layout copy.
  elnMiss <- setdiff(c("ELN_Name", "ELN_ID", "Operator"), names(metadata))
  resolve("eln_columns", "Layout_Plate has ELN_Name, ELN_ID and Operator",
          if (length(elnMiss)) sprintf("  Layout_Plate is missing column(s): %s",
                                       paste(elnMiss, collapse = ", ")) else character(0), LP)
  ## The Info sheet is now OPTIONAL: everything on it has a documented default (no compensation, no
  ## downsampling, exports off, dashboard/export dirs unset). Absent, a run still works -- so this is
  ## a warning about defaults being taken, not a failure.
  if (is.null(info) || !nrow(info))
    add("info_present", "Info sheet is populated", "warn",
        "  no Info sheet -- no compensation, no downsampling, exports off", IN)
  else
    add("info_present", "Info sheet is populated", "pass", NA_character_, IN)

  ## Does each Dirname actually resolve to FCS files? Matched the same way the loader does --
  ## literal, case-insensitive prefix on the relative path -- so this cannot pass while the loader
  ## then finds nothing.
  if (!dir.exists(fcs_root)) {
    add("fcs_root", "FCS folder exists", "fail", sprintf("  FCS folder not found: %s", fcs_root), fcs_root)
    add("fcs_per_dirname", "Every Dirname resolves to FCS files", "skip",
        "no FCS folder to search", fcs_root)
    add("fcs_row_count", "FCS file count matches the layout rows", "skip",
        "no FCS folder to search", fcs_root)
  } else {
    add("fcs_root", "FCS folder exists", "pass", fcs_root, fcs_root)
    dirs <- list.dirs(fcs_root, recursive = FALSE, full.names = FALSE)
    missFcs <- character(0); countBad <- character(0)
    ## FOLDERS ONCE, then row counts per TITRATION. The folder is the Dirname -- it was being looked
    ## up by titration key, so a combinatorial run reported "Dirname 'CD4': no .fcs files" for an
    ## Alias that was never a folder name. Co-titrated markers share one folder, so checking it once
    ## per Dirname also stops the same missing folder being reported once per marker.
    files <- stats::setNames(lapply(unique(groups$dirname),
                                    function(dn) actaFindAntibodyFcs(fcs_root, dn)),
                             unique(groups$dirname))
    for (dn in names(files)) {
      if (!length(files[[dn]]))
        missFcs <- c(missFcs, sprintf("  Dirname '%s': no .fcs files under %s%s", dn, fcs_root,
                                      if (length(dirs)) sprintf(" (subfolders present: %s)",
                                                                paste(head(dirs, 8), collapse = ", ")) else ""))
    }
    for (.g in seq_len(nrow(groups))) {
      hit <- files[[groups$dirname[.g]]]
      if (!length(hit)) next                     ## already reported as a missing folder
      ## Per titration: a combinatorial folder has one layout row per well PER MARKER, so counting
      ## by Dirname would compare one set of files against N sets of rows and fail every time.
      nWell <- sum(actaTitrationRowMask(metadata, groups$dirname[.g], groups$alias[.g]) &
                   !isLayoutValue(metadata$StainType, "Unstained"))
      if (length(hit) != nWell)
        countBad <- c(countBad, sprintf(paste0("  %s: %d FCS file(s) in '%s' but %d non-Unstained ",
                                               "layout row(s) -- they must correspond"),
                                        groups$key[.g], length(hit), groups$dirname[.g], nWell))
    }
    resolve("fcs_per_dirname", "Every Dirname resolves to FCS files", missFcs, fcs_root)
    resolve("fcs_row_count", "FCS file count matches the layout rows", countBad, fcs_root)
  }

  ## WELL-LEVEL correspondence, from the FCS headers. Counting files against rows (above) passes
  ## whenever the totals happen to agree -- which is exactly what a series shifted by one well does.
  ## Reading $WELLID is cheap (read.FCSheader() touches only the TEXT segment, not the events) and it
  ## is the same key annotate_changeParam_pdWrite() matches on, so a mismatch caught here is the one
  ## that would otherwise surface as a dropped well or the opaque "missing sample: NA" when the
  ## Costain ends up with no data.
  if (dir.exists(fcs_root) && all(c("Row", "Column", "StainType") %in% names(metadata))) {
    widBad <- character(0)
    ## Headers read ONCE PER FOLDER, then compared per titration. Co-titrated markers share the
    ## files, so reading per titration would re-read every header N times for no new information.
    widByDir <- stats::setNames(lapply(unique(groups$dirname), function(dn) {
      ff <- actaFindAntibodyFcs(fcs_root, dn)
      if (!length(ff)) return(character(0))
      vapply(file.path(fcs_root, ff), function(f) {
        kw <- tryCatch(flowCore::read.FCSheader(f)[[1]], error = function(e) NULL)
        if (is.null(kw) || !("$WELLID" %in% names(kw))) NA_character_
        else toupper(trimws(kw[["$WELLID"]]))
      }, character(1), USE.NAMES = FALSE)
    }), unique(groups$dirname))
    for (.g in seq_len(nrow(groups))) {
      wid <- widByDir[[groups$dirname[.g]]]
      if (!length(wid)) next
      ## The layout side is per TITRATION: a combinatorial folder lists each well once per marker,
      ## so slicing by Dirname would compare one folder's wells against every marker's rows at once
      ## and report each well as duplicated.
      ab <- groups$key[.g]
      ab <- if (identical(groups$dirname[.g], ab)) sprintf("Dirname '%s'", ab)
            else sprintf("'%s' (Dirname '%s')", ab, groups$dirname[.g])
      g <- metadata[actaTitrationRowMask(metadata, groups$dirname[.g], groups$alias[.g]), ,
                    drop = FALSE]
      ## An Unstained row is EXPECTED to have no file here: the loader excludes anything with
      ## "Unstain" in the file NAME, so that well is legitimately absent from `files`.
      gKeep  <- g[!isLayoutValue(g$StainType, "Unstained"), , drop = FALSE]
      lw     <- toupper(paste0(trimws(as.character(gKeep$Row)), trimws(as.character(gKeep$Column))))
      noFile <- lw[!lw %in% wid]
      noRow  <- wid[!is.na(wid) & !wid %in% lw]
      if (length(noFile))
        widBad <- c(widBad, sprintf("  %s: layout well(s) with no matching FCS $WELLID: %s",
                                    ab, paste(noFile, collapse = ", ")))
      if (length(noRow))
        widBad <- c(widBad, sprintf("  %s: FCS well(s) with no matching layout row: %s",
                                    ab, paste(noRow, collapse = ", ")))
      if (any(is.na(wid)))
        widBad <- c(widBad, sprintf("  %s: %d FCS file(s) carry no $WELLID and cannot be matched to a well",
                                    ab, sum(is.na(wid))))
    }
    resolve("wellid_match", "FCS $WELLID matches the layout wells", widBad, fcs_root)
  } else {
    add("wellid_match", "FCS $WELLID matches the layout wells", "skip",
        if (!dir.exists(fcs_root)) "no FCS folder to read headers from"
        else "Layout_Plate lacks Row / Column / StainType", fcs_root)
  }

  ## Every `dims` token must name something the panel actually has. Checked HERE, from the FCS
  ## headers, because the alternative is finding out from openCyto deep into gating -- after the
  ## layout plots are already written. The script hard-errors on the same rule before gating; this is
  ## the version the operator sees before pressing go. See actaUnresolvedDims().
  if (dir.exists(fcs_root) && !is.null(gating_template) && nrow(gating_template) &&
      all(c("alias", "dims") %in% names(gating_template))) {
    ## Judge against the file with the MOST $PnS markers in each group. An Unstained file commonly
    ## carries none at all (one internal run: 11 of 12 files have them, the Unstained has zero), and
    ## reading the panel off that one would report every marker in the template as missing.
    panelOf <- function(paths) {
      best <- NULL; bestN <- -1L
      for (f in paths) {
        pn <- tryCatch(actaFcsPanel(f), error = function(e) NULL)
        if (is.null(pn) || !nrow(pn)) next
        n <- sum(!is.na(pn$marker) & nzchar(trimws(pn$marker)))
        if (n > bestN) { bestN <- n; best <- pn }
      }
      best
    }
    dimsBad <- character(0)
    ## PER FOLDER. The panel is a property of the files, and co-titrated markers share them, so
    ## reading it per titration would read the same panel N times and report the same finding N
    ## times. `ab` here was a titration key, which is never a folder name in a combinatorial run.
    for (dn in unique(groups$dirname)) {
      files <- actaFindAntibodyFcs(fcs_root, dn)
      if (!length(files)) next
      pn <- panelOf(file.path(fcs_root, files))
      if (is.null(pn)) next
      u <- actaUnresolvedDims(gating_template$dims, gating_template$alias, pn$marker, pn$detector)
      if (length(u)) dimsBad <- c(dimsBad, sprintf("  Dirname '%s':", dn), u)
    }
    resolve("template_dims", "gating_template dims name real markers or channels", dimsBad, GT)
  } else {
    add("template_dims", "gating_template dims name real markers or channels", "skip",
        if (!dir.exists(fcs_root)) "no FCS folder to read a panel from"
        else "gating_template has no alias / dims columns", GT)
  }

  ## Layout-level checks (duplicate StainQty, exactly one Costain) folded into the same report.
  ## validateLayoutGroups() still emits its own warning() for the notes, so the console behaviour
  ## is unchanged; return_all only hands the same strings back for the record list.
  lg <- validateLayoutGroups(metadata, groups, stop_on_error = FALSE, return_all = TRUE)
  resolve("layout_groups", "Unique StainQty and exactly one Costain per group", lg$fatal, LP)
  for (n in lg$notes) add("unstained_multiple", "Unstained wells per group", "warn", n, LP)

  recs
}

## Console entry point. Unchanged contract: warns via validateLayoutGroups(), stops on any
## failure with the same message, returns invisible(TRUE) otherwise.
actaPrevalidate <- function(metadata, gating_template, info, groups, fcs_root,
                            marker_col = NA_character_, channel_col = NA_character_) {
  recs <- actaPrevalidateRecords(metadata, gating_template, info, groups, fcs_root,
                                 marker_col = marker_col, channel_col = channel_col)
  bad <- vapply(Filter(function(r) r$status == "fail", recs), function(r) r$detail, character(1))
  if (length(bad))
    stop("ACTA pre-flight check failed -- nothing was read. ",
         sprintf("%d problem(s):\n", length(bad)), paste(bad, collapse = "\n"),
         "\n\nFix the workbook (or blank Dirname on rows that should be ignored) and re-run.",
         call. = FALSE)
  invisible(TRUE)
}

## Collapse the layout's `Fix` / `Perm` flags into the single Fix_Perm descriptor written
## to the titration export:
##   FALSE / FALSE -> "No Fix/Perm"
##   TRUE  / FALSE -> "Fix only"
##   TRUE  / TRUE  -> "Fix+Perm"
##   FALSE / TRUE  -> ERROR. Permeabilising without fixing is not a valid combination, so
##                    this is treated as a layout-authoring mistake, not a silent category.
## Accepts the columns as logicals (what read_excel() gives for the template's real Excel
## booleans) or as text ("TRUE"/"FALSE"/"Yes"/"No"/1/0). A blank/NA flag is ALSO an error
## rather than being coerced to FALSE -- this value lands in the exported titration record,
## so an unset flag must be corrected in the layout instead of guessed at here.
## `ids` optionally supplies well/sample labels to name the offending rows.
fixPermLabel <- function(Fix, Perm, ids = NULL) {
  as_flag <- function(x) {
    if (is.logical(x)) return(x)
    v <- toupper(trimws(as.character(x)))
    ifelse(v %in% c("TRUE","T","YES","Y","1"), TRUE,
           ifelse(v %in% c("FALSE","F","NO","N","0"), FALSE, NA))
  }
  f <- as_flag(Fix); p <- as_flag(Perm)
  if (is.null(ids)) ids <- seq_along(f)
  unset <- is.na(f) | is.na(p)
  if (any(unset))
    stop(sprintf("Fix/Perm is blank or unrecognised for %d well(s): %s. Every populated layout row needs both flags set.",
                 sum(unset), paste(ids[unset], collapse = ", ")))
  if (any(!f & p))
    stop(sprintf("Perm is TRUE without Fix, check template (%d well(s): %s)",
                 sum(!f & p), paste(ids[!f & p], collapse = ", ")))
  ifelse(f & p, "Fix+Perm", ifelse(f, "Fix only", "No Fix/Perm"))
}

## Split sample indices into report pages of at most `per_page`, in facet order (ascending
## StainQty, which is how facet_wrap bins them). Exact-fill pages with a ragged last one:
## 13 facets -> 12 + 1, as specified. Returns a list of index vectors; a single chunk when
## nothing needs splitting, so callers can treat paginated and unpaginated identically.
facetPages <- function(stain_volume, per_page = 12L) {
  ord <- order(suppressWarnings(as.numeric(stain_volume)), na.last = TRUE)
  if (length(ord) <= per_page) return(list(ord))
  split(ord, ceiling(seq_along(ord) / per_page))
}

## Caption for a gate-layout plot: the FULL gating provenance for that population, so the
## panel is self-documenting and a reviewer never has to open the workbook to know how the gate
## was drawn. Takes one row of the (already filtered) gating_template and renders
##   Gating method: X || args: ... || collapse: ... || groupBy: ...
##   pp: ... || pp args: ...
## All six fields are returned as ONE unbroken string, "||" separating them. Line breaking is
## the caller's job, via wrapToWidth() -- a hard break here would fire at the same field no
## matter how wide the figure is, which is how the caption ended up wrapping at ~60% of a
## 12-inch device while two facets' worth of space sat empty to its right.
## Empty/NA fields render as an em dash rather than being dropped, so "not set" is visibly
## different from "not shown" -- which matters for the *_costain methods, where a missing
## preprocessing_method silently means an -Inf (no-op) Costain floor.
gateCaption <- function(row, compensation = NULL) {
  g <- function(col) {
    if (!col %in% names(row)) return("\u2014")
    v <- row[[col]][1]
    if (is.null(v) || length(v) == 0 || is.na(v) || !nzchar(trimws(as.character(v)))) "\u2014"
    else trimws(as.character(v))
  }
  cap <- paste0("Gating method: ", g("gating_method"),
         " || args: ",      g("gating_args"),
         " || collapse: ",  g("collapseDataForGating"),
         " || groupBy: ",   g("groupBy"),
         " || pp: ",        g("preprocessing_method"),
         " || pp args: ",   g("preprocessing_args"))
  ## Compensation is a RUN-level setting, not a per-population one, so it goes on its own line
  ## after the gating fields rather than being flowed in among them -- it explains the input the
  ## gate was drawn on, not how the gate was drawn. wrapToWidth() wraps each \n-separated
  ## paragraph independently, so this stays a single line no matter how the gating text wraps.
  if (!is.null(compensation) && nzchar(trimws(compensation)))
    cap <- paste0(cap, "\nCompensation: ", trimws(compensation))
  cap
}

## Greedily wrap `txt` onto as few lines as fit within `frac` of a `fig_width`-inch figure.
##
## Widths are MEASURED, not counted: a character budget (chars = width / 0.5em) misjudges these
## captions badly because they are mostly narrow glyphs -- "||", "=", "'", digits and periods --
## so a count-based wrap either breaks early (wasted space) or overshoots and gets clipped at the
## device edge, and ggplot does not wrap captions itself.
##
## Measured on a THROWAWAY pdf device, deliberately: strwidth() reads the metrics of whatever
## device is current, and this is called mid-build while knitr's device is open. Sizing that
## scratch device to fig_width also keeps the measurement in the same units as the target.
## cex is font_size/12 because pdf() defaults to a 12pt base size; font = 3 is italic, matching
## the caption theme. Any single token longer than the target still gets its own line rather
## than being split mid-word.
## Caption point size scaled to the FIGURE so it always reads as a footnote.
##
## A fixed 10pt looks right on a wide multi-antibody figure and far too loud on a narrow
## single-antibody one -- where it also overran the device: the OQ_Test1 ConcatPlot caption was
## clipped mid-word at the image edge. wrapToWidth() measures real glyph metrics, but only at the
## size it is TOLD, so the size used for wrapping and the size used for rendering have to be the
## same number. Hence one function, called for both.
##
## Anchored so a ~12in figure keeps the historical 10pt, and clamped at both ends: never so small it
## is unreadable, never so large it stops looking like a footnote.
actaCaptionSize <- function(fig_width) {
  if (is.null(fig_width) || !is.finite(fig_width) || fig_width <= 0) return(10)
  max(6.5, min(11, round(10 * fig_width / 12, 1)))
}

wrapToWidth <- function(txt, fig_width, font_size = 10, frac = 0.90, fontface = 3L) {
  if (is.null(fig_width) || !is.finite(fig_width) || fig_width <= 0) return(txt)
  target <- frac * fig_width
  tf <- tempfile(fileext = ".pdf")
  grDevices::pdf(tf, width = fig_width, height = 2)
  on.exit({ grDevices::dev.off(); unlink(tf) }, add = TRUE)
  wOf <- function(s) graphics::strwidth(s, units = "inches", cex = font_size / 12,
                                        font = fontface)
  out <- character(0)
  for (para in strsplit(txt, "\n", fixed = TRUE)[[1]]) {
    line <- ""
    for (tk in strsplit(para, " ", fixed = TRUE)[[1]]) {
      cand <- if (nzchar(line)) paste(line, tk) else tk
      if (!nzchar(line) || wOf(cand) <= target) line <- cand
      else { out <- c(out, line); line <- tk }
    }
    out <- c(out, line)
  }
  paste(out, collapse = "\n")
}

## Facet grid + device size for the per-antibody plots, so a panel keeps the SAME physical
## size and the SAME aspect ratio however many stain volumes a titration has.
##
## The old behaviour fixed ncol=4 and fig.height=8: extra volumes only added ROWS inside a
## canvas that never grew, so panels collapsed vertically (2.8 x 3.4 in at 8 facets down to
## 2.8 x 1.5 in at 16) while axis/stat text stayed at fixed point sizes -- which is what makes
## labels collide. Here BOTH ncol and nrow scale and the device grows with them, so the panel
## geometry is constant by construction.
##
## `ncol` is not simply ceiling(sqrt(n)): it is chosen so the grid's ASPECT RATIO best matches
## the report's text block, because the .Rmd scales each figure to fit that box
## (keepaspectratio). A grid shaped unlike the page wastes the fit and comes out smaller on
## paper, so aspect-matching maximises the on-page panel size for any n. A small
## `empty_penalty` discourages layouts that leave many blank slots.
##
## NOTE the residual limit: the page is finite, so on PAPER a panel still shrinks as n grows
## (2.0 x 2.5 in at 8 facets, 1.4 x 1.8 in at 16) -- it just shrinks UNIFORMLY now, text and
## all, instead of being squashed in one axis. Keeping the on-page size literally constant
## would require paginating the facets across several pages.
##
## Defaults are calibrated so n = 6..8 reproduces today's 4-column, ~12 x 8 in layout.
facetLayout <- function(n, col_w = 2.90, row_h = 3.65, over_w = 0.80, over_h = 1.00,
                        page_w = 9.00, page_h = 5.72, ncol_max = 8, empty_penalty = 0.05,
                        ncol_fixed = NULL) {
  n <- max(1L, as.integer(n))
  ## ncol_fixed: skip the aspect search and use this many columns. Used for REPORT pages,
  ## which are a fixed 4-wide grid so every page has identical panel geometry.
  if (!is.null(ncol_fixed)) {
    nc <- min(n, as.integer(ncol_fixed)); nr <- ceiling(n / nc)
    return(list(ncol = nc, nrow = nr, width = nc * col_w + over_w,
                height = nr * row_h + over_h, pen = NA_real_, facets = n))
  }
  best <- NULL
  for (nc in seq_len(min(n, ncol_max))) {
    nr  <- ceiling(n / nc)
    W   <- nc * col_w + over_w
    H   <- nr * row_h + over_h
    pen <- abs(log((W / H) / (page_w / page_h))) + empty_penalty * (nc * nr - n) / n
    if (is.null(best) || pen < best$pen)
      best <- list(ncol = nc, nrow = nr, width = W, height = H, pen = pen)
  }
  best$facets <- n
  best
}

## Computes adaptive bin count for geom_bin2d based on median event count across
## samples in a given population. Uses sqrt(median_n) heuristic, clamped to
## [min_bins, max_bins] to stay readable at both low and high event counts.
adaptiveBins<-function(gs, pop, min_bins=50, max_bins=300){
  n_events<-sapply(seq_along(gs), function(i){
    nrow(exprs(gh_pop_get_data(gs[[i]], pop)))
  })
  bins<-round(sqrt(median(n_events)))
  return(max(min_bins, min(max_bins, bins)))
}

## Computes percentile-based axis limits (0.5th--99.5th percentile) with 10%
## symmetric padding, plus a square binwidth in data units for geom_bin2d,
## for a given population and pair of channels across ALL samples in a
## GatingSet. Returns list(xlim, ylim, binwidth).
##
## xchannel / ychannel: channel names matching the x / y aes in the plot.
## bins: adaptive bin count (from adaptiveBins) used to size the binwidth.
computePlotParams <- function(gs, pop, xchannel, ychannel, bins) {
  xlim_raw <- c(
    min(sapply(seq_along(gs), function(i)
      quantile(exprs(gh_pop_get_data(gs[[i]], pop))[, xchannel], 0.005))),
    max(sapply(seq_along(gs), function(i)
      quantile(exprs(gh_pop_get_data(gs[[i]], pop))[, xchannel], 0.995)))
  )
  ylim_raw <- c(
    min(sapply(seq_along(gs), function(i)
      quantile(exprs(gh_pop_get_data(gs[[i]], pop))[, ychannel], 0.005))),
    max(sapply(seq_along(gs), function(i)
      quantile(exprs(gh_pop_get_data(gs[[i]], pop))[, ychannel], 0.995)))
  )
  xlim <- xlim_raw + c(-1, 1) * 0.1 * diff(xlim_raw)
  ylim <- ylim_raw + c(-1, 1) * 0.1 * diff(ylim_raw)
  ## Per-axis binwidth: each axis gets ~`bins` bins over its OWN range. A single
  ## square width (min(range)/bins) breaks when the axes are on different scales
  ## -- e.g. a biexp marker (range ~1e3) vs LINEAR scatter (range ~1e6): the tiny
  ## marker-derived width applied to the huge scatter axis yields thousands of
  ## near-empty bins (sparse, dot-like plots). Independent widths fix that.
  binwidth <- c(diff(xlim), diff(ylim)) / bins
  list(xlim = xlim, ylim = ylim, binwidth = binwidth)
}

## Drop-in replacement for geom_hex using rectangular 2D bins with a
## log10-scaled flow cytometry colour ramp (dark blue -> cyan -> green ->
## yellow -> red). Returns a list so it adds cleanly to a ggplot/ggcyto chain.
## Empty bins (count = 0, NA after log10) are rendered as dark navy.
##
## Use bins= (scalar) for most plots. For plots where x and y data ranges
## differ substantially (e.g. DUMP gate), pass binwidth= instead -- a length-2
## vector c(x_width, y_width) in data units. This keeps bins visually square
## and prevents horizontal stripe artefacts caused by a narrow y-range.
## `xlim`/`ylim`: BIN ONLY THE VISIBLE WINDOW. Borrowed from URSULA's computePlotParams, which
## documents the same failure: "geom_bin2d bins over the raw data range (incl. the extreme off-view
## tails the P0.5-P99.5 xlim trims) ... a few off-view events inflate full_range, so dense/bins
## implied thousands of bins".
##
## The limits ACTA computes already trim to P0.5-P99.5, but geom_bin2d never sees them -- it sizes its
## grid from the DATA, so the off-view tails multiply the cell count while the binwidth stays sized
## for the visible span. Each cell then holds about one event, and a log10 fill ramp renders that as
## one flat colour: the density disappears. It bites hardest when BOTH axes are fluorescence, because
## both have long biexp tails -- which is exactly the case that broke (the CD4/CD8 quadrant plot).
## With one linear scatter axis the tails are bounded by the instrument range, which is why every
## other layout plot survived it.
##
## Passing explicit `breaks` pins the grid to the window instead. Events outside fall in no bin and
## are dropped, which changes nothing visible -- they were already clipped by the axis limits.
flow_bin2d<-function(bins=NULL, binwidth=NULL, xlim=NULL, ylim=NULL){
  bin_args<-if(!is.null(binwidth)) list(binwidth=binwidth) else list(bins=bins)
  if (!is.null(binwidth) && !is.null(xlim) && !is.null(ylim) &&
      all(is.finite(c(xlim, ylim))) && diff(range(xlim)) > 0 && diff(range(ylim)) > 0) {
    ## seq() to the window's far edge, +1 width so the last partial bin is not clipped short.
    .br<-function(l, w) seq(min(l), max(l) + w, by = w)
    bin_args<-list(breaks = list(x = .br(xlim, binwidth[1]), y = .br(ylim, binwidth[2])))
  }
  list(
    do.call(geom_bin2d, bin_args),
    scale_fill_gradientn(
      colours  = c("navy","#1E90FF","cyan","#32CD32","yellow","orange","red"),
      trans    = "log10",
      na.value = "navy",
      name     = "Count"
    )
  )
}

## Resolve an openCyto template `dims` string (comma-separated marker and/or channel
## names) to actual channel names, using the boundary Channel<->Target map. A token
## matching a Target (marker name, e.g. "Viability") returns its Channel; a token that
## is already a channel name (e.g. "FSC-A") is returned as-is. Used to derive plot
## channels for each gating_template population without hardcoding them.

## Resolve THIS antibody's titrated channel ($PnN) from its layout row.
##
## Two ways in, checked in this order:
##   Channel populated -> matched against colnames() ($PnN) EXACTLY (case-insensitively)
##   otherwise Dim     -> matched against markernames() ($PnS), via boundary$Target
##
## `Dim` has always been a MARKERNAME field despite its channel-sounding name -- every lookup ran
## it against boundary$Target, which is markernames(). That is kept exactly as it was, so no
## existing layout changes behaviour. `Channel` is the new escape hatch for panels where the
## markername is missing, duplicated, or simply not how the analyst thinks about the parameter --
## common on conventional flow, where $PnS may never have been filled in.
##
## BOTH values are matched EXACTLY and LITERALLY, ignoring case and surrounding whitespace. No
## regex and no substring: `cparp`, `cParp` and `CPARP` all find the marker cPARP, and nothing
## else does.
##
## Exact rather than partial is the safer default for both columns. Marker and detector names
## routinely share prefixes -- CD1 against CD14 and CD19, `PE-A` against `PE-Alexa Fluor 700-A` --
## and a substring rule silently titrates the wrong parameter, which is invisible in the finished
## report. Literal rather than regex means a name containing regex metacharacters (`CD8+`,
## `CD19 (BV421)`) is typed as-is, with nothing to escape.
## Exact, case- and whitespace-insensitive equality against a pool of names. No regex, so a value
## containing metacharacters ("CD8+", "PE-A") is compared as typed.
.actaSame <- function(pool, value) {
  !is.na(pool) & toupper(trimws(as.character(pool))) == toupper(trimws(as.character(value)))
}

resolveMarkerChannel <- function(dim_value, channel_value, boundary, data_channels, group = "",
                                 dim_col = "Markername") {
  clean <- function(x) { x <- trimws(as.character(x)); x <- x[!is.na(x) & nzchar(x)]; unique(x) }
  ch  <- clean(channel_value)
  dm  <- clean(dim_value)
  ctx <- if (nzchar(group)) sprintf(" (antibody '%s')", group) else ""

  if (length(ch) > 1)
    stop(sprintf("Layout_Plate gives %d different Channel values%s: %s. One antibody group titrates one channel.",
                 length(ch), ctx, paste(ch, collapse = ", ")), call. = FALSE)
  if (length(ch) == 1) {
    hit <- data_channels[.actaSame(data_channels, ch)]
    if (!length(hit))
      stop(sprintf(paste0("Layout_Plate Channel '%s'%s does not name a channel in the data. ",
                          "Channel is matched exactly (case-insensitively) against colnames() ",
                          "($PnN). Available: %s"),
                   ch, ctx, paste(data_channels, collapse = ", ")), call. = FALSE)
    if (length(hit) > 1)
      stop(sprintf("Layout_Plate Channel '%s'%s matches %d channels (%s) -- the panel has duplicate names.",
                   ch, ctx, length(hit), paste(hit, collapse = ", ")), call. = FALSE)
    return(hit[1])
  }

  if (!length(dm))
    stop(sprintf(paste0("Layout_Plate has neither %s nor Channel populated%s. Set %s to the ",
                        "marker name ($PnS) or Channel to the detector name ($PnN)."),
                 dim_col, ctx, dim_col),
         call. = FALSE)
  if (length(dm) > 1)
    stop(sprintf("Layout_Plate gives %d different %s values%s: %s. One antibody group titrates one marker.",
                 length(dm), dim_col, ctx, paste(dm, collapse = ", ")), call. = FALSE)
  ok  <- .actaSame(boundary$Target, dm)
  hit <- boundary$Channel[ok]
  if (!length(hit))
    stop(sprintf(paste0("Could not resolve a channel for %s '%s'%s against the panel markers. ",
                        "%s is matched exactly (case-insensitively) against markernames() ($PnS): ",
                        "%s. If this panel carries no usable marker names, put the detector name ",
                        "in the Channel column instead."),
                 dim_col, dm, ctx, dim_col, paste(boundary$Target, collapse = ", ")), call. = FALSE)
  if (length(hit) > 1)
    ## With exact matching this can only happen if the PANEL itself repeats a marker name -- a data
    ## problem the analyst has to resolve, so it is fatal rather than a silent [1] pick.
    stop(sprintf(paste0("%s '%s'%s matches %d markers (%s) -- the panel repeats that marker name. ",
                        "Use the Channel column to name the detector instead."),
                 dim_col, dm, ctx, length(hit), paste(boundary$Target[ok], collapse = ", ")),
         call. = FALSE)
  hit[1]
}

## ======================= TEMPLATE PRE-FLIGHT =======================
## Two checks that used to be discovered only by openCyto, deep into gating, AFTER plots had already
## been written -- at which point the run had produced partial output and (with report = TRUE) the
## failure was indistinguishable from a LaTeX error. Both are pure functions over the template text so
## the script can hard-error before gating AND the app can show them before the operator presses go.

## Dims that name nothing in the data. resolveDimsToChannels() matches a token case-insensitively
## against boundary$Target and, FAILING THAT, PASSES THE TOKEN THROUGH UNCHANGED as if it were a
## channel name. That pass-through is the whole problem: a typo or a marker absent from this panel
## reaches openCyto as a channel, and surfaces much later as "can't find Viability" -- reported by a
## colleague on 2026-08-18, by which point the plots were on disk and the app said the export was
## ready. Mirrors that matching rule exactly; a token is fine if it is a $PnS marker OR a channel.
## BOTH checkers skip rows whose alias starts with "#". The script filters those out before it gets
## here, but the app reads the RAW sheet -- and a commented-out row keeps its `pop` and `dims` cells, so
## without this the disabled `#CD4_CD8` quadrant row in a real workbook would block the run it was
## disabled to allow. Reported row numbers stay as SHEET rows, so they match what the operator sees.
.actaTemplateLive <- function(aliases) {
  a <- trimws(as.character(aliases))
  !is.na(a) & nzchar(a) & !startsWith(a, "#")
}

actaUnresolvedDims <- function(dims, aliases, markers, channels) {
  up <- function(x) toupper(trimws(as.character(x)))
  ok <- c(up(markers), up(channels))
  live <- .actaTemplateLive(aliases)
  out <- character(0)
  for (i in seq_along(dims)) {
    if (!isTRUE(live[i])) next
    d <- trimws(as.character(dims[i]))
    if (is.na(d) || !nzchar(d)) next
    toks <- trimws(strsplit(d, ",")[[1]]); toks <- toks[nzchar(toks)]
    bad  <- toks[!(up(toks) %in% ok)]
    if (length(bad))
      out <- c(out, sprintf("  row %d (alias '%s', dims '%s'): %s matches no marker and no channel",
                            i, as.character(aliases[i])[1], d,
                            paste0("'", bad, "'", collapse = ", ")))
  }
  out
}

## Every scaffolded gate is DRAWN as a 2D panel, so its `dims` must name two channels. A row with
## one dim reached computePlotParams() as (x, NA) and died with "subscript out of bounds" -- AFTER
## the whole tree had been gated and, in the app, after the layout plots were on disk. Reported by
## the user on 2026-08-26 on a template whose viability row read `dims = LIVEDEAD`, which is
## perfectly ordinary openCyto: a viability gate on one channel is how anybody would write it.
##
## A 1D GATE STAYS 1D. The gating methods read `channels[1]` only (gate_flowmeans opens with
## `chnl <- channels[1]` and returns a rectangleGate on that one channel), so naming a scatter
## channel second changes the PLOT and not the gate, not the statistics and not the tree. That is
## what makes this a fix in the sheet rather than a redesign here -- and why the message says so.
##
## The MARKER row is the exception, and in the other direction: the script prepends this group's
## resolved marker channel to it (`gt_this$dims[mr] <- paste(markerChannel, yDim, sep = ", ")`) and
## keeps only the FIRST sheet-authored token as y, so it needs exactly ONE -- a second is silently
## dropped. Give this the RAW sheet: run it after that rewrite and the marker row already looks 2D.
##
## Returns list(fatal, notes), the shape actaQuadPopFix() and validateLayoutGroups() use, so a
## caller can stop on the first and message the second.
## Plot dimensions for a gating_template row. A row may legitimately name ONE dimension -- a
## viability gate on a single channel is how anybody writes it in openCyto -- but every layout plot
## is a 2D panel, and computePlotParams() handed a NA second channel died with "subscript out of
## bounds" AFTER the whole tree had been gated (reported 2026-08-27).
##
## The second axis is supplied HERE, from the instrument: pickForwardScatter(), the same channel the
## marker pseudocolor already uses for its y. Never the literal "FSC-A", which does not exist on a
## ZE5 (FS00-A) or a Guava (FSC-HLin).
##
## DISPLAY ONLY. The gate is untouched: the gating methods read `channels[1]` and nothing else
## (gate_flowmeans opens with `chnl <- channels[1]` and returns a rectangleGate on that one channel),
## so this changes the panel and not the statistics, the tree or the verdicts.
##
## Chosen over rendering a 1D density panel: that would mean a parallel branch through
## makeGateLayoutGreatAgain(), which EVERY layout plot goes through, and its biexp axis handling is
## the most delicate code here. A display axis keeps all layout plots one shape.
##
## If the single dim IS the forward scatter, pairing it with itself is a degenerate plot, so the next
## scatter channel is used; with no second scatter at all, that is a hard error naming the row.
actaPlotDims <- function(chans, channels, alias = NA_character_) {
  chans <- chans[!is.na(chans) & nzchar(chans)]
  if (length(chans) >= 2L) return(unname(chans[1:2]))
  if (!length(chans)) return(character(0))
  y <- pickForwardScatter(channels)
  if (identical(toupper(y), toupper(chans[[1]]))) {
    alt <- setdiff(channels[isScatterChannel(channels)], chans[[1]])
    if (!length(alt))
      stop(sprintf(paste0("actaPlotDims: row%s names only '%s', which is also the only scatter ",
                          "channel, so it cannot supply the display y axis too. Give this row a ",
                          "second dim."),
                   if (is.na(alias)) "" else sprintf(" (alias '%s')", alias), chans[[1]]),
           call. = FALSE)
    y <- alt[[1]]
  }
  unname(c(chans[[1]], y))
}

## Are `gating_args` / `preprocessing_args` VALID R? Returns a character vector of problems, one
## per offending row, empty when everything parses.
##
## WHY THIS EXISTS. Building the DATASET-1 instructions the args were written `cluster=neg`
## instead of `cluster="neg"` (a shell ate the apostrophes). The workbook opened fine, pre-flight
## passed 17/17, and the run died ~75 s in -- AFTER Cells, Single_cells and Live had all gated --
## with openCyto's `object 'neg' not found`. Nothing in the project checked that the args were
## even parseable, so the cost of a typo was a minute of gating and an error message pointing at R
## internals rather than at a cell in the sheet.
##
## openCyto splices these into a call, so the test is exactly that: wrap in `list(...)` and PARSE.
## Parse only -- never evaluate. Evaluating would run whatever the sheet says at pre-flight time,
## which is both a side-effect risk and wrong, since names like `neg` are only bound inside the
## plugin. `cluster=neg` PARSES (it is a symbol), so the parse test alone would not have caught
## the original fault; the eval-free NAME CHECK below is what catches it: every bare symbol used as
## a value is reported, because a value in these args is always a literal, never a variable.
actaArgsNotParseable <- function(args, aliases, column = "gating_args") {
  live <- .actaTemplateLive(aliases)
  out  <- character(0)
  for (i in which(live)) {
    a <- trimws(as.character(args[[i]]))
    if (is.na(a) || !nzchar(a)) next
    al <- trimws(as.character(aliases[[i]]))
    e  <- tryCatch(parse(text = paste0("list(", a, ")")), error = function(err) err)
    if (inherits(e, "error")) {
      ## R's parse errors carry the offending line and a caret on the next; collapsed to one line
      ## so a console list of problems stays one problem per line.
      out <- c(out, sprintf("row '%s': %s does not parse as R -- %s. Value: %s",
                            al, column, gsub("\\s+", " ", trimws(conditionMessage(e))), a))
      next
    }
    ## A doubled or trailing comma PARSES (R allows empty arguments) but is never what was meant,
    ## and openCyto passes it straight into a call where it becomes a missing-argument error.
    if (grepl(",\\s*,", a) || grepl(",\\s*$", a))
      out <- c(out, sprintf("row '%s': %s has an empty argument -- a doubled or trailing comma. Value: %s",
                            al, column, a))
    ## A CALL that is not on the allowlist. This is the security half of the check (2_94 review
    ## H-1, widened): openCyto parses these cells without evaluating, then `do.call`s the plugin
    ## with the resulting language objects, so R evaluates each one as the plugin reads it. A cell
    ## reading `quantile=(function(){...})()` therefore runs, mid-gating, as the analyst. Refusing
    ## it HERE -- before any data is read -- is the only place we control, since we cannot stop
    ## openCyto evaluating what it is handed. See ACTA_ARG_CALL_ALLOW for why the list is three
    ## names long.
    nogo <- actaDisallowedCalls(e)
    if (length(nogo))
      out <- c(out, sprintf(paste0("row '%s': %s calls %s. Argument cells may contain numbers, ",
                                   "quoted text, TRUE/FALSE/NA/NULL, and %s -- nothing that runs. ",
                                   "This cell would be EXECUTED during gating. Value: %s"),
                            al, column, paste(sprintf("`%s`", nogo), collapse = ", "),
                            paste(sprintf("`%s`", ACTA_ARG_CALL_ALLOW), collapse = "/"), a))
    ## A bare symbol where a value belongs. `quantile=0.99` is a literal; `cluster=neg` is a name
    ## openCyto will look up and fail on. all.vars() is used rather than a hand-rolled walk of the
    ## parse tree: it already ignores function names, so `min=c(x,y)` stays legal, and it handles
    ## the empty argument above without the walk erroring on R's empty symbol.
    bare <- setdiff(all.vars(e), c("T", "F", "TRUE", "FALSE", "NA", "NULL", "Inf", "NaN"))
    if (length(bare))
      out <- c(out, sprintf(paste0("row '%s': %s uses the bare name%s %s as a value. Quote it ",
                                   "(e.g. cluster=\"neg\", not cluster=neg) -- openCyto will ",
                                   "look it up as a variable and fail after gating has started. ",
                                   "Value: %s"),
                            al, column, if (length(bare) > 1L) "s" else "",
                            paste(sprintf("`%s`", bare), collapse = ", "), a))
  }
  out
}

actaDimsNot2D <- function(dims, aliases) {
  if (!length(dims)) return(list(fatal = character(0), notes = character(0)))
  live <- .actaTemplateLive(aliases)
  al   <- tolower(trimws(as.character(aliases)))
  tok  <- function(i) {
    t <- trimws(strsplit(trimws(as.character(dims[i])), ",")[[1]])
    t[!is.na(t) & nzchar(t)]
  }
  ## The example channel is taken from the TEMPLATE, never from a literal "FSC-A": on a ZE5 (FS00-A)
  ## or a Guava (FSC-HLin) that literal names nothing, and telling somebody to write a channel their
  ## instrument does not have is worse than naming none. A scatter token already in the sheet is by
  ## definition one this panel has -- same reasoning as the derived `scatterChannels` in the script.
  eg <- {
    all_toks <- unlist(lapply(which(live), tok), use.names = FALSE)
    sc <- all_toks[isScatterChannel(all_toks)]
    if (length(sc)) sc[[1]] else "FSC-A"
  }
  fatal <- character(0); notes <- character(0)
  for (i in seq_along(dims)) {
    if (!isTRUE(live[i])) next
    d <- trimws(as.character(dims[i]))
    ## Empty dims is a DIFFERENT fault (the row names no channel at all) and openCyto reports it
    ## clearly; saying "add a second dim" to a row with no first one would only mislead.
    if (is.na(d) || !nzchar(d)) next
    n <- length(tok(i))
    if (al[i] %in% c("reagent", "antibody")) {
      if (n > 1L)
        notes <- c(notes, sprintf(paste0(
          "  row %d (alias '%s', dims '%s'): the marker row takes ONE dim. ACTA puts this group's ",
          "marker channel first and keeps '%s' as y; anything after it is dropped."),
          i, as.character(aliases[i])[1], d, tok(i)[1]))
      next
    }
    if (n == 1L)
      ## A NOTE, not a failure, since 2026-08-27: actaPlotDims() supplies the instrument's forward
      ## scatter as the display y axis, so this runs. Still worth saying, because the operator
      ## should know which axis they are looking at and that they can choose it themselves.
      notes <- c(notes, sprintf(paste0(
        "  row %d (alias '%s', dims '%s'): one dimension. The layout panel is 2D, so the ",
        "instrument's forward-scatter channel will be used as the display y axis. The gate is ",
        "unaffected -- the gating method reads the first dim only. Name it yourself to choose ",
        "which: dims = \"%s, %s\"."),
        i, as.character(aliases[i])[1], d, tok(i)[1], eg))
    else if (n > 2L)
      notes <- c(notes, sprintf(paste0(
        "  row %d (alias '%s', dims '%s'): %d dims. The plot uses the first two; the gate uses as ",
        "many as its gating method supports."),
        i, as.character(aliases[i])[1], d, n))
  }
  list(fatal = fatal, notes = notes)
}


## `pop` values ACTA can represent. Every form openCyto's own CSV template supports is allowed --
## ACTA is meant to be a front end to the gating template, not a subset of it:
##   "+" / "-"                one node, named by the alias
##   "+/-"                    two nodes, <alias>+ and <alias>-  (the marker row's shape)
##   "+/-+/-"                 a quadrant: openCyto expands it to six named populations, and those
##                            ARE parentable -- see actaTemplateRowNodes() for the expansion
##   "++" "+-" "-+" "--"      a single quadrant via refGate, named by the alias
##   "*"                      as many populations as the gating function returns; names unknown
##                            ahead of gating, so nothing is scaffolded for it (openCyto documents
##                            this form as terminal -- not usable as a parent)
## The list exists to catch TYPOS, which would otherwise reach openCyto and be reported obscurely.
ACTA_POP_SUPPORTED <- c("+", "-", "+/-", "+/-+/-", "++", "+-", "-+", "--", "*")
## ---------------------------------------------------------------------------------------------
## QUADRANT ROWS: openCyto reads `pop`, and IGNORES `alias`.
##
## Reported by the user on 2026-08-20, who had written the row the way the openCyto documentation
## reads for naming several populations at once:
##
##     alias = "CD4-CD8+,CD4+CD8+,CD4+CD8-,CD4-CD8-"   pop = "*"   gating_method = gate_quad_tmix
##
## That combination is valid openCyto for a method that returns SEVERAL gates to be matched against
## the alias list -- but not for the four quadrant methods. For those, openCyto:
##   * prints "alias '...' is ignored since pop names are auto-generated from 'dims'" and replaces the
##     alias with "*"; and
##   * PARSES the `pop` string itself, in .gating_gtMethod:
##         pops <- strsplit(strsplit(gsub("([\+-])([^/$])", "\1&\2", pop), "&")[[1]], "/")
##         pops <- paste0(rep(pops[[1]], each = length(pops[[2]])), pops[[2]])
##     which needs TWO groups, one per dimension. `pop = "*"` yields one, so `pops[[2]]` fails with
##     "subscript out of bounds" -- from deep inside openCyto, AFTER every gate has been computed.
##     Reproduced on a copy of OQ_Test1: about a minute of gating, then that message and nothing else.
##
## So the row cannot work as written, and the fix is one cell: pop must carry the quadrant spec. Since
## that is the ONLY thing a quadrant method can mean, ACTA rewrites it rather than refusing the run --
## and says so, in the run log and in the instructions check, because a silent rewrite of somebody's
## template would be worse than the error it replaces.
ACTA_QUAD_METHODS <- c("gate_quad_tmix", "gate_quad_sequential", "quadGate.tmix", "quadGate.seq")

.actaIsQuadMethod <- function(m)
  tolower(trimws(as.character(m))) %in% tolower(ACTA_QUAD_METHODS)

## Would openCyto's quadrant path parse this pop value? Mirrors the two lines quoted above rather
## than guessing: anything that does not split into two "/"-groups is what blows up.
.actaQuadPopOK <- function(pop) {
  p <- trimws(as.character(pop))[1]
  if (is.na(p) || !nzchar(p)) return(FALSE)
  parts <- strsplit(strsplit(gsub("([\\+-])([^/$])", "\\1&\\2", p), "&")[[1]], "/")
  length(parts) >= 2 && all(vapply(parts[1:2], function(z) length(z) && all(nzchar(z)), TRUE))
}

## Returns the template with any unusable quadrant `pop` replaced by the full spec, plus one note per
## row changed. Non-quadrant methods are never touched: `pop = "*"` is legitimate for them.
actaQuadPopFix <- function(gating_template) {
  out <- list(template = gating_template, notes = character(0))
  if (is.null(gating_template) || !nrow(gating_template)) return(out)
  if (!all(c("alias", "pop", "gating_method") %in% names(gating_template))) return(out)
  live <- .actaTemplateLive(gating_template$alias)
  for (i in seq_len(nrow(gating_template))) {
    if (!isTRUE(live[i]) || !.actaIsQuadMethod(gating_template$gating_method[i])) next
    if (.actaQuadPopOK(gating_template$pop[i])) next
    was <- trimws(as.character(gating_template$pop[i]))
    out$template$pop[i] <- "+/-+/-"
    out$notes <- c(out$notes, sprintf(paste0(
      "gating_template row %d (alias '%s', method %s): pop '%s' cannot drive a quadrant gate -- ",
      "openCyto parses `pop` for these methods and needs both dimensions, and it IGNORES `alias`. ",
      "Read as pop '+/-+/-', which gates all four quadrants and names them from dims (%s)."),
      i, trimws(as.character(gating_template$alias[i])),
      trimws(as.character(gating_template$gating_method[i])),
      if (is.na(was) || !nzchar(was)) "" else was,
      trimws(as.character(if ("dims" %in% names(gating_template)) gating_template$dims[i] else NA))))
  }
  out
}

actaUnsupportedPops <- function(pops, aliases) {
  p <- trimws(as.character(pops))
  bad <- which(.actaTemplateLive(aliases) & !is.na(p) & nzchar(p) & !(p %in% ACTA_POP_SUPPORTED))
  if (!length(bad)) return(character(0))
  sprintf("  row %d (alias '%s'): pop = '%s' is not a valid openCyto pop -- expected one of %s.",
          bad, as.character(aliases)[bad], p[bad],
          paste(sprintf("'%s'", ACTA_POP_SUPPORTED), collapse = ", "))
}

resolveDimsToChannels <- function(dims_str, boundary){
  toks <- trimws(strsplit(dims_str, ",")[[1]])
  vapply(toks, function(tk){
    hit <- boundary$Channel[toupper(boundary$Target) == toupper(tk)]
    if (length(hit) >= 1) hit[1] else tk
  }, character(1), USE.NAMES = FALSE)
}

## Build a faceted (per StainQty) gate-layout plot for one population, from a
## scaffold entry `sc` (list with $alias, $parent, $channels, $index, $params as
## produced in the script from the gating_template). titlePrefix is the antibody
## alias, abChannel the titrated marker channel, methodLabel this population's gating
## method provenance (shown verbatim in the CAPTION -- build it with gateCaption()), and
## `label` the per-antibody reagent/test-material
## descriptor appended to the subtitle after the parent pop ("|"-separated, same
## convention as the histogram/pseudocolor plots).
## `benchlingID` and `operator` are stamped onto the TITLE in that order, matching the
## histogram/pseudocolor titles: "<alias> <channel> : <pop> | <ELN_ID> | <operator>". Either
## may be omitted, in which case its separator is omitted too.
## layout_ncol sets the facet_wrap column count (default 4). caption_width is the width IN
## INCHES of the device this plot will be drawn on -- the caption is wrapped to 90% of it by
## wrapToWidth(), so it fills the figure instead of breaking at a fixed field. Pass the `width`
## from facetLayout(); NULL leaves the caption unwrapped. Returns a ggcyto object.
## One optional, separator-carrying segment of a plot title or facet strip. Returns "" for
## nothing-to-say -- absent metadata must not print as "Clone " or "Clone NA", which reads as a
## measurement that failed rather than as a column this workbook does not fill.
##
## Also the guard that keeps a title SCALAR: it was written for an un-collapsed unique() of
## ELN_ID, where a length-2 value silently rendered one plot per element and duplicated the saved
## PNGs. Anything vector-shaped is joined, never recycled.
##
## `sep` because the two consumers punctuate differently and both are deliberate: plot titles
## chain on " | ", the facet strip on " || " to match the strip's own separators.
actaTitleStamp <- function(x, prefix = "", sep = " | ") {
  x <- trimws(as.character(x)); x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) "" else paste0(sep, prefix, paste(x, collapse = ", "))
}

makeGateLayoutGreatAgain <- function(gs, sc, titlePrefix, abChannel, methodLabel, label = "",
                               benchlingID = "", operator = "", clone = "", trans = NULL,
                               maxValue = NULL, layout_ncol = 4, caption_width = NULL){
  paths <- gs_get_pop_paths(gs)
  ## Built first, then the biexp axes are added -- see addBiexpAxes() on why they cannot be
  ## spliced into the `+` chain as a list. This population's dims may be scatter, fluorescence
  ## or a mix, so only the fluorescence axes get inverted.
  ## See actaTitleStamp() for what this tolerates and why.
  .stamp <- actaTitleStamp
  ## Same limits must feed ggcyto_par_set and the axis breaks, or the ticks and the panel
  ## disagree -- so clamp once, here.
  .xl <- biexpFloorLimits(sc$params$xlim, sc$channels[1], trans)
  .yl <- biexpFloorLimits(sc$params$ylim, sc$channels[2], trans)
  ## `sc$index` may name SEVERAL populations: a quadrant row is one gate that yields four, and four
  ## separate single-population panels of the same 2D plot is not what a quadrant looks like anywhere
  ## else. So the gates are added one layer at a time in a loop -- ggcyto's `+` SILENTLY IGNORES a
  ## list of layers (see addBiexpAxes()), so they cannot be spliced into the chain -- and before
  ## geom_stats, which keeps the labels drawn on top exactly as they were for a single gate.
  p <- ggcyto(gs, aes(x = .data[[sc$channels[1]]], y = .data[[sc$channels[2]]]), subset = sc$parent) +
    ## The plotted window is handed to flow_bin2d so the grid covers it instead of the full data
    ## range -- see flow_bin2d(). .xl/.yl are the same clamped limits ggcyto_par_set gets below, so
    ## the bins and the axes cannot disagree.
    flow_bin2d(binwidth = sc$params$binwidth, xlim = .xl, ylim = .yl)
  for (.nd in paths[sc$index]) p <- p + geom_gate(gs_pop_get_gate(gs, .nd))
  ## Computed HERE, not inside the `+` chain below: `p <- p + .chanDerived <- ...` PARSES, because
  ## `<-` is right-associative and lower-precedence than `+`, and then fails at evaluation with the
  ## rest of the chain silently detached. Parsing clean is not the same as being in the right place.
  .chanDerived <- length(sc$index) > 1L &&
    all(vapply(basename(paths[sc$index]), function(nm)
          all(vapply(sc$channels, function(ch) grepl(ch, nm, fixed = TRUE), logical(1))),
        logical(1)))
  p <- p +
    ## size: geom_stats has no `size` default of its own -- it forwards to geom_label, whose
    ## ggplot2 default is 3.88. Stated explicitly at that default: these labels were briefly
    ## dropped to 2.88 and put back, so pin the value rather than leave it implicit.
    ## "count" alongside the percent: a percentage alone cannot tell you whether a population is
    ## 45% of 20,000 events or 45% of 30, and the second case makes every downstream statistic
    ## meaningless. The min_events QC asserts the same thing numerically; this is the version you
    ## can see. Deliberately NOT added to the histogram, where the label sits over the curve.
    ## gate_name is dropped for CHANNEL-DERIVED names, not for "more than one gate".
    ##
    ## On a quadrant the names are openCyto's channel-derived ones -- "BUV496-A-Alexa Fluor 532-A+"
    ## and friends -- and four of them overflow their quadrants and each other into unreadable
    ## overlap; the position in the quadrant already says which population is which, so the name
    ## earns nothing there.
    ##
    ## But the test used to be `length(sc$index) > 1L`, and that silenced the +/- PAIR too, which
    ## was never the case it was written for. USER-REPORTED 2026-08-28: a `CD3 +/-` row drew
    ## "37.3% / 25583" and "62.7% / 43009" with nothing on the panel saying which side was CD3+ --
    ## and for a pair the names are the short alias-derived "CD3+"/"CD3-", i.e. exactly the label
    ## that was missing. Count was never the problem; NAME LENGTH was, so test the cause: a name
    ## that embeds both channel names is openCyto's channel-derived kind and is dropped, anything
    ## else is kept. A single gate is unaffected either way.
    geom_stats(paths[sc$index],
               type = if (.chanDerived) c("percent","count")
                      else c("percent","count","gate_name"),
               fill = "white", color = "black", alpha = 0.9, size = 3.88) +
    theme_bw() +
    ggcyto_par_set(limits = list(x = .xl, y = .yl)) +
    labs(title = paste0(paste(titlePrefix, abChannel, ":", sc$alias),
                        .stamp(benchlingID), .stamp(operator),
                        actaTitleStamp(clone, "Clone ")),
         subtitle = paste0("Parent pop: ", sc$parent, if (nzchar(label)) paste0(" | ", label) else ""),
         caption  = wrapToWidth(methodLabel, caption_width)) +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1, face = "plain", size = 12),
          strip.text  = element_text(size = 12, face = "bold", color = "darkblue"),
          plot.subtitle = element_text(face = "italic", size = 12),
          plot.caption  = element_text(face = "italic", size = 10, hjust = 0),
          plot.title    = element_text(face = "bold", size = 14)) +
    ## Facet on the NUMERIC volume, matching the histogram/pseudocolor plots. Faceting on the
    ## raw StainQty column sorts it as text, so 10 lands before 2.5 -- and since facetPages()
    ## splits pages by numeric order, a paginated layout would show a numerically-chosen page of
    ## wells in lexical order. round() to 3 dp for the same reason validateLayoutGroups() does:
    ## it is the value the panels are keyed on.
    facet_wrap(~round(as.numeric(StainQty), 3), ncol = layout_ncol)
  addBiexpAxes(p, xch = sc$channels[1], ych = sc$channels[2], trans = trans, maxValue = maxValue,
               xlim = .xl, ylim = .yl)
}



## Reinstate pData strings that GatingSet() mangled.
##
## flowWorkspace's GatingSet() round-trips the phenoData through its own backend and reparses
## numeric-looking CHARACTER values, silently stripping leading zeros: "001" becomes "1" and "0042"
## becomes "42". The column stays class character, so nothing downstream looks wrong -- the value is
## just quietly different from the layout. Measured on flowWorkspace 4.22.1.
##
## That is data corruption in an IDENTIFIER, and Lot_Num is the one that matters: a lot recorded as
## "0042" in the layout would be reported as "42" in the export, the report and the dashboard. SOP
## and PlateID are exposed the same way.
##
## The flowSet's pData is still correct at this point, so it is the source of truth. Only columns
## that actually differ are touched, and only where both sides are character, so a legitimately
## numeric column is left alone. Matched on sample name rather than row order.
actaRestorePData <- function(gs, fs, quiet = FALSE) {
  pf <- flowWorkspace::pData(fs); pg <- flowWorkspace::pData(gs)
  if (is.null(pf) || is.null(pg) || !nrow(pg)) return(invisible(gs))
  key <- if ("name" %in% names(pf) && "name" %in% names(pg)) "name" else NULL
  idx <- if (is.null(key)) match(rownames(pg), rownames(pf)) else match(pg[[key]], pf[[key]])
  if (anyNA(idx)) return(invisible(gs))
  ## ONLY zero-padded values are restored, and only the cells that differ.
  ##
  ## Restoring every changed cell was wrong: the layout stores small stain volumes in Excel
  ## scientific notation ("3.90625E-2") and GatingSet's reparse NORMALISES them to "0.0390625",
  ## which is strictly better for anyone reading the export. A blanket restore undid that, so the
  ## fix for one cosmetic problem introduced another. Caught by the regression gate.
  ##
  ## Numeric equality cannot tell the two cases apart -- "001" and "1" are equal too -- so the
  ## discriminator is zero-PADDING: a leading zero immediately followed by another digit. That
  ## matches an identifier ("001", "0042", "-007") and not a decimal ("0.039") or an exponent.
  padded <- function(x) grepl("^\\s*-?0[0-9]", x)
  fixed <- character(0)
  for (cn in intersect(names(pf), names(pg))) {
    a <- pf[[cn]][idx]; b <- pg[[cn]]
    if (!is.character(a) || !is.character(b)) next
    hit <- !is.na(a) & !is.na(b) & a != b & padded(a)
    if (any(hit)) { pg[[cn]][hit] <- a[hit]; fixed <- c(fixed, cn) }
  }
  if (length(fixed)) {
    flowWorkspace::pData(gs) <- pg
    if (!isTRUE(quiet))
      message(sprintf("ACTA: restored %d pData column(s) reparsed by GatingSet(): %s",
                      length(fixed), paste(fixed, collapse = ", ")))
  }
  invisible(gs)
}

## One Info-sheet lookup, generalising the match-on-lowercased-name / first-non-blank-cell pattern
## that actaCompMode(), actaCompLabel() and the MM_R2_threshold read each spell out separately.
## Header annotations are tolerated: the sheet writes `miflowcyt_export (L)` and `dashboard_creation
## (L)`, so a caller asking for "miflowcyt_export" must still find it.
actaInfoValue <- function(info, field) {
  if (is.null(info) || !is.data.frame(info) || !nrow(info)) return(NA_character_)
  nm  <- tolower(trimws(names(info)))
  bare <- trimws(sub("\\s*\\([^()]*\\)\\s*$", "", nm))     # drop a trailing "(N)" / "(L)"
  hit <- match(tolower(trimws(field)), nm)
  if (is.na(hit)) hit <- match(tolower(trimws(field)), bare)
  if (is.na(hit)) return(NA_character_)
  v <- trimws(as.character(info[[hit]][1]))
  if (is.na(v) || !nzchar(v) || toupper(v) == "NA") NA_character_ else v
}

## Detector -> marker for every recorded parameter, from one FCS file. Used for the costain panel:
## the layout names only the antibody under titration, so the rest of the panel has to come from
## what the instrument actually recorded.
actaFcsPanel <- function(path) {
  v <- actaFcsVoltages(path, all = TRUE)
  v[, c("detector", "marker"), drop = FALSE]
}

## ======================= MiFlowCyt METADATA (2_87) ==================================
## ONE assembler, consumed by two renderers: the report typesets from it, and the
## FlowRepository export serialises it. Deriving each independently would guarantee they drift the
## first time a field moved -- and a PDF that disagrees with the metadata submitted alongside it is
## worse than either being absent.
##
## Data only, NO formatting. Nothing here knows about LaTeX, kable or JSON.
##
## COMPLETENESS. Several MiFlowCyt items legitimately live in the ELN (purpose, experiment
## variables, organization, conclusions, specimen source and collection, instrument QC). They are
## emitted as explicit ELN references rather than silently blank, and `standard$completeness` records
## that the result is MiFlowCyt-STRUCTURED, not MiFlowCyt-COMPLETE, so no downstream consumer can
## present it as a compliant submission document.
ACTA_MIFLOWCYT_DEFERRED <- "See ELN"

## Reference to the ELN entry that carries a deferred item, e.g. "See ELN EXP00000001 (Benchling)".
##
## Reads the LAYOUT first and the Info sheet only as a fallback. ELN_Name / ELN_ID / Operator are
## per-well columns in Layout_Plate, which is where the rest of the pipeline has always taken them
## from (BID and OPR are `unique(metadata_file$ELN_ID)` / `$Operator`); duplicating them in Info gave
## two sources for one fact and no rule for which wins. Info is still honoured so a workbook that
## carries them there keeps working.
.mfcElnRef <- function(info, stats = NULL) {
  id <- .mfcOne(stats, "ELN_ID");   if (is.na(id)) id <- actaInfoValue(info, "ELN_ID")
  nm <- .mfcOne(stats, "ELN_Name"); if (is.na(nm)) nm <- actaInfoValue(info, "ELN_Name")
  if (is.na(id) || !nzchar(id)) return(ACTA_MIFLOWCYT_DEFERRED)
  sprintf("See ELN %s%s", id, if (!is.na(nm) && nzchar(nm)) sprintf(" (%s)", nm) else "")
}

## First non-NA, non-empty value of a column, as a character scalar. Layout columns are constant
## within a run for everything used here, but a stray blank row must not decide the answer.
.mfcOne <- function(df, col) {
  if (is.null(df) || !col %in% names(df)) return(NA_character_)
  v <- as.character(df[[col]]); v <- v[!is.na(v) & nzchar(trimws(v))]
  if (!length(v)) NA_character_ else trimws(v[1])
}
.mfcUniq <- function(df, col) {
  if (is.null(df) || !col %in% names(df)) return(character(0))
  v <- as.character(df[[col]]); v <- unique(trimws(v[!is.na(v) & nzchar(trimws(v))]))
  v
}

## Per-detector voltages ($PnV) from ONE representative FCS file. MiFlowCyt asks for instrument
## settings, and the detector gains are the part that actually varies between runs on the same
## machine -- the channel list alone does not distinguish two very different acquisitions.
## Returns a zero-row frame rather than failing: a missing keyword must not stop a report.
actaFcsVoltages <- function(path, all = FALSE) {
  empty <- data.frame(detector = character(0), marker = character(0), voltage = character(0),
                      stringsAsFactors = FALSE)
  if (is.na(path) || !nzchar(path) || !file.exists(path)) return(empty)
  kw <- tryCatch(flowCore::read.FCSheader(path)[[1]], error = function(e) NULL)
  if (is.null(kw)) return(empty)
  get <- function(k) if (k %in% names(kw)) as.character(kw[[k]]) else NA_character_
  n <- suppressWarnings(as.integer(get("$PAR")))
  if (is.na(n) || n < 1) return(empty)
  out <- lapply(seq_len(n), function(i) data.frame(
    detector = get(paste0("$P", i, "N")), marker = get(paste0("$P", i, "S")),
    voltage  = get(paste0("$P", i, "V")), stringsAsFactors = FALSE))
  out <- do.call(rbind, out)
  ## Drop rows with no voltage: scatter and time channels often carry none, and listing them with a
  ## blank gain reads as missing data rather than as not-applicable. `all` keeps them, which is what
  ## the panel listing needs -- there a scatter channel is a real panel entry, just ungained.
  ## A gain of ZERO is not a gain. The ZE5 reports $PnV = 0 for its pulse-shape and time channels
  ## (T0/TLSW, T1/TMSW, INFO), which sailed through a mere nzchar() test and inflated "detectors with
  ## gain" from 29 real PMTs to 32. Filter on the parsed NUMBER, not on the string being present.
  if (isTRUE(all)) return(out)
  v <- suppressWarnings(as.numeric(out$voltage))
  out[!is.na(v) & v > 0, , drop = FALSE]
}

## `stats` holds ONE ROW PER WELL for the titrated antibody, so the titrated reagent's metadata comes
## straight off it. The costain panel is the OTHER markers the instrument recorded: named, but with
## catalogue/lot/clone deliberately EMPTY, because the layout carries that provenance only for the
## antibody under titration. Empty is the honest answer; inventing or repeating the titrated
## reagent's identifiers there would misattribute them.
.mfcReagents <- function(stats, panel) {
  titratedFor <- function(g) {
    s <- stats[stats$Dirname == g, , drop = FALSE]
    data.frame(
      group      = g,
      analyte    = .mfcOne(s, "Alias"),
      marker     = .mfcOne(s, "Markername"),
      reporter   = .mfcOne(s, "Fl"),
      detector   = .mfcOne(s, "Channel"),
      clone      = .mfcOne(s, "Clone"),
      vendor     = .mfcOne(s, "Vendor"),
      catalogue  = .mfcOne(s, "Cat_Num"),
      lot        = .mfcOne(s, "Lot_Num"),
      volumes_uL = paste(sort(unique(suppressWarnings(as.numeric(s$StainQty)))), collapse = ", "),
      stringsAsFactors = FALSE)
  }
  groups <- unique(stats$Dirname[!is.na(stats$Dirname)])
  titrated <- do.call(rbind, lapply(groups, titratedFor))

  ## Costain panel: every recorded marker that is not one of the titrated ones.
  costain <- data.frame(marker = character(0), detector = character(0), vendor = character(0),
                        catalogue = character(0), lot = character(0), clone = character(0),
                        stringsAsFactors = FALSE)
  if (!is.null(panel) && nrow(panel)) {
    tm <- unique(c(titrated$marker, titrated$detector))
    ## FLUORESCENCE reagents only. MiFlowCyt 2.4 is the reagent item (inside the SPECIMEN section --
    ## NOT a section of its own, which is what an earlier version of this comment claimed), and
    ## scatter, time and
    ## pulse-shape channels are not reagents -- listing TLSW and Event Info as panel antibodies
    ## with blank catalogue numbers is worse than omitting them. AREA channels only as well: the
    ## -H/-A pair is one reagent measured two ways, and -A is what everything downstream uses.
    keep <- panel[isFluorChannel(panel$detector, panel$marker) &
                    grepl("(-A|_A)$", panel$detector, ignore.case = TRUE) &
                    !(panel$marker %in% tm) & !(panel$detector %in% tm), , drop = FALSE]
    if (nrow(keep))
      costain <- data.frame(marker = keep$marker, detector = keep$detector,
                            vendor = NA_character_, catalogue = NA_character_,
                            lot = NA_character_, clone = NA_character_, stringsAsFactors = FALSE)
  }
  list(titrated = titrated, costain_panel = costain)
}

## MiFlowCyt appendix, as a flat Item / Description / Value table for the titration export.
##
## Written when Info!miflowcyt_export is TRUE. It is the submission-facing view of the same `actaMfc`
## object the report typesets, so the two cannot disagree about a value -- only about layout.
##
## NUMBERING FOLLOWS MIFLOWCYT 1.0, WHICH IS NOT WHAT ACTA'S REPORT USES. Checked against ISAC's
## published checklist and the filled-in example annotation (University of Iowa Flow Core, numbered
## per the MIFlowCyt 1.0 document). The standard has FOUR top-level sections, not five:
##   1 Experiment Overview            2 Flow Sample/Specimen Details
##   3 Instrument Details             4 Data Analysis Details
## Two differences matter. Reagents are 2.4 Fluorescence Reagent Description -- part of the SPECIMEN
## section -- not a section of their own, and Data Analysis is 4, not 5. ACTA's report currently
## numbers Reagents as 4 and Data Analysis as 5. This table uses the STANDARD's numbering because an
## appendix that does not match the standard is useless for a submission; the report's own numbering is
## a separate decision for the user.
##
## Layout: the published example is an indented outline, "1.1. Purpose" with the value beneath it. A
## spreadsheet cannot indent usefully, so the hierarchy becomes the Item column and section headings
## are their own rows with an empty Value -- which sorts and filters, where an outline would not.
actaMiFlowCytAppendix <- function(mfc) {
  if (is.null(mfc)) return(NULL)
  ## A cell holding the literal text "NA" is a blank someone typed, not a value -- actaInfoValue()
  ## has always treated it that way, and without the same rule here the sheet reported
  ## "well volume NA uL". Applied per element so one missing part cannot poison a composed string.
  v <- function(x) {
    x <- as.character(x)
    x <- x[!is.na(x) & nzchar(trimws(x)) & toupper(trimws(x)) != "NA"]
    if (!length(x)) "(not supplied)" else paste(x, collapse = "; ")
  }
  ## First value that is actually present. `c(a, b)[1]` does NOT do this -- it returns a when a is NA,
  ## which is how the detector came out "(not supplied)" while the reporter held it all along.
  pick <- function(...) {
    z <- as.character(c(...))
    z <- z[!is.na(z) & nzchar(trimws(z)) & toupper(trimws(z)) != "NA"]
    if (!length(z)) "(not supplied)" else z[1]
  }
  rows <- list()
  add  <- function(item, desc, value = "") rows[[length(rows) + 1L]] <<-
    data.frame(Item = item, Description = desc, Value = value, stringsAsFactors = FALSE)

  e <- mfc$experiment; s <- mfc$specimen; i <- mfc$instrument; a <- mfc$analysis
  add("", "MiFlowCyt 1.0 -- completeness", v(mfc$standard$completeness))
  add("", "Note", v(mfc$standard$note))

  add("1", "Experiment Overview")
  add("1.1", "Purpose", v(e$purpose))
  add("1.2", "Keywords", v(e$keywords))
  add("1.3", "Experiment Variables", v(e$experiment_variables))
  add("1.4", "Organization", v(e$organization))
  add("1.5", "Primary Contact", v(e$primary_contact))
  add("1.6", "Date", v(e$date))
  add("1.7", "Conclusions", v(e$conclusions))
  add("1.8", "Quality Control Measures", v(e$qc_measures))

  add("2", "Flow Sample/Specimen Details")
  add("2.1", "Sample/Specimen Material Description", v(s$characteristics$test_material))
  add("2.2", "Sample Characteristics",
      v(sprintf("%s e6 cells per well; well volume %s uL",
                v(s$characteristics$cells_per_well_e6), v(s$characteristics$well_volume_uL))))
  add("2.3", "Sample Treatment Description",
      v(sprintf("SOP %s; fixed: %s; permeabilised: %s; protocol: %s",
                v(s$treatment$sop), v(s$treatment$fixed), v(s$treatment$permeabilised),
                v(s$treatment$protocol))))
  ## 2.4 is one row per reagent: the titrated antibody of each group, then the rest of the panel,
  ## which MiFlowCyt wants described even though ACTA does not titrate it.
  add("2.4", "Fluorescence Reagent Description")
  ti <- mfc$reagents$titrated
  if (!is.null(ti) && nrow(ti))
    for (k in seq_len(nrow(ti)))
      add(sprintf("2.4.%d", k), sprintf("Titrated reagent -- %s", v(ti$group[k])),
          sprintf("%s; detector %s; clone %s; %s cat %s lot %s; volumes titrated %s uL",
                  v(ti$marker[k]),
                  ## Channel is blank whenever the marker was identified by NAME in the layout, in
                  ## which case `Fl` (the resolved marker channel) is the detector.
                  pick(ti$detector[k], ti$reporter[k]), v(ti$clone[k]),
                  v(ti$vendor[k]), v(ti$catalogue[k]), v(ti$lot[k]), v(ti$volumes_uL[k])))
  cp <- mfc$reagents$costain_panel
  if (!is.null(cp) && nrow(cp))
    add(sprintf("2.4.%d", (if (is.null(ti)) 0L else nrow(ti)) + 1L), "Remainder of the panel (not titrated)",
        v(paste(sprintf("%s on %s", cp$marker, cp$detector), collapse = "; ")))

  add("3", "Instrument Details")
  ## ACTA records manufacturer and model as ONE field, because that is how the FCS keywords give it.
  ## Splitting it by guessing where the vendor name ends would invent information.
  add("3.1", "Instrument Manufacturer", "Recorded as a single field with the model; see 3.2")
  add("3.2", "Instrument Model", v(i$manufacturer_model))
  vt <- i$settings$detector_voltages
  add("3.3", "Instrument Configuration and Settings",
      if (is.data.frame(vt) && nrow(vt))
        v(sprintf("%d detector(s) with a recorded gain: %s", nrow(vt),
                  paste(sprintf("%s=%s", vt[[1]], vt[[ncol(vt)]]), collapse = "; ")))
      else "(not supplied)")
  add("3.4", "Other Relevant Instrument Details", v(i$qc))

  add("4", "Data Analysis Details")
  df <- a$data_files
  add("4.1", "List-mode Data Files",
      if (!is.null(df) && nrow(df)) v(sprintf("%d FCS file(s): %s", nrow(df),
                                              paste(df$file, collapse = "; ")))
      else "(not supplied)")
  add("4.1.1", "Events read per file", v(a$downsample$text))
  add("4.2", "Compensation Description",
      v(if (is.null(a$compensation)) NULL
        else sprintf("mode: %s%s", v(a$compensation$mode),
                     if (!is.null(a$compensation$raw) && !is.na(a$compensation$raw))
                       sprintf(" (Info!compensation: %s)", a$compensation$raw) else "")))
  add("4.3", "Data Transformation Details")
  add("4.3.1", "Purpose of Data Transformation",
      "Display and gating on a FlowJo biexponential scale")
  ## The argument list ALREADY contains maxValue, so naming it separately printed it twice.
  add("4.3.2", "Data Transformation Description",
      v(paste(sprintf("%s = %s", names(a$transformation$arguments),
                      vapply(a$transformation$arguments, function(z) v(z), character(1))),
              collapse = "; ")))
  add("4.4", "Gating (Data Filtering) Details")
  add("4.4.1", "Gate Description", v(a$gating$description))
  add("4.4.2", "Gate Statistics",
      if (!is.null(a$gating$statistics) && nrow(a$gating$statistics))
        v(sprintf(paste("Count, percent of parent, MFI and robust SD for %d population/well",
                        "combination(s); see the Pop_stats sheet of the titration export"),
                  nrow(a$gating$statistics)))
      else "(none: no population flagged stats_export)")
  add("4.4.3", "Gate Boundaries",
      v("Per-sample gate coordinates are computed by openCyto; see the gating_template sheet"))
  add("4.4.4", "Populations gated", v(a$gating$populations))
  ## MIFLOWCYT 1.0 HAS NO SOFTWARE ITEM -- section 4 ends at 4.4.4 Other Relevant Gate Information.
  ## Labelled as an addition rather than smuggled in under a real item number, so a reader checking
  ## the appendix against the standard is not left wondering which item this is.
  add("4.5", "Analysis software (not a MiFlowCyt 1.0 item)",
      v(sprintf("%s; %s", v(a$software$acta_version), v(a$software$r_version))))

  do.call(rbind, rows)
}

actaMiFlowCyt <- function(stats, info, gating_template = NULL, qcList = NULL, gate_stats = NULL,
                          comp = NULL, trans_args = NULL, max_value = NA,
                          script_version = NA_character_, representative_fcs = NA_character_,
                          panel = NULL, downsample = NULL) {
  eln <- .mfcElnRef(info, stats)
  kw  <- actaInfoValue(info, "keywords")

  volts <- actaFcsVoltages(representative_fcs)

  ## 1 -- Experiment overview. Quality control measures point AT the report's own QC section rather
  ## than restating verdicts: one place to read them, and it cannot fall out of step.
  experiment <- list(
    purpose              = eln,
    ## MiFlowCyt 1.2. NA rather than a guess when Info!keywords is blank: a renderer prints "not
    ## supplied", and inventing keywords would put words in the submitter's mouth.
    keywords             = if (is.na(kw) || !nzchar(kw)) NA_character_ else kw,
    experiment_variables = sprintf("Stain volume (uL) per antibody; see Reagents. %s", eln),
    organization         = eln,
    primary_contact      = .mfcOne(stats, "Operator"),
    date                 = .mfcOne(stats, "Acquisition_date"),
    conclusions          = eln,
    qc_measures          = if (length(qcList))
                             sprintf("%d automated check(s) per antibody group; see the QC section.",
                                     length(qcList[[1]]))
                           else ACTA_MIFLOWCYT_DEFERRED)

  ## 2 -- Specimen. Sample treatment is where the SOP belongs: it is the staining protocol
  ## reference, which is exactly MiFlowCyt 2.3, and Fix/Perm are the parts of it the layout records.
  specimen <- list(
    characteristics = list(
      test_material      = .mfcOne(stats, "Test_Material"),
      cells_per_well_e6  = .mfcOne(stats, "e6_cells_per_well"),
      well_volume_uL     = .mfcOne(stats, "Well_volume_uL")),
    source     = eln,
    collection = eln,
    treatment  = list(
      sop      = .mfcOne(stats, "SOP"),
      fixed    = .mfcOne(stats, "Fix"),
      permeabilised = .mfcOne(stats, "Perm"),
      ## The protocol is BOTH: the SOP is the controlled document the staining followed, and the ELN
      ## entry is where that run's deviations and observations live. Naming only the ELN sent a reader
      ## looking for a protocol to a lab notebook; naming only the SOP would hide the run-specific
      ## record. Falls back to whichever exists.
      protocol = paste(c(if (!is.na(.mfcOne(stats, "SOP")))
                           sprintf("SOP %s", .mfcOne(stats, "SOP")), eln),
                       collapse = "; ")))

  ## 3 -- Instrument. Manufacturer/model comes from the FCS keywords via the script's Equipment
  ## column, so it reflects what acquired the data rather than what the layout claims.
  instrument <- list(
    manufacturer_model = .mfcOne(stats, "Equipment"),
    settings           = list(detector_voltages = volts,
                              n_detectors_with_gain = nrow(volts)),
    qc                 = eln)

  ## Derived from the FCS when not supplied, so callers need not thread it through.
  if (is.null(panel)) panel <- actaFcsPanel(representative_fcs)
  reagents <- .mfcReagents(stats, panel)

  ## 5 -- Data analysis. Gating points at the gating_template sheet for the parameters rather than
  ## duplicating them: the sheet is the executable definition, and a prose copy would be a second
  ## source of truth that nothing keeps honest.
  analysis <- list(
    data_files = if ("name" %in% names(stats))
                   data.frame(file = .mfcUniq(stats, "name"), stringsAsFactors = FALSE)
                 else data.frame(file = character(0), stringsAsFactors = FALSE),
    compensation = comp,
    ## Preprocessing, and it comes FIRST in the analysis because every statistic below is computed on
    ## the events it kept. Pass the actaDownsampleReport() list; NULL is read as "not recorded".
    downsample = if (is.null(downsample))
                   list(requested = NA_integer_, text = "Not recorded.")
                 else downsample,
    gating = list(
      ## Same filter the script applies: a blank alias, or one starting '#', is a commented-out
      ## reference row that the pipeline ignores. Reading the RAW sheet listed all 20 method
      ## reference rows and an NA as though they were gated populations.
      populations = if (!is.null(gating_template) && "alias" %in% names(gating_template)) {
                      a <- trimws(as.character(gating_template$alias))
                      a[!is.na(a) & nzchar(a) & !startsWith(a, "#")]
                    } else character(0),
      description = paste("Automated gating via openCyto.",
                          "See the gating_template sheet in the instructions file for more info."),
      statistics  = gate_stats),
    transformation = list(arguments = trans_args, max_value = max_value),
    software = list(acta_version = script_version, r_version = R.version.string))

  list(
    standard = list(
      name = "MiFlowCyt", version = "1.0",
      completeness = "structured-not-complete",
      note = paste("Structured to MiFlowCyt but not self-contained: purpose, experiment variables,",
                   "organization, conclusions, specimen source and collection, and instrument QC",
                   "are recorded in the ELN and referenced here. Not a compliant submission",
                   "document on its own.")),
    experiment = experiment, specimen = specimen, instrument = instrument,
    reagents = reagents, analysis = analysis)
}

## ============================ COMPENSATION (2_85) ==================================
## Spectral (Aurora/ZE5) data arrives already unmixed, so ACTA never needed compensation.
## CONVENTIONAL flow data does, and it must be applied to the RAW linear values -- i.e. before
## the biexp transformer is registered on the GatingSet, since compensation is a linear
## un-mixing and applying it after a non-linear transform is simply wrong.
##
## The mode comes from the workbook's `Info` sheet, column `compensation`, one value for the
## whole run:
##   "csv"   -> read a *CompMatrix*.csv from the working directory
##   "self"  -> each frame's own acquisition-time matrix ($SPILLOVER / $SPILL)
##   anything else, blank, or missing column -> IDENTITY
## Identity is applied rather than skipped so the downstream object is a compensated set in
## every branch: one code path, and `fs_comp` always means the same thing. Multiplying by
## solve(diag(n)) == diag(n) is exact in floating point, so an identity run is bit-identical
## to no compensation at all.

## Normalise the Info sheet's `compensation` cell to one of "csv" / "self" / "none".
## An absent sheet, absent column, blank cell or NA all mean "none" -- compensation is opt-in,
## so a workbook written before this column existed keeps working untouched.
actaCompMode <- function(info, quiet = FALSE) {
  if (is.null(info) || !is.data.frame(info) || !nrow(info)) return("none")
  hit <- match("compensation", tolower(trimws(names(info))))
  if (is.na(hit)) return("none")
  v <- trimws(as.character(info[[hit]][1]))
  if (is.na(v) || !nzchar(v) || toupper(v) == "NA") return("none")
  m <- tolower(v)
  if (!m %in% c("csv", "self")) {
    ## Named loudly on purpose: a typo like "CSV " or "spill" must not silently become "no
    ## compensation", which would look like a successful run and quietly skew every gate.
    if (!quiet)
      warning(sprintf(paste0("ACTA: Info!compensation is '%s', which is not 'csv' or 'self' -- ",
                             "running UNCOMPENSATED (identity matrix)."), v), call. = FALSE)
    return("none")
  }
  m
}

## The Info sheet's `compensation` cell, VERBATIM, for plot captions.
##
## Deliberately the layout's own value and nothing else -- not the matrix, not the mode that was
## applied. The caption reports what the workbook asked for; what ACTA did with it is logged at
## run time by actaCompensate(). An empty cell shows as an em dash, matching how gateCaption()
## renders every other unset field.
actaCompLabel <- function(info) {
  raw  <- ""
  if (!is.null(info) && is.data.frame(info) && nrow(info)) {
    hit <- match("compensation", tolower(trimws(names(info))))
    if (!is.na(hit)) {
      v <- trimws(as.character(info[[hit]][1]))
      if (!is.na(v) && nzchar(v) && toupper(v) != "NA") raw <- v
    }
  }
  if (!nzchar(raw)) "\u2014" else raw
}

## Pull one frame's acquisition-time spillover matrix. Keyword order is deliberate: the FCS3.1
## standard name ($SPILLOVER) first, then the older/vendor spellings.
.actaFrameSpill <- function(ff, sn = "") {
  kw <- flowCore::keyword(ff)
  for (k in c("$SPILLOVER", "$SPILL", "SPILL", "SPILLOVER", "spillover")) {
    v <- kw[[k]]
    if (!is.null(v) && is.matrix(v) && nrow(v) && nrow(v) == ncol(v)) return(v)
  }
  stop(sprintf(paste0("ACTA: compensation='self' but sample '%s' carries no spillover matrix ",
                      "($SPILLOVER or $SPILL). Either export the FCS with its matrix, or use ",
                      "compensation='csv' with a *CompMatrix*.csv."), sn), call. = FALSE)
}

## Locate and read the run's compensation CSV: any file in `wd` whose NAME CONTAINS "CompMatrix"
## and ends .csv. Expected shape is a square matrix with channel names as both the header row and
## the first column -- the layout flowCore's `spillover()` and FlowJo/Diva both export.
.actaReadCompCsv <- function(wd) {
  hits <- list.files(wd, pattern = "CompMatrix", ignore.case = TRUE, full.names = TRUE)
  hits <- hits[grepl("\\.csv$", hits, ignore.case = TRUE)]
  if (!length(hits))
    stop(sprintf(paste0("ACTA: compensation='csv' but no file matching *CompMatrix*.csv found in ",
                        "'%s'. Put the exported matrix there, or set Info!compensation to 'self'."),
                 wd), call. = FALSE)
  if (length(hits) > 1)
    ## Deliberately fatal rather than picking the first: silently compensating with the wrong
    ## matrix is unrecoverable downstream and invisible in the report.
    stop(sprintf(paste0("ACTA: compensation='csv' found %d candidate matrices in '%s' (%s). ",
                        "Leave exactly one *CompMatrix*.csv in the working directory."),
                 length(hits), wd, paste(basename(hits), collapse = ", ")), call. = FALSE)
  m <- as.matrix(utils::read.csv(hits[1], row.names = 1, check.names = FALSE))
  if (nrow(m) != ncol(m))
    stop(sprintf("ACTA: '%s' is %d x %d -- a compensation matrix must be square.",
                 basename(hits[1]), nrow(m), ncol(m)), call. = FALSE)
  ## Some exports leave the row labels off (or number them); the matrix is square and ordered,
  ## so the column names are the authoritative channel list.
  if (!all(rownames(m) %in% colnames(m))) rownames(m) <- colnames(m)
  storage.mode(m) <- "numeric"
  attr(m, "acta_source") <- basename(hits[1])
  m
}

## Restrict a spillover matrix to the channels actually present in the data.
##
## CAVEAT, stated in the log rather than buried: dropping a channel also drops its correction
## INTO the channels that are kept, so a filtered matrix is an approximation of the full
## un-mixing, not an equivalent of it. That is unavoidable when the matrix covers a bigger panel
## than the acquisition, but it should be a visible choice.
.actaFilterSpill <- function(m, chans, quiet = FALSE) {
  keep <- colnames(m)[colnames(m) %in% chans]
  if (!length(keep))
    stop(sprintf(paste0("ACTA: no channel in the compensation matrix matches the data. ",
                        "Matrix: %s. Data: %s."),
                 paste(colnames(m), collapse = ", "), paste(chans, collapse = ", ")),
         call. = FALSE)
  dropped <- setdiff(colnames(m), keep)
  if (length(dropped) && !quiet)
    message("[ACTA] compensation: matrix channels not in this data, dropped -- ",
            paste(dropped, collapse = ", "),
            " (their spill INTO the retained channels is lost; the filtered matrix is an ",
            "approximation of the full un-mixing).")
  m[keep, keep, drop = FALSE]
}

## Apply compensation and return the compensated set. Works on a flowSet or a cytoset --
## flowCore::compensate() is generic over both, so this does not care which object model the
## ingestion step uses.
actaCompensate <- function(fs, mode = "none", wd = getwd(), quiet = FALSE) {
  mode  <- tolower(trimws(as.character(mode)))
  chans <- colnames(fs)
  fluor <- chans[isFluorChannel(chans)]
  if (mode == "csv") {
    m   <- .actaReadCompCsv(wd)
    src <- attr(m, "acta_source")
    m   <- .actaFilterSpill(m, chans, quiet = quiet)
    if (!quiet)
      message(sprintf("[ACTA] compensation: 'csv' -- %s, %d x %d after filtering to the data (%s).",
                      src, nrow(m), ncol(m), paste(colnames(m), collapse = ", ")))
    out <- flowCore::compensate(fs, m)
  } else if (mode == "self") {
    ## Per-sample: each frame is compensated with ITS OWN acquisition matrix. A named list keyed
    ## by sample is the form compensate() accepts for a set, and it keeps this branch working if
    ## one well was acquired with a different matrix than the rest.
    sns   <- sampleNames(fs)
    spill <- setNames(lapply(sns, function(sn) .actaFrameSpill(fs[[sn]], sn)), sns)
    dims  <- vapply(spill, ncol, integer(1))
    if (!quiet)
      message(sprintf("[ACTA] compensation: 'self' -- per-sample $SPILLOVER/$SPILL, %s across %d sample(s).",
                      if (length(unique(dims)) == 1) sprintf("%d x %d", dims[1], dims[1])
                      else sprintf("%d-%d channels (NOT uniform)", min(dims), max(dims)),
                      length(sns)))
    out <- flowCore::compensate(fs, spill)
  } else {
    id <- diag(length(fluor)); dimnames(id) <- list(fluor, fluor)
    if (!quiet)
      message(sprintf("[ACTA] compensation: none -- identity applied over %d fluorescence channel(s); values unchanged.",
                      length(fluor)))
    out <- flowCore::compensate(fs, id)
  }
  ## compensate() can return a set whose phenoData has been reduced to just `name`, and every
  ## downstream step (facets, stats, export) reads the layout columns off pData -- so restore it.
  pd <- pData(fs)
  if (!identical(sort(names(pData(out))), sort(names(pd)))) pData(out) <- pd[sampleNames(out), , drop = FALSE]
  out
}


## -----------------------------------------------------------------------------------------
## REPORT QC CHECKS
## -----------------------------------------------------------------------------------------
## Each check answers ONE plain question with TRUE / FALSE, for the "QC" section of the report.
## Deliberately NON-FATAL: a failing check must never stop the run, only surface in the report,
## so the analyst sees the whole picture and decides. Add new checks by appending to the list
## actaQCChecks() returns -- the .Rmd renders whatever it gets, so no .Rmd edit is needed.
##
## A missing population is reported, not silently passed: `No "<pop>" gate found, QC skipped`.
## That distinction matters -- "skipped" is not "passed".

## Locate a population node by ALIAS (basename of its path), case-insensitively.
## Returns the full node path, or NA if absent.
## PASS/FAIL from a logical, keeping the logical alongside it for the roll-up.
.actaQCVerdict <- function(id, statement, ok) {
  ok <- isTRUE(ok)
  list(id = id, statement = statement, verdict = if (ok) "PASS" else "FAIL", passed = ok)
}

## Every check carries a stable `id`. The statement is prose and will be reworded; the id is what
## the export column (QC_<id>) and the report table key on, so neither breaks when the wording does.
## HARMONISED VOCABULARY. Every QC column -- the roll-up and each QC_<id> -- reports one of
## PASS / FAIL / SKIPPED and nothing else. This used to put the free-text reason in `verdict`, so
## a skipped check wrote a whole sentence ("No Costain SI, QC skipped") into a column whose
## siblings held a single word, and the report matrix inherited it as an unreadable cell. The
## reason is kept in `note`, which is where an explanation belongs.
##
## `passed = NA` is deliberate and unchanged: actaQCStatus() rolls up on isTRUE(passed), so a
## SKIPPED check still makes the overall verdict FAIL. A check that never ran is not a pass.
.actaQCSkip <- function(id, statement, why) list(id = id, statement = statement,
                                                verdict = "SKIPPED", note = why, passed = NA)

## Roll up one antibody's checks into a single PASS/FAIL for the titration export.
## PASS only if EVERY check passed; a failed OR skipped check yields FAIL, since a check that never
## ran is not evidence of quality.
actaQCStatus <- function(checks) {
  if (!length(checks)) return("FAIL")
  ok <- vapply(checks, function(c) isTRUE(c$passed), logical(1))
  if (all(ok)) "PASS" else "FAIL"
}

actaQCFindPop <- function(gs, alias) {
  p <- tryCatch(gs_get_pop_paths(gs), error = function(e) character(0))
  if (!length(p)) return(NA_character_)
  hit <- which(tolower(basename(p)) == tolower(alias))
  if (!length(hit)) NA_character_ else p[hit[1]]
}

## Scatter channel names taken FROM THE GATE ITSELF, never hardcoded: a panel may name them
## "FSC-A"/"SSC-A" or otherwise, and the gate is the authority on which dims it was built in.
## Returns list(fsc=, ssc=) with NA for whichever is absent from the gate's parameters.
actaQCScatterDims <- function(gs, node) {
  ch <- tryCatch(names(flowCore::parameters(gs_pop_get_gate(gs, node)[[1]])),
                 error = function(e) NULL)
  if (is.null(ch) || !length(ch))
    ch <- tryCatch(as.vector(flowCore::parameters(gs_pop_get_gate(gs, node)[[1]])),
                   error = function(e) character(0))
  ## Vendor-agnostic, via the same helpers the rest of the pipeline uses. These were hardcoded
  ## `^FSC` / `^SSC` greps, which is fine on an Aurora and silently wrong on a ZE5: its gates are
  ## stored in $PnN space as FS00-A / SS02-A, neither grep matched, and cells_fsc reported SKIPPED
  ## on every group -- a QC that never runs, which actaQCStatus() then rolls up as a failure.
  fsc <- pickForwardScatter(ch)
  sideCandidates <- ch[isScatterChannel(ch) & !ch %in% fsc &
                         !grepl(ACTA_FWD_SCATTER_RE, ch, ignore.case = TRUE, perl = TRUE)]
  list(fsc = if (length(fsc)) fsc[1] else NA_character_,
       ssc = if (length(sideCandidates)) sideCandidates[1] else NA_character_)
}

## QC 1 -- did we gate the CELLS, or the debris? `which.max(w)` inside gate_flowclust_2d picks
## the largest-weight component with nothing anchoring it to the cell population, so a
## debris-heavy prep can silently invert the gate (measured 2026-08-13: it flips at ~30-40%
## debris, reporting a plausible-looking "36.5% Cells" while gating the junk).
##
## The test compares Cells against ALL ROOT events, NOT against the excluded events. Against the
## excluded set it does not work: that set is BIMODAL -- a little debris below plus the whole
## high-FSC shoulder and aggregates above -- and on real data the upper mode dominates ~8:1, so
## the excluded median sits ABOVE the cells median and a perfectly good gate scores FALSE.
## Measured on an internal run (1.7M events, K=5 q=0.9):
##      vs excluded : ratio 0.55 (good gate) -> FALSE   <- cries wolf, useless
##      vs root     : ratio 0.87 (good gate) vs 0.06 (gated on debris)  <- ~14x separation
## Root already contains the shoulder on both sides of the comparison, so it cannot invert it.
## The ratio is dimensionless, so this travels across FSC/SSC voltage settings unchanged.
## Verdicts are PASS / FAIL rather than TRUE / FALSE, and each check also returns `passed` as a
## LOGICAL for the roll-up: TRUE, FALSE, or NA when the check could not run. NA is deliberately not
## FALSE here -- the report distinguishes "skipped" from "failed" -- but actaQCStatus() counts it as
## a failure, because an unrun check is not a passed one.
actaQCCellsBrighter <- function(gs, alias = "Cells", min_ratio = 0.5) {
  node <- actaQCFindPop(gs, alias)
  qOf <- function(ch) sprintf("Median %s of %s is at least %.0f%% of the median of all events",
                              ch, alias, 100 * min_ratio)
  if (is.na(node))
    return(.actaQCSkip("cells_fsc", qOf("FSC"), sprintf('No "%s" gate found, QC skipped', alias)))
  dims <- actaQCScatterDims(gs, node)
  q <- qOf(if (is.na(dims$fsc)) "FSC" else dims$fsc)
  if (is.na(dims$fsc))
    return(.actaQCSkip("cells_fsc", q, sprintf('No FSC channel on the "%s" gate, QC skipped', alias)))
  inside <- rootv <- numeric(0)
  for (i in seq_along(gs)) {
    v <- tryCatch(flowCore::exprs(gh_pop_get_data(gs[[i]], "root"))[, dims$fsc], error = function(e) NULL)
    ind <- tryCatch(gh_pop_get_indices(gs[[i]], node), error = function(e) NULL)
    if (is.null(v) || is.null(ind) || length(v) != length(ind)) next
    inside <- c(inside, v[ind]); rootv <- c(rootv, v)
  }
  if (!length(inside) || !length(rootv))
    return(.actaQCSkip("cells_fsc", q, "Could not read scatter values, QC skipped"))
  mr <- median(rootv)
  if (!is.finite(mr) || mr == 0)
    return(.actaQCSkip("cells_fsc", q, "Median of all events is zero/NA, QC skipped"))
  .actaQCVerdict("cells_fsc", q, (median(inside) / mr) > min_ratio)
}

## QC 2 / 3 -- proportion checks on a population, as % OF ITS PARENT (the same number the
## gating plots print). Non-fatal and phrased as the question the analyst actually asks.
actaQCPercentAbove <- function(gs, alias, threshold, statement, id = tolower(alias)) {
  node <- actaQCFindPop(gs, alias)
  if (is.na(node))
    return(.actaQCSkip(id, statement, sprintf('No "%s" gate found, QC skipped', alias)))
  pc <- tryCatch(gs_pop_get_stats(gs, node, type = "percent")[[3]], error = function(e) NULL)
  if (is.null(pc) || !length(pc) || all(is.na(pc)))
    return(.actaQCSkip(id, statement, sprintf('No statistics for "%s", QC skipped', alias)))
  ## Verdict on the WORST WELL, not the group mean. percent is a FRACTION (0-1) from
  ## gs_pop_get_stats, hence threshold/100.
  ##
  ## This was mean(pc) > threshold, and a pooled mean hides exactly what the check exists to catch:
  ## a group whose viability collapsed in some wells still averages above the line and reports PASS,
  ## which is what prompted "low live cells but QC says pass". One unusable well invalidates its
  ## point on the titration curve no matter how healthy its neighbours are, so the worst well has to
  ## decide -- the same rule min_events already uses.
  pcMin <- min(pc, na.rm = TRUE); pcMean <- mean(pc, na.rm = TRUE)
  out <- .actaQCVerdict(id, statement, pcMin > threshold / 100)
  ## The mean is still reported, because "worst well 41%, group mean 63%" is a different situation
  ## from "worst well 41%, group mean 42%" and they call for different responses.
  if (!isTRUE(out$passed))
    out$note <- sprintf("worst well %.1f%% (group mean %.1f%%, threshold %g%%) across %d well(s)",
                        100 * pcMin, 100 * pcMean, threshold, length(pc))
  out
}

## Every check for one GatingSet, in report order. APPEND HERE to add a QC.
## The Costain is the background control: every titration dose should stain ABOVE it. If it does
## not, the series never separated from background, and maxSI_StainQty lands on the Costain well --
## which reads as a valid recommendation while meaning the opposite.
## Ties pass: a Costain equal to the weakest dose is still "the lowest".
## Unstained never enters this -- it is dropped before gating, so `si` only holds Costain + Stain.
actaQCCostainLowest <- function(si) {
  st <- "Costain SI is the lowest in the group"
  if (is.null(si) || !nrow(si) || !all(c("StainType", "SI") %in% names(si)))
    return(.actaQCSkip("costain_lowest", st, "No per-sample SI table, QC skipped"))
  co <- suppressWarnings(as.numeric(si$SI[isLayoutValue(si$StainType, "Costain")]))
  sa <- suppressWarnings(as.numeric(si$SI[isLayoutValue(si$StainType, "Stain")]))
  co <- co[is.finite(co)]; sa <- sa[is.finite(sa)]
  if (!length(co)) return(.actaQCSkip("costain_lowest", st, "No Costain SI, QC skipped"))
  if (!length(sa)) return(.actaQCSkip("costain_lowest", st, "No Stain SI to compare, QC skipped"))
  out <- .actaQCVerdict("costain_lowest", st, min(co) <= min(sa))
  ## A note only on FAIL, as `live` and `min_events` already do. This was the ONE check that failed
  ## carrying nothing but its own statement, so "costain_lowest FAIL" meant re-deriving the culprit
  ## by hand from the SI table -- which happened twice on OQ_Test3 before it was written down.
  ##
  ## The weakest dose is what decides this check, so the weakest dose is what the note names. It
  ## also reports the series MAXIMUM, because the two situations that reach this line are different
  ## and call for different responses: a series that never separated at all (max ~ the Costain, the
  ## failure this check exists to catch, where maxSI_StainQty lands on the Costain well and reads
  ## as a recommendation while meaning the opposite), versus a series that separates perfectly at
  ## the doses you would actually use and has one noisy well near background. Measured on
  ## OQ_Test3's CCR7 series 2026-09-03: Costain 2.187, StainQty 0.3125 at 1.599, series max 3.809
  ## -- the second situation, and the note now says so instead of leaving it to be reconstructed.
  if (!isTRUE(out$passed)) {
    .sa  <- si[isLayoutValue(si$StainType, "Stain"), , drop = FALSE]
    .sv  <- suppressWarnings(as.numeric(.sa$SI))
    .sv[!is.finite(.sv)] <- Inf
    .k   <- which.min(.sv)
    .q   <- if (length(.k) && "StainQty" %in% names(.sa)) as.character(.sa$StainQty[[.k]]) else "?"
    out$note <- sprintf(paste0("Costain SI %.3f exceeds the weakest dose: StainQty %s has SI %.3f ",
                               "(series max %.3f). That dose did not separate from background."),
                        min(co), .q, min(sa), max(sa))
  }
  out
}

## QC 5 -- is every gated population big enough to say anything about?
## A gate can be drawn perfectly and still leave a population with 12 events in it, at which point
## every downstream statistic (MFI, SI, the Michaelis-Menten fit) is noise dressed as a number. This is the
## check that catches a titration whose top dilutions have been gated down to nothing, and it is
## also the one that flags an over-tight gate that the ratio-based checks above cannot see, because
## a tiny population can sit in exactly the right place.
##
## Scoped per WELL and per POPULATION: the worst single (well, population) pair decides the verdict,
## since one starved well invalidates that point on the curve even if every other well is fine. root
## is excluded -- it is the acquisition, not a gating outcome, and would only ever restate the
## events-per-well setting. actaQCChecks() is called once per Dirname, so this is per-Dirname too.
actaQCMinEvents <- function(gs, min_events = 100, si = NULL) {
  st <- sprintf("Every population has at least %d events in every well (except costain)", min_events)
  nodes <- tryCatch(setdiff(gs_get_pop_paths(gs, path = "auto"), "root"),
                    error = function(e) character(0))
  if (!length(nodes))
    return(.actaQCSkip("min_events", st, "No gated populations found, QC skipped"))
  sn <- tryCatch(sampleNames(gs), error = function(e) rep(NA_character_, length(gs)))
  ## The Costain well's MARKER-POSITIVE population is exempt. Costain is the background control at
  ## stain volume 0, so few positive events is what it is SUPPOSED to look like -- counting it here
  ## flagged a correct result as a failure (OQ_Test1: 88 events in Reagent+ of the Costain well).
  ## Only the "+" population is skipped: the Costain well's Cells/Single_cells/Live are ordinary
  ## populations and a starved one there is still a real problem worth catching.
  costain <- character(0)
  if (!is.null(si) && all(c("name", "StainType") %in% names(si)))
    costain <- unique(as.character(si$name[isLayoutValue(si$StainType, "Costain")]))
  worst <- Inf; where <- NA_character_; nRead <- 0L; nSkip <- 0L
  for (i in seq_along(gs)) for (nd in nodes) {
    if (!is.na(sn[i]) && sn[i] %in% costain && grepl("\\+$", nd)) { nSkip <- nSkip + 1L; next }
    n <- tryCatch(sum(gh_pop_get_indices(gs[[i]], nd)), error = function(e) NA_integer_)
    if (is.na(n)) next
    nRead <- nRead + 1L
    if (n < worst) { worst <- n; where <- sprintf("%s in %s", nd, sn[i]) }
  }
  if (!nRead || !is.finite(worst))
    return(.actaQCSkip("min_events", st, "Could not read population counts, QC skipped"))
  out <- .actaQCVerdict("min_events", st, worst >= min_events)
  ## A note only on FAIL, matching how .actaQCSkip uses it. "min_events FAIL" on its own would send
  ## someone hunting through every well and every gate; the smallest population names itself.
  if (!isTRUE(out$passed))
    out$note <- sprintf("smallest population is %s with %d event(s), below %d%s", where, worst,
                        min_events,
                        if (nSkip) sprintf(" (%d costain marker-positive population(s) exempt)", nSkip) else "")
  out
}

actaQCChecks <- function(gs, si = NULL) {
  list(
    actaQCCellsBrighter(gs, "Cells"),
    actaQCPercentAbove(gs, "Live",         50, "More than 50% Live cells",      id = "live"),
    actaQCPercentAbove(gs, "Single_cells", 80, "More than 80% single cells",    id = "single_cells"),
    actaQCCostainLowest(si),
    actaQCMinEvents(gs, 100, si)
  )
}

## One PASS/FAIL/skip value per check, named QC_<id>, for the export. Built from the checks
## themselves so a new check becomes a new column with no further edit.
actaQCColumns <- function(checks) {
  if (!length(checks)) return(list())
  setNames(lapply(checks, function(c) c$verdict),
           paste0("QC_", vapply(checks, function(c) c$id, character(1))))
}


## ======================= MICHAELIS-MENTEN MODEL ON THE STAIN INDEX =======================
## SI = Vmax * V / (Km + V), fitted per antibody against stain volume V. Algebraically the
## Michaelis-Menten form, so stats::SSmicmen() supplies a self-starting fit.
##
## Reported: Vmax (the saturating Stain Index), a pseudo-R^2, and the VOLUME at which the model
## reaches 90% of Vmax -- which for this model is exactly 9*Km, since
## 0.9*Vmax = Vmax*V/(Km+V)  =>  V = 9*Km. That is a recommended-volume readout, comparable with
## the existing maxSI_StainQty.
##
## CAVEAT worth keeping in view: a Michaelis-Menten model is monotonic and saturating, whereas SI
## against volume frequently TURNS OVER -- background MFI rises faster than signal at high dose,
## which is exactly why maxSI/maxSI_StainQty exist. On such a series the fit is poor and Vmax is
## pushed above any observed SI, putting 9*Km outside the tested range. The R^2 is the thing to
## read first; a low value means the model, not the antibody, is the problem.
## Why a fit failed, in terms an analyst can act on. "nls did not converge" is true but says
## nothing; by far the most common cause here is a series that DECLINES with dose, which a
## saturating isotherm cannot represent at all -- so name that shape when it is present.
.actaMichaelisMentenWhy <- function(v, y) {
  o    <- order(v)
  vs   <- v[o]; ys <- y[o]
  pk   <- which.max(ys)
  ## Peak before the top dose, and a real drop after it (>5% of the peak), = turnover.
  if (pk < length(ys) && (ys[pk] - ys[length(ys)]) > 0.05 * abs(ys[pk]))
    return(sprintf("SI peaks at %s uL then declines -- not a saturating isotherm",
                   signif(vs[pk], 3)))
  if (diff(range(ys)) < .Machine$double.eps^0.5)
    return("SI is flat across the series")
  "nls did not converge"
}

actaMichaelisMenten <- function(volume, si) {
  out <- list(ok = FALSE, Vmax = NA_real_, Km = NA_real_, r2 = NA_real_, v90 = NA_real_,
              n = 0L, note = NA_character_)
  keep <- is.finite(suppressWarnings(as.numeric(volume))) & is.finite(si)
  v <- as.numeric(volume)[keep]; y <- as.numeric(si)[keep]
  keep2 <- v >= 0
  v <- v[keep2]; y <- y[keep2]
  out$n <- length(v)
  ## Two parameters, so three distinct doses is the minimum for a fit that is not just
  ## interpolation -- and an R^2 of 1 on two points would be meaningless.
  if (length(unique(v)) < 3) { out$note <- "fewer than 3 distinct stain volumes"; return(out) }

  fit <- try(stats::nls(y ~ stats::SSmicmen(v, Vm, K)), silent = TRUE)
  if (inherits(fit, "try-error")) {
    ## The self-start derives its guess from a Lineweaver-Burk style linearisation, which falls
    ## over on a series that turns over. Retry from explicit data-driven starts, bounded via the
    ## "port" algorithm so neither parameter can go negative on the way.
    b0 <- max(y, na.rm = TRUE) * 1.2
    k0 <- stats::median(v[v > 0], na.rm = TRUE)
    if (!is.finite(b0) || b0 <= 0 || !is.finite(k0) || k0 <= 0) {
      out$note <- "no usable starting values"; return(out)
    }
    fit <- try(stats::nls(y ~ Vmax * v / (Km + v), start = list(Vmax = b0, Km = k0),
                          algorithm = "port", lower = c(Vmax = 0, Km = 1e-9)), silent = TRUE)
  }
  if (inherits(fit, "try-error")) { out$note <- .actaMichaelisMentenWhy(v, y); return(out) }

  cf   <- stats::coef(fit)
  Vmax <- unname(cf[[1]]); Km <- unname(cf[[2]])
  ## A negative or zero Km/Vmax is a numerically valid solution of a non-physical curve; reporting
  ## it as a titration recommendation would be worse than reporting nothing.
  if (!is.finite(Vmax) || !is.finite(Km) || Vmax <= 0 || Km <= 0) {
    out$note <- "fit returned non-physical parameters"; return(out)
  }
  pred <- Vmax * v / (Km + v)
  sst  <- sum((y - mean(y))^2)
  out$Vmax <- Vmax
  out$Km   <- Km
  ## Pseudo-R^2: 1 - SSres/SStot. Not a true R^2 (the model is non-linear, so it is not a variance
  ## decomposition and can go negative when the fit is worse than the mean) -- it is reported as a
  ## goodness-of-fit indicator only.
  out$r2   <- if (sst > 0) 1 - sum((y - pred)^2) / sst else NA_real_
  out$v90  <- 9 * Km
  out$ok   <- TRUE
  out
}

## One fit per group. Returns a data.frame with the grouping columns plus Vmax / Km / r2 / v90.
actaMichaelisMentenFits <- function(df, group_cols = c("Dirname", "label"),
                             vol = "StainQty", y = "SI") {
  ## as.data.frame first: `stats` arrives as a data.table (gs_pop_get_stats returns one), where
  ## df[, cols] means something entirely different and errors on a character vector of names.
  df <- as.data.frame(df)
  need <- c(group_cols, vol, y)
  miss <- setdiff(need, names(df))
  if (length(miss))
    stop(sprintf("actaMichaelisMentenFits(): missing column(s) %s", paste(miss, collapse = ", ")),
         call. = FALSE)
  keys <- unique(df[, group_cols, drop = FALSE])
  res  <- lapply(seq_len(nrow(keys)), function(i) {
    sel <- rep(TRUE, nrow(df))
    for (gc in group_cols) sel <- sel & as.character(df[[gc]]) == as.character(keys[[gc]][i])
    f <- actaMichaelisMenten(df[[vol]][sel], df[[y]][sel])
    cbind(keys[i, , drop = FALSE],
          data.frame(MM_Vmax = f$Vmax, MM_Kd = f$Km, MM_R2 = f$r2, MM_90pct_Vmax = f$v90,
                     MM_ok = f$ok, MM_note = f$note, stringsAsFactors = FALSE))
  })
  do.call(rbind, res)
}

## Curve points for overlaying the fit on the SI plot.
##
## The SI plot draws stain volume as EVENLY SPACED categories (a 2-fold series would otherwise
## crowd every low dose against the left edge), so the curve has to be expressed in category
## POSITION, not volume. `axis_levels` are the volumes in axis order; position i corresponds to
## axis_levels[i], and approx() interpolates between them.
##
## The grid is built uniformly in POSITION space and then converted to volume -- not the other way
## round. A uniform volume grid would put almost no points in the first few categories, where a
## 2-fold series is densest, and the curve would render visibly straight-edged there.
##
## Position -> volume is interpolated in LOG space (see .actaPosToVol). A dilution series is
## geometric, so log10(volume) is LINEAR in category position -- interpolating the volumes directly
## is only piecewise-linear, which leaves a derivative kink at every category boundary and reads as
## a visibly angular curve on the steep part of the rise.
.actaPosToVol <- function(pos, lv) {
  ip <- which(lv > 0)
  if (!length(ip)) return(rep(0, length(pos)))
  p1  <- min(ip)
  out <- numeric(length(pos))
  hi  <- pos >= p1
  ## exact for a geometric series: equal position steps are equal log steps
  if (any(hi))
    out[hi] <- 10^stats::approx(x = ip, y = log10(lv[ip]), xout = pos[hi], rule = 2)$y
  ## the 0 -> first-positive gap has no log representation, so cross it linearly in volume
  if (any(!hi))
    out[!hi] <- stats::approx(x = c(1, p1), y = c(lv[1], lv[p1]), xout = pos[!hi], rule = 2)$y
  out
}

## Inverse of .actaPosToVol(): a volume -> its (fractional) category position, so a fitted value
## that falls BETWEEN two doses can be drawn on the evenly-spaced axis. Returns NA when the volume
## lies outside the plotted series, so callers can omit the line rather than clamp it to the border
## and imply a dose that was never tested.
actaVolToPos <- function(vol, axis_levels) {
  lv <- sort(unique(suppressWarnings(as.numeric(axis_levels))))
  ip <- which(lv > 0)
  if (length(lv) < 2 || !length(ip)) return(rep(NA_real_, length(vol)))
  p1  <- min(ip)
  out <- rep(NA_real_, length(vol))
  v   <- suppressWarnings(as.numeric(vol))
  inr <- is.finite(v) & v >= lv[1] & v <= max(lv)
  hi  <- inr & v >= lv[p1]
  if (any(hi))
    out[hi] <- stats::approx(x = log10(lv[ip]), y = ip, xout = log10(v[hi]), rule = 2)$y
  lo <- inr & !hi
  if (any(lo))
    out[lo] <- stats::approx(x = c(lv[1], lv[p1]), y = c(1, p1), xout = v[lo], rule = 2)$y
  out
}

actaMichaelisMentenCurve <- function(fits, axis_levels, n = 600L) {
  lv <- sort(unique(suppressWarnings(as.numeric(axis_levels))))
  if (length(lv) < 2) return(NULL)
  ok <- fits[isTRUE_vec(fits$MM_ok), , drop = FALSE]
  if (!nrow(ok)) return(NULL)
  pos  <- seq(1, length(lv), length.out = n)
  vols <- .actaPosToVol(pos, lv)
  do.call(rbind, lapply(seq_len(nrow(ok)), function(i) {
    b <- ok$MM_Vmax[i]; k <- ok$MM_Kd[i]
    d <- ok[i, setdiff(names(ok), c("MM_Vmax","MM_Kd","MM_R2","MM_90pct_Vmax","MM_ok","MM_note")),
            drop = FALSE]
    cbind(d[rep(1, length(pos)), , drop = FALSE],
          data.frame(x = pos, SI = b * vols / (k + vols)))
  }))
}

## vapply-based isTRUE over a vector; fits$MM_ok can carry NA when a group failed to fit.
isTRUE_vec <- function(x) !is.na(x) & x


## ======================= CALLABLE ENTRY POINT =======================
## run_acta() makes a version folder runnable from code -- a Shiny app, a test, a scheduler --
## without the caller depending on this.path() resolving under whatever harness is driving it.
##
## WHY SOURCE RATHER THAN EXTRACT. The analysis script is the single source of truth and the .Rmd
## already consumes it by sourcing it into the knit environment and reading its objects. Lifting
## its body into a function would fork that contract and put every downstream reader at risk of a
## silent divergence -- in a pipeline whose documented failure mode is wrong numbers rather than
## an error. Sourcing into a private environment gives the same callable API for a two-line change
## to the script, and the regression gate in tests/ proves the numbers did not move.
##
## The script sets the working directory (process-global). That is tolerable for a single-user
## desktop app and NOT safe for a concurrent server, which is why v1 is a desktop app. The
## original wd is restored on exit either way, including on error.
##
## Returns a list with the accumulators the report and the app both need. `env` carries the whole
## evaluation environment for anything not surfaced explicitly, so adding a field to the script
## does not require editing this function.
## `report` and `plots` are the app's two checkboxes, both ON by default. The titration export is
## NOT optional and has no switch: it is the file the dashboard consumes.
##
## report = TRUE renders the .Rmd, which SOURCES the script -- so one render produces the analysis,
## the PNGs and the PDF. That is how the workflow has always been driven interactively. It is kept
## separable because (a) the regression gate only compares numbers and re-rendering every plot
## through xelatex would roughly double a ~105 s run for a PDF it discards, and (b) the report is
## the most fragile step: it is LaTeX, and this one has already needed texEsc() for `#`/`_` in
## group names. A LaTeX failure must not destroy an otherwise-successful analysis, so a failed
## render is reported via $report_ok/$report_error rather than thrown -- the export is already on
## disk by then, and the objects survive because knitr evaluated the chunks in `env`.
## The report's intermediates, in one place: run_acta() sweeps them after a successful render and
## reads the .log out of them after a failed one, and both have to look in the version folder AND the
## code folder because a render can leave files in either (see run_acta). `ext = "log"` narrows it to
## just the LaTeX log; the default covers every intermediate including the figures DIRECTORY, which an
## extension-only sweep missed -- that left a stray ACTA_Report_<ver>_files/ tree inside an OQ case
## folder and broke the "OQ folder carries no code copies" self-containment check.
actaReportIntermediates <- function(dirs, ext = c("tex", "log", "aux", "out", "toc", "knit[.]md",
                                                 "utf8[.]md"),
                                    include_files_dir = TRUE) {
  dirs <- unique(dirs[nzchar(dirs) & !is.na(dirs)])
  pat  <- sprintf("^ACTA_Report.*[.](%s)$", paste(ext, collapse = "|"))
  files <- unlist(lapply(dirs, function(d) list.files(d, pattern = pat, full.names = TRUE)))
  ## `include_files_dir = FALSE` for callers that COPY rather than delete: the figures directory is a
  ## directory, and file.copy() without recursive = TRUE just fails on it.
  if (!isTRUE(include_files_dir) || identical(ext, "log")) return(files)
  c(files, unlist(lapply(dirs, function(d)
    list.files(d, pattern = "^ACTA_Report.*_files$", full.names = TRUE, include.dirs = TRUE))))
}

run_acta <- function(version_dir = getwd(), report = TRUE, plots = TRUE, quiet = FALSE,
                     code_dir = version_dir) {
  version_dir <- normalizePath(version_dir, mustWork = TRUE)
  ## code_dir holds the script/.Rmd/generator; version_dir holds the layout, FCS and outputs. Equal
  ## in normal use; separated so a diagnostic case can carry only its inputs and be driven by the
  ## working version's code rather than a second copy that has to be kept in sync.
  code_dir <- normalizePath(code_dir, mustWork = TRUE)
  script <- list.files(code_dir, pattern = "^ACTA_Script.*\\.R$", full.names = TRUE)
  if (length(script) != 1)
    stop(sprintf("run_acta: expected exactly one ACTA_Script*.R in '%s'; found %d.",
                 code_dir, length(script)), call. = FALSE)

  ## THE SCRIPT SEES WHATEVER run_acta ITSELF SEES. parent.env(environment()) is this function's
  ## ENCLOSING environment: namespace:ACTA when installed as a package, R_GlobalEnv when the
  ## helpers were source()d into a version folder. So the pipeline script reaches every helper by
  ## lexical scoping in BOTH layouts, and none of them has to be exported to make that work.
  ## Written self-referentially on purpose: asNamespace("ACTA") would bind to whatever ACTA is
  ## INSTALLED, which on a development machine can be a stale release -- exactly the trap that
  ## made smoke_app.R test ACTA 2.90.0 instead of the checkout.
  env <- new.env(parent = parent.env(environment()))
  assign("ACTA_WORK_DIR",    version_dir, envir = env)
  assign("ACTA_CODE_DIR",    code_dir,    envir = env)
  assign("ACTA_WRITE_PLOTS", isTRUE(plots), envir = env)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  report_ok <- NA; report_error <- NA_character_
  t0 <- proc.time()[["elapsed"]]
  if (isTRUE(report)) {
    if (!requireNamespace("rmarkdown", quietly = TRUE))
      stop("run_acta(report = TRUE) needs the 'rmarkdown' package.", call. = FALSE)
    rmd <- list.files(code_dir, pattern = "^ACTA_Report.*\\.Rmd$", full.names = TRUE)
    if (length(rmd) != 1)
      stop(sprintf("run_acta: expected exactly one ACTA_Report*.Rmd in '%s'; found %d.",
                   code_dir, length(rmd)), call. = FALSE)
    ## envir = env so the chunk objects land somewhere we can read afterwards; knit_root_dir so a
    ## programmatic render cannot knit this .Rmd against another version folder's script.
    ##
    ## tinytex.clean = FALSE is the reason the .log now actually survives. render(clean = FALSE)
    ## alone was NOT enough and never was: rmarkdown hands the .tex to tinytex::latexmk(), and
    ## tinytex does its OWN aux-file sweep on failure under `tinytex.clean` (default TRUE), so it
    ## deleted the .log after LaTeX wrote it and before anything could read it. MEASURED on
    ## 2026-08-18: a failed OQ_Test2 render left the .tex but NO .log; re-compiling that same .tex
    ## by hand with tinytex.clean = FALSE produced both the .log and a valid PDF. That is also why
    ## every failure report so far has said only "error in running command" -- there was nothing
    ## left on disk to quote.
    ##
    ## tinytex.verbose is deliberately NOT set: it streams the whole engine transcript to the
    ## console on EVERY render, success included, which would bury the app's own messages. The .log
    ## file is what actaOQFinish() quotes, so preserving it is the whole requirement.
    ## RENDER INTERMEDIATES GO TO LOCAL DISK, NEVER the version folder.
    ##
    ## knitr writes each figure into <intermediates>/..._files/figure-latex/ and xdvipdfmx reads it
    ## back moments later. Every ACTA folder lives in OneDrive CloudStorage, and a just-written file
    ## in a syncing folder is not reliably readable that fast. MEASURED 2026-08-19 on the user's
    ## OQ_Test1: nine of ten figures were included and the tenth failed with
    ##   ! xdvipdfmx:fatal: Image inclusion failed. Could not find file: ...lp1-CD4_CD8-1-1.png
    ## One file out of ten is a race, not a logic error, and the same run passed here every time --
    ## via run_acta(), via actaOQRun(), under --vanilla, and with spaces and an "&" in the path --
    ## the ONLY difference being that my copies were on local /private/tmp. It also matches two
    ## workbooks mutating on disk unprompted the same day and a LaTeX failure the day before that
    ## compiled cleanly on retry, unchanged.
    ##
    ## Intermediates have no business in a synced folder regardless of the cause: they are transient,
    ## they are large (a figure per plot per page), and syncing them is pure cost. Only the PDF still
    ## lands in version_dir, via output_dir. This also removes the .knit.md accumulation that
    ## intermediates_dir = version_dir was introduced to fix.
    .interDir <- file.path(tempdir(), sprintf("acta_render_%s", basename(version_dir)))
    unlink(.interDir, recursive = TRUE)      # a stale figure from a previous attempt must not be reused
    dir.create(.interDir, recursive = TRUE, showWarnings = FALSE)
    .tt <- options(tinytex.clean = FALSE)
    on.exit(options(.tt), add = TRUE)
    report_ok <- tryCatch({
      ## output_dir as well as knit_root_dir: rmarkdown writes beside the INPUT by default, which
      ## would drop the PDF in the code folder when the two differ.
      ## clean = FALSE so the .tex and .log SURVIVE a failure. conditionMessage(e) for a failed PDF
      ## build is only ever "error in running command" -- identical for a missing engine, a missing
      ## pandoc and a LaTeX compile error -- so on its own it identifies nothing. LaTeX writes the
      ## real fault into the .log as a "! ..." line, and actaOQFinish() quotes it from there.
      ##
      ## Reading that .log was attempted once before and never fired, because render() defaults to
      ## clean = TRUE and had already deleted it. Fixing THAT is the whole change. A sink()-based
      ## capture of the console was tried here instead and was strictly worse: it collected nothing,
      ## and its bare on.exit() cleared this function's on.exit(setwd(old_wd)) -- R replaces every
      ## registered handler when on.exit() is called with no arguments -- so the working directory
      ## silently stopped being restored. The simple version is the one that works.
      ## intermediates_dir pins the .knit.md / .tex / .log / figure folder to the RUN's directory.
      ## Without it rmarkdown writes them beside the INPUT .Rmd -- which for an OQ or app run is the
      ## shared CODE folder, not the case folder. clean = FALSE therefore left ACTA_Report_*.knit.md
      ## accumulating in the version folder after every run, and the cleanup below never saw them
      ## because it only looks in version_dir. This also puts a failed render's .log next to the rest
      ## of that run's artefacts, where actaOQFinish() collects from.
      ## START CLEAN. tinytex.clean = FALSE (set above) means a FAILED render now leaves its .aux /
      ## .toc / .out behind -- which is the point, they are the evidence. But LaTeX READS those files
      ## on the next compile, and one truncated by a failed run breaks the next one in the preamble:
      ##   ! LaTeX Error: Missing \begin{document}.
      ##   l.28 o
      ##        n-information}{{}{26}{Session Information}{section*.12}{}}
      ## -- a half-written .out line, nothing to do with the document. MEASURED 2026-08-19: both OQ
      ## cases failed this way, and the "transient" 2_87 failure on 2026-08-18 that compiled fine by
      ## hand was the same thing. So sweep before rendering as well as after: one failure must not be
      ## able to fail the next run.
      unlink(actaReportIntermediates(c(version_dir, code_dir)), recursive = TRUE)
      rmarkdown::render(rmd, envir = env, knit_root_dir = version_dir,
                        output_dir = version_dir, intermediates_dir = .interDir,
                        quiet = isTRUE(quiet), clean = FALSE)
      ## Tidy the intermediates ONLY on success; on failure they are the evidence.
      ## Covers the figures DIRECTORY as well as the loose files. clean = FALSE keeps
      ## ACTA_Report_<ver>_files/ too, and an extension-only sweep left it sitting in the case folder
      ## -- which broke the OQ self-containment check ("OQ folder carries no code copies") and would
      ## have shipped a stray figures tree inside a diagnostic meant to hold only its inputs.
      ##
      ## BOTH folders, not just version_dir. With tinytex.clean = FALSE a SUCCESSFUL render leaves
      ## ACTA_Report_<ver>.log in the CODE folder -- measured on an OQ run, where version_dir is the
      ## case folder and code_dir is the version folder: the .log appeared in the version folder,
      ## which this sweep did not look at. Keeping tinytex's aux files is the point, so they have to
      ## be tidied everywhere they can land or they accumulate in the code folder run after run.
      unlink(actaReportIntermediates(c(version_dir, code_dir)), recursive = TRUE)
      TRUE
    }, error = function(e) {
      ## Name the real fault while the evidence is still on disk. The .log can land in EITHER folder
      ## (see the sweep above), and actaOQFinish() only searches the case folder -- so move it next
      ## to the rest of the run's artefacts and fold its first "! ..." lines into the message here.
      ## Doing it in run_acta means the app gets the same diagnosis the OQ log does.
      msg <- conditionMessage(e)
      ## The .log and .tex now live on local disk, so COPY them back beside the run's artefacts --
      ## that is where actaOQFinish() and the operator look, and a temp dir is not somewhere anyone
      ## will think to check.
      for (f in actaReportIntermediates(.interDir, ext = c("log", "tex"), include_files_dir = FALSE))
        try(file.copy(f, file.path(version_dir, basename(f)), overwrite = TRUE), silent = TRUE)
      for (f in actaReportIntermediates(code_dir, ext = "log"))
        try(file.rename(f, file.path(version_dir, basename(f))), silent = TRUE)
      ln <- unlist(lapply(actaReportIntermediates(version_dir, ext = "log"),
                          function(f) tryCatch(readLines(f, warn = FALSE),
                                               error = function(e) character(0))))
      bang <- unique(trimws(grep("^!|^l[.][0-9]+|LaTeX Error|Fatal error", ln, value = TRUE)))
      bang <- bang[nzchar(bang)]
      report_error <<- if (length(bang))
        paste0(msg, " -- LaTeX said: ", paste(utils::head(bang, 3), collapse = " | ")) else msg
      FALSE
    })
    options(.tt)
  } else if (isTRUE(quiet)) {
    suppressMessages(suppressWarnings(sys.source(script, envir = env)))
  } else {
    sys.source(script, envir = env)
  }
  elapsed <- proc.time()[["elapsed"]] - t0

  pick <- function(nm) if (exists(nm, envir = env, inherits = FALSE)) get(nm, envir = env) else NULL
  ## ok reflects the ANALYSIS, and now actually MEASURES it: the script sets
  ## ACTA_ANALYSIS_COMPLETE as its final statement. It used to be the literal TRUE, which is right
  ## only when the analysis and the render are separate steps -- but with report = TRUE the .Rmd
  ## sources the script, so an analysis failure was caught by the render's tryCatch and returned
  ## ok = TRUE with report_ok = FALSE. That is how a run whose gating died mid-way still presented
  ## as "analysis fine, report failed" and went on to claim an export it never wrote.
  ## A LaTeX/pandoc failure after a complete analysis still gives ok = TRUE with report_ok = FALSE,
  ## because the export and plots are on disk and are usable -- that distinction is the point.
  analysis_ok <- isTRUE(pick("ACTA_ANALYSIS_COMPLETE"))
  out <- list(
    ok            = analysis_ok,
    ## The error text belongs to whichever stage actually failed. When the render tryCatch fired but
    ## the analysis never finished, the message it caught IS the analysis error, and calling it a
    ## report error sends the reader to the wrong place entirely.
    analysis_error = if (!analysis_ok && !is.na(report_error)) report_error else NA_character_,
    report_ok     = report_ok,
    report_error  = report_error,
    wrote_plots   = isTRUE(plots),
    version_dir   = version_dir,
    script        = basename(script),
    script_version = pick("ScriptVersion"),
    elapsed_s     = elapsed,
    seed          = pick("ACTA_SEED"),
    ## analysis results
    stats         = pick("stats"),
    qcList        = pick("qcList"),
    mmFits        = pick("mmFits"),
    antibody      = pick("antibody"),
    gs            = pick("gs"),
    maxValue      = pick("actaMaxValue"),
    transArgs     = pick("actaTransEff"),
    ## report-facing plot collections
    SIPlot        = pick("SIPlot"),      SatPlot      = pick("SatPlot"),
    plotList_hist = pick("plotList_hist"), plotList_pseudo = pick("plotList_pseudo"),
    plotLayoutList = pick("plotLayoutList"), plotExportList = pick("plotExportList"),
    plotDims      = pick("plotDims"),
    reportHist    = pick("reportHist"),  reportPseudo = pick("reportPseudo"),
    reportLayout  = pick("reportLayout"), reportDims  = pick("reportDims"),
    panelList     = pick("panelList"),   panelStr     = pick("panelStr"),
    ## MiFlowCyt metadata, data only -- the report typesets it and the FlowRepository export will
    ## serialise it. Surfaced here so the OQ can assert on the object rather than on rendered prose.
    miflowcyt     = pick("actaMfc"),
    ## artefacts on disk
    ## THIS run's export, by the name the script itself built -- not the newest match in the folder.
    ## Globbing meant a run that failed before writing anything reported a PREVIOUS run's export as
    ## its own, which is the same misreading the `report` field above already guards the PDF against.
    export_file   = { nm <- pick("exportName")
                      p  <- if (length(nm) == 1L && !is.na(nm) && nzchar(nm))
                              file.path(version_dir, basename(nm)) else NA_character_
                      if (!is.na(p) && file.exists(p)) p else NA_character_ },
    plots_dir     = { d <- file.path(version_dir, "Plots"); if (dir.exists(d)) d else NA_character_ },
    env           = env
  )
  out
}

## The three artefacts a run produces, as a named list of paths with an exists flag. The
## completion message names all of them: "Report is ready" alone cannot tell an operator whether
## the export -- the file the dashboard actually consumes -- was written.
## What the Downsample section of the report states, and what the OQ asserts on. Data only.
##
## `n` is Info!Downsample_to (NA = not set) and `read` is the per-sample event counts actually read.
## Both are needed: a file with fewer events than n is read WHOLE, so "downsampled to n" on its own
## would be a claim the data does not support. $TOT is left untouched by flowCore's which.lines, so
## the acquired counts remain available separately -- what this reports is what ACTA READ.
actaDownsampleReport <- function(n = NA_integer_, read = integer(0)) {
  read <- read[!is.na(read)]
  out <- list(requested = if (is.na(n)) NA_integer_ else as.integer(n), n_files = length(read),
              min = if (length(read)) min(read) else NA_integer_,
              max = if (length(read)) max(read) else NA_integer_,
              total = if (length(read)) sum(read) else NA_integer_, capped = 0L, short = 0L)
  if (is.na(n)) {
    out$text <- "No downsampling performed."
    return(out)
  }
  out$capped <- sum(read == n); out$short <- sum(read < n)
  out$text <- sprintf("Downsampled to %s events per FCS file.", format(as.integer(n), big.mark = ","))
  if (out$short)
    out$text <- paste(out$text, sprintf(
      "%d of %d file(s) held fewer than %s events and were read in full (smallest %s).",
      out$short, out$n_files, format(as.integer(n), big.mark = ","), format(out$min, big.mark = ",")))
  out
}

## Did THIS run actually write the titration export? The export is the one artefact with no switch,
## which is why both callers below simply assumed it existed -- and then announced one left behind by
## an earlier run. run_acta() already resolves export_file to a path that exists or NA; this states the
## question once so neither caller has to remember which it is.
actaExportMade <- function(res) {
  p <- res$export_file
  length(p) == 1L && !is.na(p) && nzchar(p) && file.exists(p)
}

actaRunArtefacts <- function(res) {
  vd <- res$version_dir
  ## Only claim the PDF if THIS run produced it. Globbing the folder finds a PDF left by an
  ## earlier run, so a report-less run would otherwise report yesterday's report as ready -- the
  ## precise misreading the completion message exists to prevent.
  pdfs <- if (isTRUE(res$report_ok)) list.files(vd, pattern = "^ACTA_Report.*\\.pdf$", full.names = TRUE)
          else character(0)
  ## Same rule as the PDF: only PNGs from a run that actually completed are THIS run's. Plots/ is
  ## never emptied between runs, so a failed run found the previous one's set sitting there and
  ## reported "Plots (8 PNG)" as its own output. Any partial PNGs are still on disk either way -- what
  ## changes is that the summary stops implying this run produced them.
  pngs <- if (isTRUE(res$wrote_plots) && !isFALSE(res$ok) && !is.na(res$plots_dir))
            list.files(res$plots_dir, pattern = "\\.png$", full.names = TRUE) else character(0)
  skipped <- function(why) list(label = NA_character_, path = NA_character_, skipped = why)
  list(
    report = if (isTRUE(res$report_ok))
               list(label = "Report (PDF)", path = pdfs[[which.max(file.mtime(pdfs))]], skipped = NA_character_)
             ## "render failed" is only true if the render is what failed. When the analysis never
             ## completed there was nothing to render, and blaming LaTeX sends the reader to the
             ## wrong stage -- the same misattribution the run log used to make.
             else if (isFALSE(res$ok))
               list(label = "Report (PDF)", path = NA_character_, skipped = "analysis did not complete")
             else if (isFALSE(res$report_ok))
               list(label = "Report (PDF)", path = NA_character_, skipped = "render failed")
             else list(label = "Report (PDF)", path = NA_character_, skipped = "not requested"),
    export = if (actaExportMade(res))
               list(label = "Titration export", path = res$export_file, skipped = NA_character_)
             else list(label = "Titration export", path = NA_character_, skipped = "not written"),
    ## Plots were REQUESTED but none exist -> say so. Leaving `skipped` NA alongside a NA path made
    ## the UI print the literal "Plots (0 PNG) -- NA", since it falls back to `skipped` whenever there
    ## is no path.
    plots  = if (isTRUE(res$wrote_plots) && length(pngs))
               list(label = sprintf("Plots (%d PNG)", length(pngs)), path = res$plots_dir,
                    skipped = NA_character_)
             else if (isTRUE(res$wrote_plots))
               list(label = "Plots", path = NA_character_,
                    skipped = if (isFALSE(res$ok)) "analysis did not complete" else "none written")
             else list(label = "Plots", path = NA_character_, skipped = "not requested")
  )
}

## The completion message, built from what was ACTUALLY produced rather than from what was asked
## for: "Report and Dashboard are ready" must not appear when the render failed.
actaReadyMessage <- function(res, dashboard_ok = FALSE) {
  made <- c(if (isTRUE(res$report_ok)) "Report", if (isTRUE(dashboard_ok)) "Dashboard")
  ## "Titration export is ready" used to be the unconditional fallback, so a run that produced
  ## nothing at all still ended on a sentence naming an artefact. Say what happened instead.
  if (!length(made))
    return(if (actaExportMade(res)) "Titration export is ready"
           else if (isFALSE(res$ok)) "Analysis did not complete -- nothing was written"
           else "No report, dashboard or titration export was produced")
  sprintf("%s %s ready", paste(made, collapse = " and "), if (length(made) > 1) "are" else "is")
}


## ======================= APP SUPPORT =======================

## Launch the desktop app from an INSTALLED package. This is the entry point that the app's
## CODE_DIR / WORK_DIR split exists to make possible:
##
##     library(ACTA); acta_app()          # work in the current directory
##     acta_app("~/titrations/EXP123")    # or somewhere specific
##
## `work_dir` becomes the app's WORK_DIR via ACTA_WORK_DIR -- the folder holding the instructions
## workbook, Titration_FCS/, the exports and the run log. The CODE stays where it was installed and
## is never written to, which is the point: a package library is often read-only, and putting a run
## log inside it would fail on a managed machine.
##
## Returns nothing; it blocks until the app window is closed, like shiny::runApp().
acta_app <- function(work_dir = getwd(), launch.browser = TRUE, ...) {
  if (!requireNamespace("shiny", quietly = TRUE))
    stop("acta_app(): the 'shiny' package is required.", call. = FALSE)
  work_dir <- normalizePath(work_dir, mustWork = TRUE)
  app <- system.file("pipeline", "ACTA_App.R", package = "ACTA")
  if (!nzchar(app) || !file.exists(app))
    stop("acta_app(): ACTA_App.R not found in the installed package.", call. = FALSE)
  old <- Sys.getenv("ACTA_WORK_DIR", unset = NA_character_)
  Sys.setenv(ACTA_WORK_DIR = work_dir)
  on.exit(if (is.na(old)) Sys.unsetenv("ACTA_WORK_DIR") else Sys.setenv(ACTA_WORK_DIR = old),
          add = TRUE)
  message("ACTA app: working folder ", work_dir)
  shiny::runApp(app, launch.browser = launch.browser, ...)
}
## Non-UI logic for ACTA_App.R lives here, as plain functions: it is testable without a browser,
## and it keeps the Shiny file a thin layer over behaviour that can be exercised from the console.

## --- status records, shared vocabulary with actaPrevalidateRecords() ------------------------
## FOUR states, never colour alone. A skip is not a pass.
ACTA_GLYPH <- c(pass = "\u2713", fail = "\u2717", warn = "!", skip = "\u2013")
.appRec <- function(id, label, status, detail = NA_character_, where = NA_character_)
  list(id = id, label = label, status = status, detail = detail, where = where)

## --- which version files is the app driving? -------------------------------------------------
## Mirrors the rule the script itself enforces: EXACTLY ONE match. A leftover
## ACTA_Script_2_86_backup.R alongside the real one is fatal to the script, so it must be fatal
## here too rather than the app silently driving whichever sorted first. The resolved filename and
## its mtime are surfaced so the operator can see WHICH version is about to run before pressing go.
## `code_dir` separates the four SHIPPED files from the one the USER supplies. They are the same
## directory in a version folder (hence the default), and different when ACTA runs from an installed
## package: the scripts live read-only in inst/pipeline/ while the workbook sits in the user's own
## folder. Checking all five in one place is what made the app unusable from a package.
actaVersionFiles <- function(version_dir, code_dir = version_dir) {
  want <- list(
    script    = list(label = "ACTA Script",     pat = "^ACTA_Script.*\\.R$",    dir = "code"),
    functions = list(label = "ACTA Functions",  pat = "^ACTA_Function.*\\.R$",  dir = "code"),
    dashboard = list(label = "ACTA Dashboard",  pat = "^ACTA_Dashboard.*\\.R$", dir = "code"),
    report    = list(label = "ACTA Report",     pat = "^ACTA_Report.*\\.Rmd$",  dir = "code"),
    layout    = list(label = "Instructions workbook", pat = "Titration_Instructions.*\\.xlsx$",
                     dir = "work")
  )
  lapply(names(want), function(k) {
    w   <- want[[k]]
    dir <- if (identical(w$dir, "code")) code_dir else version_dir
    hit <- list.files(dir, pattern = w$pat, full.names = TRUE)
    hit <- hit[!grepl("^~\\$", basename(hit))]          # skip Excel lock files
    if (length(hit) == 1)
      .appRec(k, w$label, "pass",
              sprintf("%s  (%s)", basename(hit), format(file.mtime(hit), "%Y-%m-%d %H:%M")), hit)
    else if (!length(hit)) {
      ## The helper library is the ONE entry that has a second legitimate source: when ACTA runs from
      ## an installed package, inst/pipeline/ deliberately carries no ACTA_Functions.R and the helpers
      ## come from the namespace instead. Reporting that as a missing file would make a correct
      ## package install look broken in the app's very first check. Every other entry is a real file
      ## or nothing.
      if (identical(k, "functions") && requireNamespace("ACTA", quietly = TRUE))
        .appRec(k, w$label, "pass",
                sprintf("from the installed ACTA package (%s)", utils::packageVersion("ACTA")),
                dir)
      ## THE USER-SUPPLIED WORKBOOK IS *SKIP* WHEN ABSENT, NOT FAIL. This rule exists to catch
      ## AMBIGUITY -- two workbooks in one folder, where the app cannot know which to read, which is
      ## the `length(hit) > 1` branch below and stays fatal. ABSENCE is a different thing entirely:
      ## it is what a fresh clone looks like. The workbook carries real experiment data, so it is
      ## gitignored and can never ship, which meant a newly cloned repo FAILED 2 of 11 test scripts
      ## on the first thing a new user does -- discovered 2026-09-04 by building the public export.
      ##
      ## Deliberately NOT solved by shipping a dummy workbook: the OQ cases under Diagnostics/ are
      ## already the worked examples, complete with expected values, and a second example at the
      ## repo root would compete with them for the reader's attention.
      ##
      ## The four SHIPPED files keep failing when absent -- those are ours, and their absence is a
      ## broken install, not an empty working folder. Hence the `w$dir` split rather than one rule.
      else if (identical(w$dir, "work"))
        .appRec(k, w$label, "skip",
                paste("none in the working folder yet -- put your instructions workbook here, or",
                      "copy one from Template/ (see Diagnostics/OQ_Test* for worked examples)"), dir)
      else
        .appRec(k, w$label, "fail",
                sprintf("not found in %s", if (identical(w$dir, "code")) "the code folder"
                                           else "the working folder"), dir)
    }
    else
      .appRec(k, w$label, "fail",
              sprintf("%d matches -- exactly one is required: %s", length(hit),
                      paste(basename(hit), collapse = ", ")), dir)
  })
}
actaVersionFile <- function(recs, id) {
  r <- Filter(function(x) x$id == id, recs)
  if (length(r) == 1 && r[[1]]$status == "pass") r[[1]]$where else NA_character_
}

## --- dashboard template contract -------------------------------------------------------------
## Four checkable properties. The placeholder count is the one that matters most: the generator
## substitutes with sub(), FIRST MATCH ONLY, so a token named twice (in a comment, say) swallows
## the substitution and leaves the real one unreplaced -- a dashboard that is broken on arrival.
ACTA_TPL_TOKENS <- c("__META_JSON__", "__DATA_JSON__", "__COLS_JSON__", "__GROUPABLE_JSON__")
actaValidateDashboardTemplate <- function(path) {
  if (is.na(path) || !nzchar(path) || !file.exists(path))
    return(list(.appRec("tpl_file", "Dashboard template found", "fail",
                        if (is.na(path) || !nzchar(path)) "no template selected" else "file does not exist", path)))
  txt <- tryCatch(paste(readLines(path, warn = FALSE), collapse = "\n"),
                  error = function(e) NA_character_)
  if (is.na(txt))
    return(list(.appRec("tpl_file", "Dashboard template readable", "fail", "could not be read", path)))
  out <- list(.appRec("tpl_file", "Dashboard template found", "pass", basename(path), path))

  n <- vapply(ACTA_TPL_TOKENS, function(tk) lengths(regmatches(txt, gregexpr(tk, txt, fixed = TRUE))), integer(1))
  bad <- n[n != 1]
  out <- c(out, list(if (!length(bad))
    .appRec("tpl_tokens", "Each JSON placeholder appears exactly once", "pass",
            paste(ACTA_TPL_TOKENS, collapse = ", "), path)
    else .appRec("tpl_tokens", "Each JSON placeholder appears exactly once", "fail",
                 paste(sprintf("%s x%d", names(bad), bad), collapse = "; "), path)))

  ## The generator uppercases column headers; CSS text-transform maps U+00B5 MICRO SIGN to
  ## U+039C GREEK CAPITAL MU, so an uppercased "(uL)" silently renders as "(ML)" -- microlitres
  ## reported as millilitres.
  ##
  ## TWO HALVES, and both are checked. The CSS rule alone is not a guard: the generator marks a unit
  ## column with `unit: true` in COLS, and the template's own script has to turn that into a class on
  ## the <th>. Three templates in Other_Templates -- including `ACTA_dashboard_template_navy-chrome` -- carried the rule and
  ## NEVER APPLIED THE CLASS, and passed this check for months because it only looked for the rule.
  ## A styled-but-unapplied guard is the worst of the three states: it reads as protected.
  hasRule  <- grepl("text-transform:\\s*none", txt)
  applies  <- grepl("c\\.unit", txt) || grepl("\\.unit\\s*[?)]", txt)
  out <- c(out, list(
    if (hasRule && applies)
      .appRec("tpl_unit_guard", "Unit guard present (protects \u00b5L from uppercasing)", "pass",
              NA_character_, path)
    else if (hasRule)
      .appRec("tpl_unit_guard", "Unit guard present (protects \u00b5L from uppercasing)", "fail",
              paste("the `text-transform: none` rule is there but nothing applies the class: the",
                    "script never reads COLS' `unit` flag, so a \u00b5L header WILL render as ML."),
              path)
    else
      .appRec("tpl_unit_guard", "Unit guard present (protects \u00b5L from uppercasing)", "warn",
              "no `text-transform: none` rule found; an uppercased \u00b5L header may render as ML",
              path)))

  ext <- regmatches(txt, gregexpr('(?:src|href)\\s*=\\s*["\']https?:', txt, perl = TRUE))[[1]]
  out <- c(out, list(if (!length(ext))
    .appRec("tpl_selfcontained", "Self-contained (no external requests)", "pass", NA_character_, path)
    else .appRec("tpl_selfcontained", "Self-contained (no external requests)", "fail",
                 sprintf("%d external reference(s); the dashboard is opened from a synced folder",
                         length(ext)), path)))
  out
}

## --- export scan --------------------------------------------------------------------------
## The count alone is not the useful signal. What an operator needs before rebuilding is: how many
## exports are there, how many are NEW since the last dashboard build, and is anything unreadable
## or duplicated for the same Benchling ID.
## Will the dashboard actually SEE what we copied?
##
## The copy destination and the scan folder are separate fields, so they can point at unrelated
## places. When they do, the copy succeeds, the dashboard generates, and it shows a run from before
## the copy -- which is what "the report has the titer but the dashboard does not" looks like from
## the outside. Nothing errors, so without this check the operator has no way to tell.
##
## Reachable means: the scan folder itself (its root counts as a run), an immediate subfolder of it,
## or that subfolder's ACTA_OUTPUT_SUBDIR -- exactly the set the generator walks. WARN rather than
## fail: copying somewhere the dashboard does not index is unusual but legitimate (an archive drop,
## a shared folder someone else scans), so it must not block the run.
## Is there a PDF engine on this machine?
##
## A CAPABILITY check, deliberately not a package check. requireNamespace("tinytex") would be wrong
## in both directions: the tinytex PACKAGE installs in seconds while the TeX DISTRIBUTION is a
## separate ~100 MB step (tinytex::install_tinytex()), so the package alone gives a green row and a
## failed render; and anyone with MacTeX / TeX Live / MiKTeX has a working engine and no tinytex
## package at all, so requiring it would fail a machine that is perfectly fine.
##
## This exists because a colleague's fresh install failed both OQ cases with every pre-flight green:
## 11 of 12 assertions passed, all numbers matched the baselines, and the only failure was
## rmarkdown's "error in running command" -- a message naming neither the cause nor the cure.
actaPdfEngine <- function() {
  eng <- Sys.which(c("pdflatex", "xelatex", "lualatex", "tectonic"))
  eng <- eng[nzchar(eng)]
  if (length(eng)) return(list(ok = TRUE, how = sprintf("%s (%s)", names(eng)[1], unname(eng)[1])))
  ## is_tinytex() tests the DISTRIBUTION, which is the thing that matters.
  if (requireNamespace("tinytex", quietly = TRUE) &&
      isTRUE(tryCatch(tinytex::is_tinytex(), error = function(e) FALSE)))
    return(list(ok = TRUE, how = "TinyTeX"))
  list(ok = FALSE, how = NA_character_)
}

## Severity follows the Report checkbox. XQuartz beside this is unconditionally fatal; LaTeX is not
## -- plots, the export, the dashboard and every QC number are produced without it, so a missing
## engine must not block a run that was never going to render a PDF.
actaPdfToolchainRec <- function(report_wanted = TRUE) {
  id <- "pdf_engine"; label <- "LaTeX engine (PDF report only)"
  e <- actaPdfEngine()
  if (isTRUE(e$ok)) return(.appRec(id, label, "pass", e$how, NA_character_))
  .appRec(id, label, if (isTRUE(report_wanted)) "fail" else "warn",
          paste0("no LaTeX engine found. TWO steps -- the package alone is not enough:  ",
                 'install.packages("tinytex")  then  tinytex::install_tinytex()',
                 "  .  Plots, export, dashboard and all QC numbers work without it."),
          NA_character_)
}

actaCopyDestReachable <- function(dest, scan) {
  id <- "copy_reachable"; label <- "Copy destination is indexed by the scan folder"
  blank <- function(x) is.null(x) || is.na(x) || !nzchar(x)
  if (blank(dest) || blank(scan))
    return(.appRec(id, label, "skip", "copy destination or scan folder not set", dest))
  n <- function(x) tryCatch(normalizePath(x, winslash = "/", mustWork = FALSE),
                            error = function(e) x)
  d <- sub("/+$", "", n(dest)); s <- sub("/+$", "", n(scan))
  ok <- identical(d, s) ||
        identical(dirname(d), s) ||
        (identical(basename(d), ACTA_OUTPUT_SUBDIR) && identical(dirname(dirname(d)), s))
  if (ok) return(.appRec(id, label, "pass", d, d))
  .appRec(id, label, "warn",
          sprintf(paste0("copying to '%s', but the dashboard indexes '%s' and its immediate ",
                         "subfolders (plus their %s/). The new export will not appear -- the ",
                         "dashboard will show whatever is already there."),
                  d, s, ACTA_OUTPUT_SUBDIR), d)
}

actaScanExports <- function(dir, dashboard_html = NA_character_) {
  if (is.na(dir) || !nzchar(dir) || !dir.exists(dir))
    return(list(recs = list(.appRec("exp_dir", "Export folder exists", "fail",
                                    if (is.na(dir) || !nzchar(dir)) "no folder selected" else "folder does not exist", dir)),
                files = character(0)))
  ## The generator treats each IMMEDIATE SUBFOLDER of this directory as one run and scans it for an
  ## export. Globbing the whole tree here would report a count the generator will not reproduce --
  ## the app saying 12 while the dashboard indexes 3. Match its rule instead: the run root plus
  ## ACTA_OUTPUT_SUBDIR, nothing deeper.
  ##
  ## This used to pass recursive = TRUE, which is exactly the divergence the comment warns about:
  ## once the OQ runner started collecting into Outputs/, this found 3 exports under Diagnostics/
  ## and reported PASS while the generator found 0 and wrote an EMPTY dashboard. A green pre-flight
  ## followed by an empty artefact is worse than either half being wrong on its own.
  runs <- list.dirs(dir, recursive = FALSE, full.names = TRUE)
  runs <- runs[!grepl("archive", runs, ignore.case = TRUE)]
  f <- unlist(lapply(runs, function(r)
    list.files(c(r, file.path(r, ACTA_OUTPUT_SUBDIR)), pattern = "TitrationExport.*\\.xlsx$",
               full.names = TRUE)),
    use.names = FALSE)
  f <- c(f, list.files(dir, pattern = "TitrationExport.*\\.xlsx$", full.names = TRUE))  # loose in the root
  f <- unique(f[!grepl("^~\\$", basename(f)) & !grepl("archive", f, ignore.case = TRUE)])
  recs <- list(.appRec("exp_dir", "Export folder exists", "pass", dir, dir))
  if (!length(f)) {
    ## WARN, not fail. An empty folder is the normal state the first time a run is written -- there
    ## is nothing to index yet, and blocking the button would stop the very run that fixes it.
    recs <- c(recs, list(.appRec("exp_found", "Titration exports detected", "warn",
                                 "No titration exports detected in the titration exports folder",
                                 dir)))
    return(list(recs = recs, files = f))
  }
  bid <- sub(".*?(EXP[0-9]{8}).*", "\\1", basename(f)); bid[!grepl("^EXP[0-9]{8}$", bid)] <- NA
  dup <- unique(bid[duplicated(bid) & !is.na(bid)])
  newer <- if (!is.na(dashboard_html) && file.exists(dashboard_html))
             sum(file.mtime(f) > file.mtime(dashboard_html)) else NA_integer_
  recs <- c(recs, list(.appRec("exp_found", "Titration exports detected", "pass",
                               sprintf("%d export(s) in %d run folder(s), %d ELN ID(s)%s", length(f), length(runs),
                                       length(unique(bid[!is.na(bid)])),
                                       if (!is.na(newer)) sprintf("; %d newer than the current dashboard", newer) else ""),
                               dir)))
  recs <- c(recs, list(if (!length(dup))
    .appRec("exp_dup", "No duplicate ELN IDs", "pass", NA_character_, dir)
    else .appRec("exp_dup", "No duplicate ELN IDs", "warn",
                 sprintf("more than one export for: %s -- the newest wins", paste(dup, collapse = ", ")), dir)))
  list(recs = recs, files = f)
}

## --- titer source options -------------------------------------------------------------------
## Two derived recommendations per antibody group. "90%Vmax-based" is DISABLED, with the reason
## shown, when the Michaelis-Menten fit cannot support it: the model is a saturating isotherm and an SI
## curve that turns over cannot be represented by one, in which case Vmax is pushed above every
## observed SI and 9*Km lands outside the doses actually tested. Handing that back as a one-click
## titer would recommend a concentration nobody measured.
actaTiterOptions <- function(mmFits, stats, r2_threshold = 0.9) {
  if (is.null(mmFits) || !nrow(mmFits)) return(list())
  key <- if ("Dirname" %in% names(mmFits)) "Dirname" else names(mmFits)[1]
  lapply(seq_len(nrow(mmFits)), function(i) {
    ab   <- as.character(mmFits[[key]][i])
    vols <- suppressWarnings(as.numeric(stats$StainQty[stats$Dirname == ab]))
    vols <- vols[is.finite(vols)]
    maxV <- suppressWarnings(as.numeric(stats$maxSI_StainQty[stats$Dirname == ab][1]))
    r2   <- suppressWarnings(as.numeric(mmFits$MM_R2[i]))
    v90  <- suppressWarnings(as.numeric(mmFits$MM_90pct_Vmax[i]))
    why <- if (!is.finite(v90) || !is.finite(r2))
             sprintf("Michaelis-Menten fit unavailable%s",
                     if (!is.null(mmFits$MM_note) && !is.na(mmFits$MM_note[i]))
                       sprintf(" -- %s", mmFits$MM_note[i]) else "")
           else if (r2 < r2_threshold)
             sprintf("fit quality too low (R\u00b2 = %.3f, threshold %.2f)", r2, r2_threshold)
           else if (length(vols) && (v90 < min(vols) || v90 > max(vols)))
             sprintf("%.3g \u00b5L is outside the tested range (%.3g\u2013%.3g \u00b5L)",
                     v90, min(vols), max(vols))
           else NA_character_
    list(dirname = ab,
         maxsi   = list(value = maxV, enabled = is.finite(maxV),
                        label = if (is.finite(maxV)) sprintf("Max SI-based (%.3g \u00b5L)", maxV)
                                else "Max SI-based (unavailable)"),
         vmax90  = list(value = v90, enabled = is.na(why), reason = why, r2 = r2,
                        label = if (is.na(why)) sprintf("90%%Vmax-based (%.3g \u00b5L)", v90)
                                else sprintf("90%%Vmax-based \u2014 unavailable: %s", why)))
  })
}

## --- archive-then-copy ------------------------------------------------------------------------
## Never overwrite in place: move what is there into Archived/<timestamp>_<ELN_ID>/ first. A FLAT
## Archived/ would just relocate the problem -- the second re-run would overwrite the first
## archive -- hence the per-run discriminator.
##
## Both dashboard generators skip any path matching "archive" case-insensitively, so archived runs
## drop out of the dashboard automatically. That exclusion is load-bearing: renaming this folder to
## anything without "archive" in it would resurface every superseded run as live dashboard rows.
##
## copy.date = TRUE because the generator orders runs by export mtime; without it every copied file
## is stamped "now" and copy order silently outranks analysis order.
actaArchiveThenCopy <- function(src_paths, dest_dir, eln_id = "run", stamp = NULL, dry_run = FALSE) {
  stamp <- if (is.null(stamp)) format(Sys.time(), "%Y%m%d_%H%M%S") else stamp
  src_paths <- src_paths[!is.na(src_paths) & nzchar(src_paths) & file.exists(src_paths)]
  if (!length(src_paths)) return(list(ok = FALSE, why = "nothing to copy", archived = character(0), copied = character(0)))
  ## A source ALREADY IN the destination is dropped. Copying a file onto itself is a no-op at best,
  ## but the archive step below runs FIRST and MOVES a clashing target into Archived/ -- so with
  ## src and dest the same folder it moved the run's own export and report away and the copy then
  ## failed with "No such file or directory": ok=FALSE, copied=0, working folder empty, output
  ## sitting in Archived/ where nobody would look. Reachable from the app by ticking "Copy ACTA
  ## output ..." and leaving the copy path blank, which resolves to the folder ACTA just wrote to.
  n <- function(x) normalizePath(x, winslash = "/", mustWork = FALSE)
  ## Anywhere INSIDE the destination tree counts as in place, not just directly in it.
  ##
  ## Comparing dirname() only caught files sitting immediately in dest. The run's plots live in
  ## <root>/Plots, so with a blank copy path (dest = <root>) every PNG had dirname <root>/Plots,
  ## failed the test, and was copied to <root>/x.png -- duplicating all eight plots at the top level
  ## of the version folder alongside a dashboard html and the export. That is what "rogue files in
  ## 2_88" turned out to be, and it recurs on every run with a blank copy destination.
  dn  <- n(dest_dir)
  sp  <- n(src_paths)
  inPlace   <- sp == dn | startsWith(sp, paste0(dn, "/"))
  in_place  <- src_paths[inPlace]
  src_paths <- src_paths[!inPlace]
  if (!length(src_paths))
    return(list(ok = TRUE, why = "already in place -- nothing to copy", archive_dir = NA_character_,
                archived = character(0), copied = character(0), in_place = in_place))
  if (!dir.exists(dest_dir) && !dry_run)
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  targets  <- file.path(dest_dir, basename(src_paths))
  clash    <- targets[file.exists(targets)]
  ## eln_id IS A WORKBOOK CELL and becomes a directory NAME here, so it is sanitised at the point of
  ## use as well as by the caller. Unsanitised, `Info!ELN_ID` of "../../../x" made this create
  ## directories outside dest_dir and MOVE a clashing file there -- a workbook choosing where the
  ## run's report and export land (demonstrated 2026-09-08). The `stamp_` prefix does not help: the
  ## first ".." becomes a literal "<stamp>_.." directory that dir.create(recursive=) happily makes,
  ## and every "../" after it climbs for real.
  eln_id   <- actaElnLabel(eln_id, for_file = TRUE)
  ## Belt: assert the name is a single path SEGMENT. Checked structurally rather than by comparing
  ## normalizePath() prefixes -- on macOS the destination exists and normalises /var -> /private/var
  ## while the not-yet-created archive path cannot, so a prefix compare rejects a perfectly contained
  ## path. Asserting "no separator" is exact, and cannot false-positive on a symlinked temp dir.
  if (grepl("[/\\\\]", eln_id) || eln_id %in% c(".", ".."))
    stop(sprintf("actaArchiveThenCopy: ELN id is not usable as a folder name (%s)", eln_id), call. = FALSE)
  archDir  <- file.path(dest_dir, "Archived", sprintf("%s_%s", stamp, eln_id))
  if (dry_run)
    return(list(ok = TRUE, dry_run = TRUE, archive_dir = archDir,
                would_archive = clash, would_copy = targets))
  archived <- character(0)
  if (length(clash)) {
    dir.create(archDir, recursive = TRUE, showWarnings = FALSE)
    okMove <- file.rename(clash, file.path(archDir, basename(clash)))
    archived <- clash[okMove]
    if (any(!okMove)) {                       # rename fails across volumes; fall back to copy+unlink
      left <- clash[!okMove]
      if (all(file.copy(left, file.path(archDir, basename(left)), copy.date = TRUE))) {
        unlink(left); archived <- c(archived, left)
      }
    }
  }
  okCopy <- file.copy(src_paths, dest_dir, overwrite = TRUE, copy.date = TRUE)
  list(ok = all(okCopy), archive_dir = if (length(archived)) archDir else NA_character_,
       archived = archived, copied = targets[okCopy],
       failed = src_paths[!okCopy])
}

## --- run log ----------------------------------------------------------------------------------
## Append-only, TSV, outside the archive/overwrite path. Records FAILED and ABORTED runs too: a log
## that only holds successes implies a clean history that never happened. The workbook hash is what
## lets a result be tied to the exact layout that produced it.
## ======================= DIAGNOSTIC CAPTURE =======================
## The ask this answers: when a run fails on someone else's machine, they should be able to send
## ONE artefact and it should be enough to troubleshoot from. Plan:
## Plans/260827_error-capture-and-diagnostic-bundle.md (gitignored -- it names real incidents).
##
## THREE WRITE POINTS, because both mechanisms that existed wrote their report after the patient
## recovered. Every `actaRunLogAppend` call site fired only once an outcome was known, so a hard
## crash left NO ROW AT ALL and the ledger showed no evidence the run had happened. And the child's
## error text -- the single most useful thing -- lived only in `rv$log`, on screen, with no
## download handler anywhere in the app.
##
##   P1 at start   `outcome = "started"` row + this file's header. The only moment guaranteed to
##                 exist. A `started` row with no terminal row IS the crash signal.
##   P2 during     the child's stdout/stderr teed here line by line as the poller reads it, so a
##                 kill -9 still leaves everything up to the last line.
##   P3 at end     an explicit closing marker. A missing marker means "incomplete", which is
##                 information rather than an absence.
##
## Where it lands: `<work_dir>/Diagnostics_log/`, falling back to tempdir() when that is not
## writable -- a sync client holding the folder must never abort a run -- and the resolved path is
## always returned so the caller can SAY it. Per-run timestamped filenames, append-only: on
## 2026-08-27 files were deleted under a live run and produced 7 spurious FAILs, so nothing here
## ever rewrites a shared file.
actaDiagLogPath <- function(work_dir, action = "run") {
  stamp <- format(Sys.time(), "%y%m%d_%H%M%S")
  nm    <- sprintf("acta_%s_%s.log", action, stamp)
  for (d in c(file.path(work_dir, "Diagnostics_log"), file.path(tempdir(), "ACTA_Diagnostics_log"))) {
    ok <- tryCatch({ dir.create(d, recursive = TRUE, showWarnings = FALSE)
                     f <- file.path(d, nm); cat("", file = f); file.exists(f) },
                   error = function(e) FALSE, warning = function(w) FALSE)
    if (isTRUE(ok)) return(file.path(d, nm))
  }
  NA_character_
}

## Append lines to a diagnostic log. UTF-8 connection explicitly, and never sink(): on 2026-08-27
## an OQ log.txt began with the fragment `" You didn"` -- console text interleaving into our file,
## a second sighting, still unexplained. A write failure is swallowed on purpose; a diagnostic that
## aborts the thing it is diagnosing is worse than no diagnostic.
actaDiagAppend <- function(path, lines) {
  if (is.null(path) || is.na(path) || !nzchar(path)) return(invisible(FALSE))
  tryCatch({
    con <- file(path, open = "at", encoding = "UTF-8"); on.exit(close(con), add = TRUE)
    writeLines(enc2utf8(as.character(lines)), con, useBytes = TRUE)
    invisible(TRUE)
  }, error = function(e) invisible(FALSE))
}

## THE FIRST error, not the last. On 2026-08-27 one missing PNG cascaded into four downstream
## FAILs and only the FIRST line explained it, so a tail-only report actively misleads. Returns ""
## when nothing in the text looks like an error, rather than guessing.
actaFirstError <- function(lines) {
  if (!length(lines)) return("")
  hit <- grep("^(Error|! |Quitting from|.*: Error in )", lines)
  if (!length(hit)) hit <- grep("(?i)error", lines, perl = TRUE)
  if (!length(hit)) return("")
  paste(lines[hit[[1]]:min(length(lines), hit[[1]] + 4L)], collapse = "\n")
}

## Reduce identity without losing meaning: absolute paths become BASENAMES -- dropping which file
## failed would defeat the purpose -- and anything matching the leak patterns the sanitisation gate
## already uses (`Users/<name>`, email) is masked. The header says the bundle was scrubbed so a
## recipient knows what to expect.
actaDiagScrub <- function(lines) {
  x <- as.character(lines)
  x <- gsub("(/[A-Za-z0-9._ +&-]+)+/([A-Za-z0-9._+-]+\\.[A-Za-z0-9]+)", "\\2", x, perl = TRUE)
  x <- gsub("Users/[A-Za-z0-9._-]+", "Users/<redacted>", x, perl = TRUE)
  x <- gsub("[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}", "<redacted-email>", x, perl = TRUE)
  x
}

## P4: one pasteable text file. Sections most-likely-useful first, because people read the top.
##
## EVERY COLLECTOR IS INDIVIDUALLY WRAPPED AND ITS FAILURE RECORDED IN THE BUNDLE. A diagnostic
## that dies while diagnosing hides the original error, which is worse than none. Two collectors
## are known hazards and are treated accordingly:
##   * actaTlmgrStatus() shells to a Perl script -- a colleague's PATH typo once made `perl`
##     invisible to R -- so it is TIME-BOXED through processx when that is available, not merely
##     tryCatch'd, since a hang is not an error.
##   * actaDependencyChecks() calls requireNamespace(), which LOADS packages, and a broken install
##     can take R down with it. It therefore runs LAST: everything above it is already on disk.
actaDiagnosticBundle <- function(work_dir, version_dir, run_log = NA_character_,
                                 child_log = NA_character_, layout = NA_character_,
                                 res = NULL, tsv = NA_character_, tlmgr_timeout = 10) {
  manifest <- character(0)
  sec <- function(title, body, name = title) {
    okd <- !inherits(body, "try-error") && length(body)
    manifest <<- c(manifest, sprintf("  %-28s %s", name,
                                     if (inherits(body, "try-error")) paste("FAILED:", trimws(body))
                                     else if (!length(body)) "empty" else sprintf("%d line(s)", length(body))))
    c(paste0("== ", title, " =="), if (okd) as.character(body) else
        paste0("(", if (inherits(body, "try-error")) paste("collector failed:", trimws(body))
                    else "nothing to report", ")"), "")
  }
  grab <- function(expr) tryCatch(expr, error = function(e) {
    z <- paste("collector failed:", conditionMessage(e)); class(z) <- "try-error"; z })

  kid <- grab(if (!is.na(child_log) && file.exists(child_log))
                readLines(child_log, warn = FALSE) else character(0))
  first <- grab(actaFirstError(if (inherits(kid, "try-error")) character(0) else kid))

  out <- c("======== ACTA DIAGNOSTIC BUNDLE ========",
           sprintf("written        : %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
           sprintf("version_dir    : %s", basename(version_dir)),
           sprintf("script_version : %s", if (!is.null(res) && length(res$script_version))
                                            as.character(res$script_version) else "unknown"),
           sprintf("seed           : %s", if (!is.null(res) && length(res$seed))
                                            as.character(res$seed) else "unknown"),
           sprintf("elapsed_s      : %s", if (!is.null(res) && length(res$elapsed_s))
                                            as.character(round(res$elapsed_s, 1)) else "unknown"),
           sprintf("layout         : %s", if (!is.na(layout)) basename(layout) else "unknown"),
           sprintf("layout_md5     : %s", if (!is.na(layout) && file.exists(layout))
                                            unname(tools::md5sum(layout)) else "unknown"),
           "paths reduced to basenames; usernames and emails masked. NOT a substitute for the",
           "workbook, which is never included -- it is proprietary.", "")
  out <- c(out, sec("FIRST ERROR", if (inherits(first, "try-error")) first
                    else if (nzchar(first)) strsplit(first, "\n")[[1]] else character(0)))
  out <- c(out, sec("RUN LEDGER rows for this run",
                    grab(if (!is.na(tsv) && file.exists(tsv)) {
                      tl <- readLines(tsv, warn = FALSE); utils::tail(tl, 6L)
                    } else character(0)), "run ledger"))
  out <- c(out, sec("CHILD STDOUT/STDERR (full)", kid, "child log"))
  out <- c(out, sec("sessionInfo()", grab(utils::capture.output(utils::sessionInfo())), "sessionInfo"))
  out <- c(out, sec("TeX: tlmgr", grab({
    if (requireNamespace("processx", quietly = TRUE) && nzchar(Sys.which("tlmgr"))) {
      r <- processx::run("tlmgr", "--version", timeout = tlmgr_timeout, error_on_status = FALSE)
      c(sprintf("exit %s", r$status), strsplit(r$stdout, "\n")[[1]])
    } else utils::capture.output(str(actaTlmgrStatus()))
  }), "tlmgr (time-boxed)"))
  out <- c(out, sec("TeX: packages the report needs",
                    grab(actaLatexPackages(version_dir)), "latex packages"))
  out <- c(out, sec("RETAINED LaTeX LOG", grab({
    lg <- list.files(work_dir, pattern = "[.]log$", full.names = TRUE, recursive = FALSE)
    if (length(lg)) readLines(lg[[which.max(file.mtime(lg))]], warn = FALSE)
    else "no .log survived -- tinytex.clean deletes it unless capture was arranged"
  }), "latex log"))
  ## LAST, deliberately: it loads packages and a broken install can take R down. Everything above
  ## has already been assembled by the time this runs.
  out <- c(out, sec("DEPENDENCY CHECKS", grab({
    d <- actaDependencyChecks(version_dir)
    vapply(d, function(r) sprintf("[%s] %s -- %s", r$status, r$label,
                                  if (is.null(r$detail) || is.na(r$detail)) "" else r$detail),
           character(1))
  }), "dependency checks"))
  out <- c(out, "== COLLECTOR MANIFEST ==", manifest, "",
           "======== END OF BUNDLE ========")
  path <- file.path(dirname(if (!is.na(run_log)) run_log else file.path(work_dir, "x")),
                    sprintf("acta_diagnostics_%s.txt", format(Sys.time(), "%y%m%d_%H%M%S")))
  ok <- tryCatch({ con <- file(path, open = "wt", encoding = "UTF-8")
                   writeLines(enc2utf8(actaDiagScrub(out)), con, useBytes = TRUE)
                   close(con); TRUE }, error = function(e) FALSE)
  list(ok = isTRUE(ok), path = if (isTRUE(ok)) path else NA_character_, lines = length(out))
}

actaRunLogAppend <- function(log_path, entry) {
  cols <- c("timestamp","user","host","action","outcome","elapsed_s","version_dir","script",
            "script_version","seed","layout","layout_md5","report","plots","titer_edited",
            "outputs","detail")
  row <- as.list(entry)[cols]
  names(row) <- cols
  row <- lapply(row, function(v) if (is.null(v) || !length(v)) "" else
                  gsub("[\t\r\n]", " ", paste(as.character(v), collapse = "; ")))
  new <- !file.exists(log_path)
  if (new) dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
  con <- file(log_path, open = "at", encoding = "UTF-8")
  on.exit(close(con))
  if (new) writeLines(paste(cols, collapse = "\t"), con)
  writeLines(paste(unlist(row), collapse = "\t"), con)
  invisible(log_path)
}
actaRunLogEntry <- function(action, outcome, version_dir, res = NULL, layout = NA_character_,
                            report = NA, plots = NA, titer_edited = FALSE,
                            outputs = character(0), detail = "") {
  list(timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
       user = unname(Sys.info()[["user"]]), host = unname(Sys.info()[["nodename"]]),
       action = action, outcome = outcome,
       elapsed_s = if (!is.null(res)) round(res$elapsed_s, 1) else "",
       version_dir = version_dir,
       script = if (!is.null(res)) res$script else "",
       script_version = if (!is.null(res)) res$script_version else "",
       seed = if (!is.null(res)) res$seed else "",
       layout = if (!is.na(layout)) basename(layout) else "",
       layout_md5 = if (!is.na(layout) && file.exists(layout))
                      unname(tools::md5sum(layout)) else "",
       report = report, plots = plots, titer_edited = titer_edited,
       outputs = basename(outputs[!is.na(outputs)]),
       ## An explicit `detail` wins; otherwise take whichever error the result carries. A run logged
       ## as "analysis_failed" with an EMPTY detail column is a dead end for whoever reads the log
       ## later -- which is exactly what happened on 2026-08-19: the outcome was recorded correctly
       ## and the reason was nowhere, so the failure had to be reproduced from scratch to be seen.
       detail = if (nzchar(detail)) detail
                else if (!is.null(res) && !is.null(res$analysis_error) &&
                         !is.na(res$analysis_error)) as.character(res$analysis_error)
                else if (!is.null(res) && !is.null(res$report_error) &&
                         !is.na(res$report_error)) as.character(res$report_error)
                else "")
}


## ======================= DEPENDENCY CHECKS =======================

## The local TeX Live release year, from `tlmgr --version` ("... version 2025"). NA when tlmgr cannot
## be run or the line is absent -- callers must tolerate that rather than assume a year.
.actaTexLiveYear <- function() {
  v <- suppressWarnings(tryCatch(system2("tlmgr", "--version", stdout = TRUE, stderr = TRUE),
                                 error = function(e) character(0)))
  hit <- grep("version[[:space:]]+[0-9]{4}", v, value = TRUE)
  if (!length(hit)) return(NA_integer_)
  suppressWarnings(as.integer(sub(".*version[[:space:]]+([0-9]{4}).*", "\\1", hit[[1]])))
}

## TeX Live keeps every past release frozen under historic/, so a distribution a year behind can
## still install ITS OWN packages indefinitely. The tlnet-final URL for a given year.
.actaTexHistoricRepo <- function(year = .actaTexLiveYear()) {
  if (is.na(year)) year <- "<year>"
  sprintf("https://ftp.tu-chemnitz.de/pub/tug/historic/systems/texlive/%s/tlnet-final", year)
}

## Put the standard system directories back on PATH when they are genuinely missing.
##
## WHY THIS EXISTS, and why REPORTING was not enough. actaTlmgrStatus() below already diagnoses this
## precisely -- it names the typo, prints the PATH, and gives the one-liner. A colleague hit it on
## 2026-08-20, was given that message, and hit the SAME thing again on 2026-08-21: their PATH read
##   /Users/<user>/.local/bin:/usr/local/bin:/user/bin:/bin
## where `/user/bin` is a one-character typo for `/usr/bin`. So perl (at /usr/bin/perl) is invisible,
## tlmgr is a Perl script, and the PDF cannot be built.
##
## Setup.R repairs this -- but only inside its OWN process. The app and the pipeline run in
## separate R sessions, each inheriting the same broken PATH, and each only *reported* it. Diagnosing
## the same fault on every run and fixing it on none is the gap this closes.
##
## Deliberately conservative: it only PREPENDS directories that (a) are standard, (b) are absent from
## PATH, and (c) actually exist. It never removes or reorders anything, so it cannot shadow a
## deliberately-chosen toolchain. It returns what it added so callers can say so out loud -- silently
## working around a broken environment would hide a fault the user still needs to fix permanently.
actaRepairPath <- function(quiet = FALSE) {
  if (.Platform$OS.type == "windows") return(character(0))
  onPath  <- strsplit(Sys.getenv("PATH"), .Platform$path.sep, fixed = TRUE)[[1]]
  wanted  <- c("/usr/bin", "/bin", "/usr/sbin", "/sbin", "/usr/local/bin", "/opt/homebrew/bin")
  addBack <- setdiff(wanted, onPath)
  addBack <- addBack[dir.exists(addBack)]
  if (!length(addBack)) return(character(0))
  Sys.setenv(PATH = paste(paste(addBack, collapse = .Platform$path.sep), Sys.getenv("PATH"),
                          sep = .Platform$path.sep))
  if (!quiet)
    message("[ACTA] PATH was missing ", paste(addBack, collapse = ", "),
            " -- added for this session.\n",
            "       This is a machine-level fault, not an ACTA one: fix it permanently by adding\n",
            "       PATH=", paste(c(addBack, "${PATH}"), collapse = .Platform$path.sep),
            "\n       to ~/.Renviron (a PATH= line that REPLACES rather than prepends is the usual cause).")
  addBack
}
## The R packages are READ OUT OF THE SCRIPT, not restated here: the script already declares them
## in `listOfLibrary` and self-installs from CRAN then Bioconductor. Duplicating that list would
## let the two drift, and the failure mode is the app reporting all-clear for a package the run
## then cannot load. Add a package to the script and it appears in this check for free.
actaScriptPackages <- function(version_dir) {
  ## Reads the script AND the .Rmd, unioned. They carry DIFFERENT lists: the report additionally
  ## needs ragg (its chunks are set to dev = "ragg_png") and sessioninfo (the Session Information
  ## section). Parsing only the script meant the dependency panel could report "all 28 present" and
  ## be right about the analysis while the report was still two packages short -- a green panel in
  ## front of a report that cannot build is the failure mode this check exists to prevent.
  files <- c(list.files(version_dir, pattern = "^ACTA_Script.*\\.R$",  full.names = TRUE),
             list.files(version_dir, pattern = "^ACTA_Report.*\\.Rmd$", full.names = TRUE))
  files <- files[file.exists(files)]
  if (!length(files)) return(character(0))
  out <- unlist(lapply(files, function(p) {
    txt <- paste(readLines(p, warn = FALSE), collapse = "\n")
    m <- regmatches(txt, gregexpr("listOfLibrary\\s*<-\\s*c\\(([^)]*)\\)", txt))[[1]]
    if (!length(m)) return(character(0))
    unlist(lapply(m, function(one)
      trimws(gsub('["\']', "", strsplit(sub(".*c\\(", "", sub("\\)$", "", one)), ",")[[1]]))))
  }))
  unique(out[nzchar(out)])
}

## Non-R dependencies cannot be fixed by install.packages(), so each one that is missing carries the
## URL to fetch it. R itself is not checked -- you cannot reach this code without it.
##
## SEVERITY. A missing R package is fatal: the run loads it. XQuartz is fatal on macOS because
## flowMeans pulls in tcltk, which needs X11 -- so library(flowMeans) fails outright without it.
## LaTeX and pandoc are WARN, not fail: they are needed only for the PDF, and the report is a
## checkbox the operator can clear and still get the export the dashboard reads.
## LaTeX packages the report's own header-includes ask for, and whether this machine has them.
##
## Read OUT OF THE .Rmd rather than restated here, for the same reason the R package list is read out
## of the script: a hardcoded copy drifts, and the failure mode is a check that passes for a package
## the render then cannot find.
##
## This exists because of a real, repeated failure. A colleague's OQ_Test1 failed on 2026-08-19 with
##   ! LaTeX Error: File `placeins.sty' not found.  ! Emergency stop.
## after every NUMBER in the run was correct -- the analysis, the QC verdicts, the export, the plots,
## the dashboard. Their TeX install simply lacks a package the report needs, and nothing checked for
## it, so the only symptom was a failed PDF. There are ELEVEN such packages and a minimal TeX
## distribution ships few of them, so without this check the fix is discovered one render at a time.
## Every LaTeX package that REACHES the engine, not just the ones written in the .Rmd.
##
## Found 2026-08-20 while working out whether a colleague's PDF would render on 2_89: reading only the
## .Rmd's own \usepackage lines verified SIX packages when the generated .tex loads fourteen. kableExtra
## injects its own set at knit time -- multirow, wrapfig, pdflscape, tabu, threeparttable,
## threeparttablex, ulem, makecell and more -- and none of them appear in the .Rmd, so the check was
## blind to exactly the packages a minimal TeX is most likely to be missing.
##
## Ordering makes the blind spot worse, not better: header-includes are emitted BEFORE kableExtra's
## injections, so a missing package of OURS stops the render before LaTeX ever reaches theirs. Her
## 2_87 failure died on placeins and therefore told us nothing about the eight behind it.
##
## The list is read from kableExtra itself rather than restated, so it cannot drift when kableExtra
## changes. latex_pkg_list() is internal, hence the tryCatch and the fallback: if it ever disappears,
## the check must degrade to the known set rather than silently shrink back to six.
## The UNION across kableExtra versions, not a snapshot of one. 1.4.0 injects `tabu`; a newer release
## replaced that unmaintained package with `xltabular`, which is what a colleague's 2_89 run was
## missing. A fallback taken from one machine names the wrong one on the other, and this list only ever
## runs when latex_pkg_list() cannot be read -- exactly when there is no way to tell which variant is
## installed. Naming one package too many costs an unnecessary tlmgr_install; naming one too few costs
## a failed render and a round trip. Both are listed on purpose.
ACTA_KABLEEXTRA_TEX <- c("booktabs", "longtable", "array", "multirow", "wrapfig", "float", "colortbl",
                         "pdflscape", "tabu", "xltabular", "threeparttable", "threeparttablex",
                         "ulem", "inputenc", "makecell", "xcolor")

actaLatexPackages <- function(version_dir) {
  rmd <- list.files(version_dir, pattern = "^ACTA_Report.*[.]Rmd$", full.names = TRUE)
  if (length(rmd) != 1) return(character(0))
  txt <- tryCatch(readLines(rmd[[1]], warn = FALSE), error = function(e) character(0))
  m <- regmatches(txt, gregexpr("usepackage(\\[[^]]*\\])?\\{[A-Za-z0-9,]+\\}", txt))
  nm <- gsub(".*\\{|\\}", "", unlist(m))
  own <- unlist(strsplit(nm, ","))
  ## Only when the report actually uses kableExtra -- the function should describe THIS report, not
  ## assume every report will.
  inj <- character(0)
  if (any(grepl("kableExtra", txt, fixed = TRUE))) {
    inj <- tryCatch({
      l <- get("latex_pkg_list", asNamespace("kableExtra"))()
      out <- gsub(".*\\{|\\}", "", l)
      if (length(out)) out else ACTA_KABLEEXTRA_TEX
    }, error = function(e) ACTA_KABLEEXTRA_TEX)
  }
  sort(unique(c(own, inj)))
}

## Which of them kpsewhich cannot find. Empty when there is no kpsewhich, so a machine with no TeX at
## all reports "no LaTeX engine" once rather than eleven missing packages on top of it.
## ---------------------------------------------------------------------------------------------
## CAN tlmgr ACTUALLY RUN? -- because telling somebody to run a command that cannot start is worse
## than telling them nothing.
##
## From a colleague's machine, 2026-08-20. The check below correctly reported four missing LaTeX
## packages and offered `tinytex::tlmgr_install(...)`. That produced:
##
##     tlmgr install fancyhdr lastpage placeins titlesec
##     env: perl: No such file or directory
##
## tlmgr is a Perl script (`#!/usr/bin/env perl`), so it never started. The cause was one character:
## that PATH carried "/user/bin" where it meant "/usr/bin", so perl -- which was present, at
## /usr/bin/perl -- was invisible to R. Everything else in /usr/bin was invisible too.
##
## This is diagnosable, so it should be diagnosed. WINDOWS IS NOT EXPOSED to the perl half: TeX Live
## and TinyTeX ship their own Perl there and tlmgr.bat uses it; MiKTeX does not use tlmgr at all.
actaTlmgrStatus <- function(probe_pkg = NULL) {
  if (!nzchar(Sys.which("tlmgr")))
    return(list(ok = FALSE, why = "tlmgr is not on PATH",
                fix = paste("install a TeX distribution (or add its bin directory to PATH);",
                            "MiKTeX users install packages through MiKTeX instead")))
  ## Cheapest real test: run it. A Perl script whose interpreter is missing fails here.
  out <- suppressWarnings(tryCatch(system2("tlmgr", "--version", stdout = TRUE, stderr = TRUE),
                                   error = function(e) NA_character_))
  code <- attr(out, "status")
  if (!any(is.na(out)) && (is.null(code) || identical(as.integer(code), 0L))) {
    ## IT RUNS -- which is NOT the same as being able to install anything.
    ##
    ## Reported 2026-08-21. A colleague's tlmgr ran perfectly and refused every install:
    ##   tlmgr: Local TeX Live (2025) is older than remote repository (2026).
    ##   Cross release updates are only supported with update-tlmgr-latest(.sh/.exe) --update
    ## TeX Live repositories are YEAR-VERSIONED. Once CTAN rolls over to the next release, an
    ## installation from the previous year can no longer install from the current repository at all.
    ## Reporting "tlmgr ok" and then telling the user to run tinytex::tlmgr_install() sends them to a
    ## command that cannot succeed -- which is exactly what happened.
    ##
    ## `probe_pkg` makes this REAL rather than a guess: a --dry-run install downloads nothing and
    ## changes nothing, but does contact the repository, so it surfaces the refusal verbatim. Skipped
    ## when no name is supplied, because it costs a round-trip (~3 s) and only matters when a package
    ## is actually missing.
    if (!is.null(probe_pkg) && length(probe_pkg) == 1L && nzchar(probe_pkg)) {
      dry <- suppressWarnings(tryCatch(
        system2("tlmgr", c("install", "--dry-run", probe_pkg), stdout = TRUE, stderr = TRUE),
        error = function(e) character(0)))
      if (length(dry) && any(grepl("older than remote repository|[Cc]ross release", dry))) {
        yr <- .actaTexLiveYear()
        return(list(
          ok  = FALSE,
          why = sprintf(paste("this TeX Live (%s) is a RELEASE OLDER than the package repository,",
                              "which refuses cross-release installs -- so tlmgr runs but cannot",
                              "install anything"),
                        if (is.na(yr)) "unknown year" else yr),
          fix = sprintf(paste("upgrade the distribution -- tinytex::reinstall_tinytex() is one",
                              "command and keeps working next year. To stay on this release",
                              "instead, pin its frozen archive: tinytex::tlmgr_repo('%s')"),
                        .actaTexHistoricRepo(yr))))
      }
    }
    return(list(ok = TRUE, why = NA_character_, fix = NA_character_))
  }

  ## It is installed and it will not run. On unix that is almost always the interpreter.
  if (.Platform$OS.type != "windows" && !nzchar(Sys.which("perl"))) {
    sys <- c("/usr/bin/perl", "/bin/perl")
    hit <- sys[file.exists(sys)]
    if (length(hit))
      return(list(ok = FALSE,
                  why = sprintf(paste("tlmgr is a Perl script and perl is not on this session's",
                                      "PATH -- though it exists at %s. PATH is: %s"),
                                hit[[1]], Sys.getenv("PATH")),
                  fix = sprintf(paste("add its directory to PATH. For this session:",
                                      "Sys.setenv(PATH = paste('%s', Sys.getenv('PATH'),",
                                      "sep = .Platform$path.sep)) -- then fix it permanently in",
                                      "~/.Renviron or your shell profile, where a PATH= line that",
                                      "REPLACES rather than prepends is the usual cause"),
                                dirname(hit[[1]])))) 
    return(list(ok = FALSE, why = "tlmgr is a Perl script and no perl was found at all",
                fix = "install perl (e.g. `brew install perl`), or install a full TeX distribution"))
  }
  list(ok = FALSE, why = paste("tlmgr will not run:", paste(utils::head(out, 2), collapse = " ")),
       fix = "check the TeX installation, or install a full distribution")
}

actaLatexMissing <- function(pkgs) {
  if (!length(pkgs) || !nzchar(Sys.which("kpsewhich"))) return(character(0))
  ## Spelled out because the obvious one-liner is WRONG and silently so. kpsewhich returns
  ## character(0) for a package it cannot find; [1] on that is NA; `%||%` does not replace NA (only
  ## NULL); and nzchar(NA) is TRUE. So the compact version reported every MISSING package as present
  ## -- a check that always passes, which is worse than no check. Caught by asking it about a package
  ## name that cannot exist.
  found <- function(k) {
    out <- suppressWarnings(system2("kpsewhich", shQuote(paste0(k, ".sty")),
                                    stdout = TRUE, stderr = FALSE))
    length(out) >= 1L && !is.na(out[[1]]) && nzchar(out[[1]])
  }
  pkgs[!vapply(pkgs, found, logical(1))]
}

actaDependencyChecks <- function(version_dir, app_pkgs = c("shiny", "processx", "this.path",
                                                          "readxl", "openxlsx", "jsonlite",
                                                          "rmarkdown")) {
  recs <- list()
  add <- function(...) recs[[length(recs) + 1L]] <<- .appRec(...)

  ## --- R packages, rolled up: one row per source rather than ~35 rows of green ---------------
  scriptPkgs <- actaScriptPackages(version_dir)
  chkSet <- function(id, label, pkgs, hint) {
    pkgs <- pkgs[nzchar(pkgs)]
    if (!length(pkgs)) return(add(id, label, "skip", "could not read the package list", version_dir))
    miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
    if (!length(miss))
      add(id, label, "pass", sprintf("all %d present", length(pkgs)), version_dir)
    else
      add(id, label, "fail",
          sprintf("%d missing: %s  --  %s", length(miss), paste(miss, collapse = ", "), hint),
          version_dir)
  }
  chkSet("pkgs_script", "R packages the analysis needs", scriptPkgs,
         "the script self-installs from CRAN then Bioconductor on first run")
  chkSet("pkgs_app", "R packages the app needs", app_pkgs,
         'install.packages(c("shiny","processx","rmarkdown"))')

  ## --- XQuartz / X11: fatal on macOS, and not installable from R -----------------------------
  if (Sys.info()[["sysname"]] == "Darwin") {
    tcl <- isTRUE(requireNamespace("tcltk", quietly = TRUE)) &&
           !inherits(try(tcltk::tclVersion(), silent = TRUE), "try-error")
    if (tcl)
      add("xquartz", "XQuartz (X11, required by flowMeans)", "pass", NA_character_, NA_character_)
    else
      add("xquartz", "XQuartz (X11, required by flowMeans)", "fail",
          paste("not working -- flowMeans loads tcltk, which needs X11, so the run will fail.",
                "Install XQuartz and restart R: https://www.xquartz.org"), "https://www.xquartz.org")
  }

  ## --- LaTeX: only needed for the PDF, hence warn -------------------------------------------
  latex <- nzchar(Sys.which("xelatex")) ||
           (requireNamespace("tinytex", quietly = TRUE) && isTRUE(tinytex::is_tinytex()))
  if (latex)
    add("latex", "LaTeX (xelatex, for the PDF report)", "pass", NA_character_, NA_character_)
  else
    add("latex", "LaTeX (xelatex, for the PDF report)", "warn",
        paste("not found -- clear \u201cGenerate report\u201d and the export is still written.",
              "To enable it: tinytex::install_tinytex(), or MacTeX from https://www.tug.org/mactex/"),
        "https://www.tug.org/mactex/")

  ## --- LaTeX PACKAGES, not just the engine -------------------------------------------------
  ## Only when an engine exists: with no TeX at all the warning above is the whole story.
  if (latex) {
    texPkgs <- actaLatexPackages(version_dir)
    if (!length(texPkgs))
      add("latex_pkgs", "LaTeX packages the report needs", "skip",
          "could not read the .Rmd header to find out which", NA_character_)
    else if (!nzchar(Sys.which("kpsewhich")))
      add("latex_pkgs", "LaTeX packages the report needs", "skip",
          "kpsewhich not on PATH, so they cannot be checked from here", NA_character_)
    else {
      miss <- actaLatexMissing(texPkgs)
      if (!length(miss))
        add("latex_pkgs", sprintf("LaTeX packages the report needs (%d)", length(texPkgs)),
            "pass", NA_character_, NA_character_)
      else {
        ## The remedy depends on whether tlmgr can run at all -- see actaTlmgrStatus().
        ## Pass a missing package so actaTlmgrStatus() can tell "tlmgr runs" from "tlmgr runs
        ## and will still refuse to install this" -- the release-skew case.
        tl <- actaTlmgrStatus(probe_pkg = miss[[1]])
        add("latex_pkgs", sprintf("LaTeX packages the report needs (%d)", length(texPkgs)), "fail",
            if (isTRUE(tl$ok))
              sprintf(paste("missing: %s. The analysis and export will still be written; only the",
                            "PDF fails. Install with:  tinytex::tlmgr_install(c(%s))"),
                      paste(miss, collapse = ", "),
                      paste(sprintf('"%s"', miss), collapse = ", "))
            else
              sprintf(paste("missing: %s. The analysis and export will still be written; only the",
                            "PDF fails. tlmgr CANNOT INSTALL THEM on this machine: %s. Fix: %s"),
                      paste(miss, collapse = ", "), tl$why, tl$fix),
            NA_character_)
      }
    }
  }

  ## --- pandoc: rmarkdown drives it; often absent in a bare R install ------------------------
  pandoc <- nzchar(Sys.which("pandoc")) ||
            (requireNamespace("rmarkdown", quietly = TRUE) && isTRUE(rmarkdown::pandoc_available()))
  if (pandoc)
    add("pandoc", "Pandoc (for rendering the report)", "pass", NA_character_, NA_character_)
  else
    add("pandoc", "Pandoc (for rendering the report)", "warn",
        paste("not found -- needed only for the PDF report.",
              "Install from https://pandoc.org/installing.html"), "https://pandoc.org/installing.html")
  recs
}


## ======================= OQ (OPERATIONAL QUALIFICATION) TESTS =======================
## An OQ folder is a self-contained, sanitised copy of a real run: its own layout workbook, FCS
## tree, script, functions, report and dashboard generator. Running it proves a fresh install on a
## different machine produces the RIGHT NUMBERS, not merely that files appeared -- which is what
## makes it a regression test rather than a smoke test.
##
## Expectations live in the folder's own expected.json, so each OQ case carries its own baseline and
## a new case needs no code change here.
##
## Every artefact is collected into <OQ folder>/Outputs/ so a passing run can be deleted in one go.
## That placement is also what makes the dashboard step work: Outputs/ becomes an immediate
## subfolder of the scan root, which is exactly the "one run per subfolder" shape the generator
## expects.
actaOQExpected <- function(oq_dir) {
  f <- file.path(oq_dir, "expected.json")
  if (!file.exists(f)) return(NULL)
  tryCatch(jsonlite::fromJSON(f, simplifyVector = TRUE), error = function(e) NULL)
}

actaOQRun <- function(oq_dir, quiet = TRUE, progress = function(...) invisible(), code_dir = NULL) {
  oq_dir <- normalizePath(oq_dir, mustWork = TRUE)
  ## Code comes from the working version by default, so the case holds only its inputs -- layout,
  ## FCS, expected.json -- and can never be running a stale copy of the pipeline.
  ##
  ## Found by walking UP until a folder contains an ACTA_Script*.R, rather than assuming the parent.
  ## Cases now live one level deeper (Diagnostics/OQ_TestN), and hardcoding the depth would break
  ## the moment that nesting changed again.
  if (is.null(code_dir)) {
    d <- dirname(oq_dir); code_dir <- NA_character_
    for (i in 1:4) {
      if (length(list.files(d, pattern = "^ACTA_Script.*[.]R$"))) { code_dir <- d; break }
      nd <- dirname(d); if (identical(nd, d)) break; d <- nd
    }
    if (is.na(code_dir))
      stop(sprintf("actaOQRun: no folder above '%s' contains an ACTA_Script*.R.", oq_dir), call. = FALSE)
  }
  code_dir <- normalizePath(code_dir, mustWork = TRUE)
  exp    <- actaOQExpected(oq_dir)
  outDir <- file.path(oq_dir, "Outputs")
  ## Start clean: a stale artefact from a previous run must not be able to satisfy a check.
  unlink(outDir, recursive = TRUE); dir.create(outDir, recursive = TRUE, showWarnings = FALSE)
  recs <- list(); t0 <- proc.time()[["elapsed"]]
  add <- function(id, label, status, detail = NA_character_)
    recs[[length(recs) + 1L]] <<- .appRec(id, label, status, detail, oq_dir)
  ## Compare-and-record. A missing expectation is a SKIP, never a silent pass.
  want <- function(id, label, got, key) {
    if (is.null(exp) || is.null(exp[[key]]))
      return(add(id, label, "skip", sprintf("no `%s` in expected.json; observed %s", key, format(got))))
    ok <- isTRUE(all.equal(got, exp[[key]]))
    add(id, label, if (ok) "pass" else "fail",
        sprintf("expected %s, got %s", format(exp[[key]]), format(got)))
  }

  ## Clear stale artefacts from the case ROOT before running. The collection step below MOVES this
  ## run's own output into Outputs/, so the runner never leaves anything behind -- but a plain ACTA
  ## run against the case folder (the app pointed at it, or a direct run_acta call while debugging)
  ## writes its export to the root and does NOT collect. That leftover then survives, and because
  ## export discovery reads a run's root PLUS Outputs/, the case DOUBLE-COUNTS itself in the pooled
  ## dashboard: OQ_Test1 contributed 2 rows for its 1 run and the pooled total read 5 instead of 4.
  ## Regenerable by definition -- this is a diagnostic case whose whole output is rebuilt each run.
  stale <- c(list.files(oq_dir, pattern = "TitrationExport.*[.]xlsx$", full.names = TRUE),
             list.files(oq_dir, pattern = "^ACTA_Report.*[.]pdf$", full.names = TRUE))
  if (length(stale)) {
    progress(sprintf("clearing %d stale artefact(s) from the case root ...", length(stale)))
    unlink(stale)
  }
  if (dir.exists(file.path(oq_dir, "Plots"))) unlink(file.path(oq_dir, "Plots"), recursive = TRUE)

  progress("running the analysis and report ...")
  res <- try(run_acta(oq_dir, report = TRUE, plots = TRUE, quiet = quiet, code_dir = code_dir),
             silent = TRUE)
  if (inherits(res, "try-error")) {
    add("run", "Analysis completed", "fail", conditionMessage(attr(res, "condition")))
    return(actaOQFinish(oq_dir, outDir, recs, proc.time()[["elapsed"]] - t0, exp))
  }
  add("run", "Analysis completed", "pass", sprintf("%.0f s", res$elapsed_s))
  ## A failed render must report the ACTUAL cause. rmarkdown says "error in running command" for ANY
  ## failed external command -- missing LaTeX, missing pandoc, or a LaTeX compile error such as an
  ## absent .sty -- so that message alone is not evidence for any one of them. Reading it as "no
  ## LaTeX" sent one investigation at the wrong cause on a machine whose dependency panel was green.
  ## Evidence is gathered in order and only what is OBSERVED is claimed:
  ##   1. no engine at all -> say so, with the two-step install
  ##   2. no pandoc        -> say so
  ##   3. a LaTeX .log     -> quote its first real error line ("! ..."), which names the true fault
  ## If none applies, the raw error stands rather than being dressed up with a guess.
  rptDetail <- if (isTRUE(res$report_ok)) NA_character_ else {
    extra <- character(0)
    if (!isTRUE(actaPdfEngine()$ok))
      extra <- c(extra, paste0('no LaTeX engine found. install.packages("tinytex") THEN ',
                               "tinytex::install_tinytex()"))
    if (!(nzchar(Sys.which("pandoc")) ||
          (requireNamespace("rmarkdown", quietly = TRUE) && isTRUE(rmarkdown::pandoc_available()))))
      extra <- c(extra, "pandoc not found -- rmarkdown needs it to render at all")
    ## Evidence from two places: the render's CAPTURED console output, and the .log kept on disk by
    ## clean = FALSE. The console is the reliable one -- the previous version read only the .log,
    ## which render() had already deleted, so this whole branch silently produced nothing and the
    ## log kept showing the bare "error in running command" it was written to explain.
    ## Only as a FALLBACK now: run_acta() already folds the LaTeX "! ..." lines into report_error
    ## while the evidence is fresh (and moves the .log here from the code folder if that is where it
    ## landed). Repeating the scan unconditionally printed the same three lines twice in one detail
    ## cell, so it runs only when the message does not already carry them.
    if (!grepl("LaTeX said:", res$report_error %||% "", fixed = TRUE)) {
      ln <- unlist(lapply(list.files(oq_dir, pattern = "^ACTA_Report.*[.]log$", full.names = TRUE),
                          function(f) tryCatch(readLines(f, warn = FALSE),
                                               error = function(e) character(0))))
      bang <- grep("^!|^\\s*Error|LaTeX Error|not found|Fatal", ln, value = TRUE)
      bang <- unique(trimws(bang)); bang <- bang[nzchar(bang)]
      if (length(bang)) extra <- c(extra, paste("render said:", paste(utils::head(bang, 3), collapse = " | ")))
    }
    if (length(extra)) paste0(res$report_error, " -- ", paste(extra, collapse = "; "))
    else res$report_error
  }
  add("report", "Report rendered", if (isTRUE(res$report_ok)) "pass" else "fail", rptDetail)

  want("stats_rows", "Per-well statistics: row count", if (is.null(res$stats)) 0L else nrow(res$stats), "stats_rows")
  want("stats_cols", "Per-well statistics: column count", if (is.null(res$stats)) 0L else ncol(res$stats), "stats_cols")
  want("n_groups",   "Antibody groups analysed", length(res$antibody), "n_groups")
  if (!is.null(exp) && !is.null(exp$group))
    add("group", "Group name", if (identical(as.character(res$antibody), as.character(exp$group))) "pass" else "fail",
        ## Both sides collapsed: `group` may be an ARRAY for a multi-group case, and an
        ## uncollapsed sprintf() would silently emit one detail string per group.
        sprintf("expected %s, got %s", paste(exp$group, collapse = ", "),
                paste(res$antibody, collapse = ", ")))

  ## Events actually READ per file, against expected.json's events_per_well. That field existed as
  ## documentation only and was never asserted -- so the one thing a downsampling feature has to
  ## prove (that it read n events and not the whole file) had nothing checking it. `actaEventsRead`
  ## is accumulated by the script across antibody groups, one entry per FCS file.
  ##
  ## Deliberately compared to what was READ rather than to $TOT: flowCore's which.lines does not
  ## rewrite $TOT, so $TOT still reports what the instrument acquired and would pass even if
  ## downsampling silently did nothing. A file holding fewer events than n is read whole, so the
  ## rule is "every file is at n, except those that never had that many".
  if (!is.null(exp) && !is.null(exp$events_per_well)) {
    ev  <- if (!is.null(res$env) && exists("actaEventsRead", envir = res$env, inherits = FALSE))
             get("actaEventsRead", envir = res$env) else integer(0)
    wantN <- as.integer(exp$events_per_well)
    ds  <- if (!is.null(res$env) && exists("actaDownsampleTo", envir = res$env, inherits = FALSE))
             get("actaDownsampleTo", envir = res$env) else NA_integer_
    tot <- if (!is.null(res$env) && exists("actaEventsAcquired", envir = res$env, inherits = FALSE))
             get("actaEventsAcquired", envir = res$env) else integer(0)
    ## A file that never held wantN events is expected to come back short, not to fail. OQ_Test1
    ## declares 60000 and contains a 970-event well, so a strict equality would fail on real data.
    tgt <- if (length(tot) && all(names(ev) %in% names(tot)))
             pmin(wantN, tot[names(ev)]) else rep(wantN, length(ev))
    off <- names(ev)[ev != tgt]
    nShort <- sum(tgt < wantN)
    add("events_per_well",
        sprintf("Events read per file%s", if (!is.na(ds)) sprintf(" (Downsample_to = %s)",
                                                                 format(ds, big.mark = ",")) else ""),
        if (!length(ev)) "fail" else if (!length(off)) "pass" else "fail",
        if (!length(ev)) "the run recorded no per-file event counts"
        else if (!length(off)) sprintf("all %d file(s) at %s events%s%s", length(ev),
                                       format(wantN, big.mark = ","),
                                       if (nShort) sprintf(" (%d acquired fewer and were read whole)", nShort) else "",
                                       if (!is.na(ds) && ds == wantN) " -- downsampling applied" else "")
        else sprintf("expected %s in every file; %d differ: %s", format(wantN, big.mark = ","),
                     length(off), paste(sprintf("%s=%d", off, ev[off]), collapse = ", ")))
  }

  ## MiFlowCyt metadata: assembled, structurally complete, and HONEST about what it is. Asserted on
  ## the OBJECT rather than on rendered prose, so the check survives any reformatting of the report.
  ## The completeness flag is the one that matters most -- it is what stops a consumer presenting this
  ## as a compliant MiFlowCyt submission -- so a change there must fail the OQ, not pass quietly.
  m <- res$miflowcyt
  if (is.null(m)) {
    add("miflowcyt", "MiFlowCyt metadata assembled", "fail", "run_acta() returned no $miflowcyt")
  } else {
    gaps <- character(0)
    for (s in c("standard", "experiment", "specimen", "instrument", "reagents", "analysis"))
      if (is.null(m[[s]])) gaps <- c(gaps, sprintf("section '%s' missing", s))
    if (!identical(m$standard$completeness, "structured-not-complete"))
      gaps <- c(gaps, sprintf("completeness is '%s', expected 'structured-not-complete'",
                              m$standard$completeness %||% "NULL"))
    if (!nrow(m$analysis$data_files %||% data.frame()))
      gaps <- c(gaps, "no data files listed")
    if (is.null(m$analysis$gating$statistics) || !nrow(m$analysis$gating$statistics))
      gaps <- c(gaps, "no gating statistics")
    if (is.null(m$analysis$downsample$text)) gaps <- c(gaps, "no downsample record")
    add("miflowcyt", "MiFlowCyt metadata assembled", if (length(gaps)) "fail" else "pass",
        if (length(gaps)) paste(gaps, collapse = "; ")
        else sprintf("6 sections, %d data file(s), %d gating statistic row(s), %s",
                     nrow(m$analysis$data_files), nrow(m$analysis$gating$statistics),
                     m$analysis$downsample$text))
  }

  ## QC: every check present AND at its baseline verdict, in EVERY group.
  ## This used to read res$qcList[[1]] only. On a single-group case that is the whole run, but
  ## OQ_Test2 has three groups -- a verdict moving in CD5 or CD8 would have gone unreported while
  ## the log still said "pass". Every group is asserted now.
  qcAll <- if (length(res$qcList)) res$qcList else list()
  gnames <- names(qcAll); if (!length(gnames)) gnames <- character(0)
  idsOf  <- function(q) vapply(q, function(c) c$id, "")
  verdOf <- function(q) setNames(vapply(q, function(c) c$verdict, ""), idsOf(q))

  if (!is.null(exp) && !is.null(exp$qc_checks)) {
    gaps <- unlist(lapply(gnames, function(g) {
      m <- setdiff(exp$qc_checks, idsOf(qcAll[[g]]))
      if (length(m)) sprintf("%s: %s", g, paste(m, collapse = ", ")) else NULL
    }))
    add("qc_present", "All expected QC checks ran",
        if (!length(gaps)) "pass" else "fail",
        if (length(gaps)) paste(gaps, collapse = "; ")
        else sprintf("%d checks in each of %d group(s)", length(exp$qc_checks), length(gnames)))
  }
  if (!is.null(exp) && isTRUE(exp$qc_all_pass)) {
    bad <- unlist(lapply(gnames, function(g) {
      v <- verdOf(qcAll[[g]]); f <- names(v)[v != "PASS"]
      if (length(f)) sprintf("%s: %s", g, paste(sprintf("%s=%s", f, v[f]), collapse = " ")) else NULL
    }))
    add("qc_pass", "Every QC check passed", if (!length(bad)) "pass" else "fail",
        if (length(bad)) paste(bad, collapse = "; ")
        else sprintf("all checks PASS in %d group(s)", length(gnames)))
  } else if (!is.null(exp) && !is.null(exp$qc_verdicts)) {
    ## Assert the exact verdicts, not merely that they all pass. A case whose data legitimately
    ## fails a check is still a regression case: what matters is that no verdict MOVES. Asserting
    ## "all pass" would have forced either a fake baseline or a bigger, slower data set.
    ##
    ## `qc_verdicts` takes two shapes. A FLAT map (check -> verdict) is the common case and must
    ## hold in every group. A NESTED map (group -> check -> verdict) is for a case whose groups
    ## legitimately disagree; groups absent from it are not asserted.
    ev <- exp$qc_verdicts
    nested <- length(ev) && all(vapply(ev, function(x) length(x) > 1 || !is.null(names(x)), logical(1)))
    perGroup <- if (nested) ev else setNames(rep(list(ev), length(gnames)), gnames)
    diffs <- unlist(lapply(intersect(gnames, names(perGroup)), function(g) {
      want_v <- unlist(perGroup[[g]]); got_v <- verdOf(qcAll[[g]])
      d <- names(want_v)[!vapply(names(want_v), function(k)
             identical(unname(got_v[k]), unname(want_v[[k]])), logical(1))]
      if (length(d)) sprintf("%s/%s: expected %s, got %s", g, d, want_v[d], got_v[d]) else NULL
    }))
    nAsserted <- sum(vapply(intersect(gnames, names(perGroup)),
                            function(g) length(unlist(perGroup[[g]])), integer(1)))
    add("qc_verdicts", "QC verdicts match the baseline",
        if (!length(diffs)) "pass" else "fail",
        ## Deliberately NOT the raw verdict vector. Printing "costain_lowest=FAIL" beside a passing
        ## assertion reads as a diagnostic failure when it is the recorded baseline for this data
        ## set. The per-check baseline lives in expected.json, which is the right place for it.
        if (!length(diffs)) sprintf("%d of %d verdicts match the baseline in expected.json across %d group(s)",
                                   nAsserted, nAsserted, length(gnames))
        else paste(diffs, collapse = "; "))
  }

  ## ---- collect the artefacts into Outputs/ -------------------------------------------------
  ## outDir is created at the top of this function, but RE-CREATED here, and mv() falls back to
  ## copy+delete when a rename fails. Both because of a real 2026-08-27 failure: `Outputs/` was
  ## created at run start, written into, and had VANISHED by the time collection ran -- a pending
  ## OneDrive deletion of the previous run's folder was applied minutes later, on top of the new one.
  ## file.rename() then failed silently for every artefact, so the report, the export and 18 plots
  ## stayed in the case root while five checks reported "no titration export to look in". The run had
  ## succeeded; only the collection lost it. A synced folder can delete a directory out from under a
  ## running process, and the old code read that as a pipeline failure.
  progress("collecting outputs ...")
  dir.create(outDir, recursive = TRUE, showWarnings = FALSE)
  moved <- character(0); lost <- character(0)
  mv <- function(from, to) {
    if (!file.exists(from)) return(invisible(FALSE))
    dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
    ok <- suppressWarnings(file.rename(from, to))
    if (!ok) {                      # cross-device, or the destination went away mid-run
      ok <- suppressWarnings(file.copy(from, to, overwrite = TRUE))
      if (ok) suppressWarnings(unlink(from))
    }
    if (ok) moved <<- c(moved, to) else lost <<- c(lost, basename(from))
    invisible(ok)
  }
  pdfs <- list.files(oq_dir, pattern = "^ACTA_Report.*[.]pdf$", full.names = TRUE)
  for (f in pdfs) mv(f, file.path(outDir, basename(f)))
  ## One export as of 2_89: the per-population table is the Pop_stats SHEET of the titration export,
  ## so there is no longer a second StatsExport workbook to collect. The pattern stays broad enough to
  ## sweep one left behind by an older version rather than leaving it in the case folder, which is what
  ## it was widened for -- the row-count assertion below globs TitrationExport specifically, so a
  ## stray file cannot be measured in its place.
  exps <- list.files(oq_dir, pattern = "(Titration|Stats)Export.*[.]xlsx$", full.names = TRUE)
  for (f in exps) mv(f, file.path(outDir, basename(f)))
  pl <- file.path(oq_dir, "Plots")
  nPlots <- 0L
  if (dir.exists(pl)) {
    nPlots <- length(list.files(pl, pattern = "[.]png$"))
    dest <- file.path(outDir, "Plots")
    if (!suppressWarnings(file.rename(pl, dest))) {
      ## "Directory not empty", or the destination vanished. Copy the PNGs across individually and
      ## drop the source only if every one arrived.
      dir.create(dest, recursive = TRUE, showWarnings = FALSE)
      pngs <- list.files(pl, pattern = "[.]png$", full.names = TRUE)
      okAll <- all(vapply(pngs, function(f)
                     isTRUE(suppressWarnings(file.copy(f, file.path(dest, basename(f)),
                                                       overwrite = TRUE))), logical(1)))
      if (okAll) unlink(pl, recursive = TRUE) else lost <- c(lost, "Plots/")
    }
  }
  want("n_plots", "Plot PNGs written", nPlots, "n_plots")
  ## Say so when collection lost something, instead of letting it surface further down as
  ## "no titration export to look in" -- which names the symptom and hides the cause.
  if (length(lost))
    add("collect", "Artefacts collected into Outputs/", "fail",
        sprintf("could not move into %s: %s", basename(outDir), paste(lost, collapse = ", ")))

  ## The MiFlowCyt appendix sheet, when the case asks for one. Both OQ cases set
  ## Info!miflowcyt_export TRUE, so this is the check that the flag is actually WIRED -- it sat unread
  ## in all four instructions files until 2026-08-19. Asserted on the sheet's own structure rather than
  ## on a row count, so rewording a value cannot fail it but losing a whole MiFlowCyt section will.
  .expForMfc <- list.files(outDir, pattern = "TitrationExport.*[.]xlsx$", full.names = TRUE)
  .wantMfc <- tryCatch({
    ii <- suppressMessages(readxl::read_excel(file.path(oq_dir, basename(
            list.files(oq_dir, pattern = "Instructions.*[.]xlsx$")[1])), sheet = "Info",
            col_types = "text", .name_repair = "minimal"))
    bare <- trimws(sub("[[:space:]]*\\(.*$", "", names(ii)))
    j <- which(bare == "miflowcyt_export")
    length(j) > 0 && toupper(trimws(as.character(ii[[j[1]]][1]))) %in% c("TRUE", "T", "YES", "1")
  }, error = function(e) FALSE)
  if (isTRUE(.wantMfc)) {
    sh <- if (length(.expForMfc)) tryCatch(readxl::excel_sheets(.expForMfc[[1]]),
                                           error = function(e) character(0)) else character(0)
    secs <- if ("MiFlowCyt_metadata" %in% sh)
              tryCatch({
                d <- suppressMessages(readxl::read_excel(.expForMfc[[1]], sheet = "MiFlowCyt_metadata",
                                                         col_types = "text"))
                intersect(c("1", "2", "3", "4"), trimws(as.character(d$Item)))
              }, error = function(e) character(0)) else character(0)
    add("miflowcyt_sheet", "MiFlowCyt_metadata sheet in the titration export",
        if (length(secs) == 4L) "pass" else "fail",
        if (!length(sh)) "no titration export to look in"
        else if (!("MiFlowCyt_metadata" %in% sh))
          sprintf("Info!miflowcyt_export is TRUE but the export has no MiFlowCyt_metadata sheet (%s)",
                  paste(sh, collapse = ", "))
        else sprintf("MiFlowCyt 1.0 sections present: %s", paste(secs, collapse = ", ")))
  }

  ## The two statistics sheets. Structural, not numeric: they exist, they carry rows, and SI_stats
  ## has one row per WELL while Pop_stats has at least as many (per well AND population). Asserting
  ## the shape rather than a count means adding a column cannot fail it, but losing a sheet -- which
  ## is what a botched consolidation looks like -- will.
  .expSheets <- list.files(outDir, pattern = "TitrationExport.*[.]xlsx$", full.names = TRUE)
  if (length(.expSheets)) {
    sh <- tryCatch(readxl::excel_sheets(.expSheets[[1]]), error = function(e) character(0))
    nr <- function(s) tryCatch(nrow(suppressMessages(readxl::read_excel(.expSheets[[1]], sheet = s,
                                 col_types = "text", .name_repair = "minimal"))),
                               error = function(e) NA_integer_)
    haveBoth <- all(c("SI_stats", "Pop_stats") %in% sh)
    ## Pop_stats exists ONLY if some gating_template row is flagged stats_export -- otherwise the
    ## script omits the sheet by design and says so. Demanding it unconditionally made the check a
    ## forcing function: OQ_Test3's flags were set TRUE to satisfy it rather than because the case
    ## needed per-population statistics, and a legitimate all-FALSE template would fail here.
    ## So: absent AND nothing asked for it -> skip; absent AND something asked for it -> fail.
    .gtWants <- tryCatch({
      .wb <- actaFindInstructions(oq_dir, full.names = TRUE)
      .g  <- suppressMessages(readxl::read_excel(.wb[[1]], sheet = "gating_template",
                                                 col_types = "text", .name_repair = "minimal"))
      .g  <- as.data.frame(.g)
      ## actaTemplateFlag(), NOT `.g$stats_export`: these columns are matched CASE-INSENSITIVELY
      ## everywhere else because the headers were lower-cased after shipping, and the SHIPPED
      ## TEMPLATE still writes `Stats_Export`. A literal lookup finds nothing, reports "nothing is
      ## flagged" and SKIPS a check that should have run -- silently, which is the failure mode the
      ## script's own comment at the gatingFlags block warns about. The helper also accepts
      ## T / YES / 1, which a `== "TRUE"` test does not.
      any(.actaTemplateLive(.g$alias) & actaTemplateFlag(.g, "stats_export", default = FALSE))
    }, error = function(e) NA)
    if (!("Pop_stats" %in% sh) && isFALSE(.gtWants)) {
      add("stats_sheets", "SI_stats and Pop_stats sheets in the titration export", "skip",
          "no gating_template row is flagged stats_export, so Pop_stats is omitted by design")
    } else
    add("stats_sheets", "SI_stats and Pop_stats sheets in the titration export",
        if (haveBoth && isTRUE(nr("SI_stats") > 0) && isTRUE(nr("Pop_stats") >= nr("SI_stats")))
          "pass" else "fail",
        if (!haveBoth) sprintf("missing from the export: %s (sheets: %s)",
                               paste(setdiff(c("SI_stats", "Pop_stats"), sh), collapse = ", "),
                               paste(sh, collapse = ", "))
        else sprintf("SI_stats %s well(s), Pop_stats %s row(s)", nr("SI_stats"), nr("Pop_stats")))
  }

  ## COMBINATORIAL ONLY, and opt-in via `shared_upstream` in expected.json so the single-marker
  ## cases neither gain a check nor lose one. This is THE property that makes co-titration sound:
  ## when one Dirname carries several titrations, each is loaded and gated in its own GatingSet, so
  ## the upstream populations they share -- Cells, Single_cells, Live -- are computed more than once
  ## from the same events. They must come out IDENTICAL. They only do because set.seed(ACTA_SEED)
  ## is re-set per loop iteration; drop that and flowClust/flowMeans would wander, the two series
  ## would sit on different parents, and their Stain Indices would not be comparable -- which is
  ## the whole claim a combinatorial titration makes.
  ##
  ## Read from the export's Pop_stats sheet rather than recomputed here, so it checks what SHIPPED.
  if (isTRUE(exp$shared_upstream)) {
    .ps <- list.files(outDir, pattern = "TitrationExport.*[.]xlsx$", full.names = TRUE)
    ps <- if (length(.ps))
            tryCatch(suppressMessages(readxl::read_excel(.ps[[1]], sheet = "Pop_stats",
                                                         .name_repair = "minimal")),
                     error = function(e) NULL) else NULL
    if (is.null(ps) || !nrow(ps)) {
      add("shared_upstream", "Co-titrated markers share identical upstream populations", "fail",
          "no Pop_stats sheet to read")
    } else {
      ps <- as.data.frame(ps)
      ## Keyed on PLATE POSITION, not on `name`: the sample name is generated PER TITRATION and
      ## embeds the target -- EXP..._E2_tgtCCR7_Costain_0_OpOP.fcs vs ..._tgtCD14_... -- so the two
      ## series never share one. Keying on it made this check compare nothing and pass with zero
      ## comparisons, which is why the zero guard below exists.
      need <- c("Dirname", "Alias", "Population", "PlateID", "Row", "Column", "count")
      if (!all(need %in% names(ps))) {
        add("shared_upstream", "Co-titrated markers share identical upstream populations", "skip",
            sprintf("Pop_stats lacks %s", paste(setdiff(need, names(ps)), collapse = ", ")))
      } else {
        multi <- names(which(tapply(ps$Alias, ps$Dirname, function(a) length(unique(a))) > 1L))
        if (!length(multi)) {
          add("shared_upstream", "Co-titrated markers share identical upstream populations", "skip",
              "no Dirname carries more than one titration in this case")
        } else {
          bad <- character(0); nCmp <- 0L
          for (dn in multi) {
            d <- ps[ps$Dirname == dn, , drop = FALSE]
            d$.well <- paste(d$PlateID, d$Row, d$Column, sep = "/")
            for (pop in unique(d$Population)) for (w in unique(d$.well)) {
              v <- d$count[d$Population == pop & d$.well == w]
              if (length(v) < 2L) next
              nCmp <- nCmp + 1L
              if (length(unique(as.character(v))) > 1L)
                bad <- c(bad, sprintf("%s / %s / well %s: %s", dn, pop, w,
                                      paste(unique(as.character(v)), collapse = " vs ")))
            }
          }
          ## ZERO COMPARISONS IS A FAILURE, not a pass. A Dirname reached this point precisely
          ## because it carries more than one titration, so there MUST be shared wells to compare;
          ## finding none means the join key is wrong, and the first version of this check keyed on
          ## `name` and reported "0 comparisons identical" as a PASS. A check that cannot fail is
          ## indistinguishable from one that never looks.
          add("shared_upstream", "Co-titrated markers share identical upstream populations",
              if (length(bad) || nCmp == 0L) "fail" else "pass",
              if (nCmp == 0L)
                sprintf(paste("no shared population/well pairs found in %s, yet it carries %d",
                              "titrations -- the join key is wrong"),
                        paste(multi, collapse = ", "),
                        length(unique(ps$Alias[ps$Dirname %in% multi])))
              else if (length(bad)) paste(utils::head(bad, 4), collapse = " | ")
              else sprintf("%d shared population/well comparison(s) identical across %s",
                           nCmp, paste(multi, collapse = ", ")))
        }
      }
    }
  }

  ## The SAME marker titrated twice -- once through the single-marker path (its own Dirname), once
  ## through the combinatorial path (an Alias inside a shared Dirname) -- must AGREE. Not be
  ## identical: OQ_Test3's two CCR7 series are independent replicates of the same wells, with their
  ## own event sample, their own control and a per-well stain-efficiency factor, because a fixture
  ## whose two panels match to the last digit reads as an artefact and it is also the dataset people
  ## try the tool with. So this asserts a BAND, in percentage points, and reports the observed worst
  ## case either way.
  ##
  ## What it still catches: a combinatorial path that systematically shifts a result. Replicate
  ## scatter is a percentage point or two; a broken shared-parent or a mis-keyed series is far more.
  ## Opt-in via `series_agreement`, so the other cases neither gain nor lose a check.
  if (!is.null(exp$series_agreement)) {
    sa   <- exp$series_agreement
    pr   <- as.character(unlist(sa$pair))
    ## as.numeric(NULL) is numeric(0), and `if (!is.finite(numeric(0)))` is an error, not FALSE --
    ## the length-ZERO end of the same trap that made `file.exists(file) && !overwrite` fail with
    ## "'length = 2' in coercion to logical(1)" when a run spanned two ELN IDs. Collapse to a scalar
    ## before any if().
    tolp <- suppressWarnings(as.numeric(sa$max_percent_diff_pp))
    tolp <- if (length(tolp) == 1L) tolp else NA_real_
    .si  <- list.files(outDir, pattern = "TitrationExport.*[.]xlsx$", full.names = TRUE)
    sit  <- if (length(.si))
              tryCatch(as.data.frame(suppressMessages(
                         readxl::read_excel(.si[[1]], sheet = "SI_stats", .name_repair = "minimal"))),
                       error = function(e) NULL) else NULL
    if (is.null(sit) || !all(c("Titration", "StainQty", "percent") %in% names(sit)) || length(pr) != 2L) {
      add("series_agreement", "Same marker both paths: replicate agreement", "fail",
          if (is.null(sit)) "no SI_stats sheet to read"
          else if (length(pr) != 2L) "series_agreement$pair must name exactly two titrations"
          else "SI_stats lacks Titration / StainQty / percent")
    } else {
      a <- sit[sit$Titration == pr[1], , drop = FALSE]
      b <- sit[sit$Titration == pr[2], , drop = FALSE]
      a <- a[order(a$StainQty), ]; b <- b[order(b$StainQty), ]
      if (!nrow(a) || !nrow(b) || nrow(a) != nrow(b)) {
        add("series_agreement", "Same marker both paths: replicate agreement", "fail",
            sprintf("%s has %d well(s), %s has %d", pr[1], nrow(a), pr[2], nrow(b)))
      } else {
        dpp   <- abs(100 * suppressWarnings(as.numeric(a$percent)) -
                     100 * suppressWarnings(as.numeric(b$percent)))
        worst <- max(dpp, na.rm = TRUE); meanD <- mean(dpp, na.rm = TRUE)
        ## No tolerance given: report what was measured rather than inventing a verdict -- the same
        ## contract every numeric check here uses when expected.json is silent.
        if (!is.finite(tolp)) {
          add("series_agreement", "Same marker both paths: replicate agreement", "skip",
              sprintf("no max_percent_diff_pp in expected.json; observed worst %.2f pp, mean %.2f pp over %d well(s)",
                      worst, meanD, nrow(a)))
        } else {
          ## Identical is a FAILURE too. Two independent replicates cannot agree exactly, so a zero
          ## difference means they are the same events -- which is what this case looked like before
          ## 2026-08-27 and what made it read as a fixture rather than an experiment.
          add("series_agreement", "Same marker both paths: replicate agreement",
              if (worst <= tolp && worst > 0) "pass" else "fail",
              if (worst == 0)
                "the two series are IDENTICAL -- they are sharing events, which is not a replicate"
              else sprintf("worst %.2f pp, mean %.2f pp over %d well(s); band %.2f pp",
                           worst, meanD, nrow(a), tolp))
        }
      }
    }
  }

  ## PACKAGE VERSIONS, recorded per case and compared against the set validated at OQ time
  ## (2_94 review, M-1). Written into Outputs/ so the artefact travels with the verdicts it belongs
  ## to, and reported as a NOTE rather than a verdict: versions legitimately move, and a check that
  ## fails on every routine upgrade is a check somebody switches off. What this buys is that a
  ## verdict which moved after an upgrade can be explained instead of investigated from scratch.
  ## WHICH ACTA PRODUCED THIS. The frozen version FOLDER used to be the answer -- you could go
  ## and look at 2_98/. A package has no such folder, so the run has to say so itself, and the
  ## pair (git tag, this record) replaces it. A tag is the stronger half: it is content-addressed
  ## and cannot drift, whereas a directory is something a person can edit.
  ## Recorded SEPARATELY from script_version, which is parsed out of the ACTA_Script_*.R FILENAME
  ## and so reports 3_0 where DESCRIPTION says 3.0.0. The filename is the pipeline's own idea of
  ## its version; DESCRIPTION is the package's. When ACTA is not installed there is no package
  ## version to report and the field says so rather than guessing.
  .sv <- tryCatch({
    v <- res$script_version
    if (length(v) == 1L && !is.na(v) && nzchar(v)) as.character(v) else NULL
  }, error = function(e) NULL)
  ## ONLY REPORT A PACKAGE VERSION IF THE PACKAGE IS WHAT RAN. packageVersion("ACTA") answers
  ## about whatever is INSTALLED, which on a development machine is routinely an older ACTA than
  ## the checkout under test -- this record first printed "package 2.90.0" for a run that used
  ## the 3_0 sources. Same self-referential test as run_acta(): the enclosing environment of
  ## these helpers is namespace:ACTA when installed and a plain environment when they were
  ## source()d, so a stale install cannot fool it.
  .fromPkg <- isNamespace(parent.env(environment()))
  .av <- if (.fromPkg)
           tryCatch(as.character(utils::packageVersion("ACTA")), error = function(e) NA_character_)
         else NA_character_
  .sha <- tryCatch({
    o <- suppressWarnings(system2("git", c("-C", shQuote(code_dir), "rev-parse", "--short", "HEAD"),
                                  stdout = TRUE, stderr = FALSE))
    if (length(o) == 1L && nzchar(o)) o else NA_character_
  }, error = function(e) NA_character_)
  add("acta_version", "Which ACTA produced this run", "pass",
      sprintf("package %s; script %s%s",
              if (is.na(.av)) "sourced, not installed" else .av,
              if (!is.null(.sv))                                 .sv          else "unknown",
              if (is.na(.sha)) "" else sprintf("; git %s", .sha)))
  tryCatch(writeLines(c(sprintf("acta_package_version\t%s", if (is.na(.av)) "NA" else .av),
                        sprintf("acta_script_version\t%s",
                                if (!is.null(.sv))                                 .sv          else "NA"),
                        sprintf("git_sha\t%s", if (is.na(.sha)) "NA" else .sha),
                        sprintf("r_version\t%s", R.version.string)),
                      file.path(outDir, "acta_version.tsv")),
           error = function(e) invisible(NULL))
  .pv <- tryCatch(actaPackageVersions(code_dir), error = function(e) NULL)
  if (!is.null(.pv)) {
    tryCatch(utils::write.table(.pv, file.path(outDir, "package_versions.tsv"), sep = "\t",
                                row.names = FALSE, quote = FALSE),
             error = function(e) invisible(NULL))
    .base <- file.path(dirname(oq_dir), "validated_packages.tsv")
    if (file.exists(.base)) {
      .bl <- tryCatch(utils::read.delim(.base, stringsAsFactors = FALSE), error = function(e) NULL)
      .dr <- actaVersionDrift(.pv, .bl)
      add("package_versions", "Package versions against the OQ-validated set",
          if (!length(.dr)) "pass" else "warn",
          if (!length(.dr)) sprintf("%d package(s), all at the validated version", nrow(.pv))
          else sprintf("%d package(s) moved: %s", length(.dr), paste(.dr, collapse = "; ")))
    } else {
      add("package_versions", "Package versions against the OQ-validated set", "skip",
          sprintf("no validated_packages.tsv beside the cases; %d version(s) recorded in Outputs",
                  nrow(.pv)))
    }
  }

  expOut <- list.files(outDir, pattern = "TitrationExport.*[.]xlsx$", full.names = TRUE)
  ## Read ONCE and keep the frame: the row count and the combinatorial assertion below both need it,
  ## and re-reading an xlsx to answer a second question about the same sheet invites the two answers
  ## coming from different files if a stray export ever appears mid-run.
  expDf <- if (length(expOut))
             tryCatch(suppressMessages(readxl::read_excel(expOut[[1]], .name_repair = "minimal")),
                      error = function(e) NULL) else NULL
  nRows <- if (is.null(expDf)) 0L else nrow(expDf)
  want("export_rows", "Titration export: row count", nRows, "export_rows")

  ## The COMBINATORIAL DECLARATION, asserted per titration rather than counted. A column count
  ## moving from 41 to 42 tells you something arrived; it does not tell you the right groups were
  ## flagged, and the failure this guards against is a value landing on the wrong titration -- which
  ## a count cannot see. `statsForExport` keeps only the maxSI row per titration, so this also pins
  ## the per-group reduction: read off a single well instead, and a group whose number sits on
  ## non-winning rows would silently come back FALSE.
  .cbKeys <- c("Titration", "Combinatorial_group (n)", "Combinatorial (L)")
  if (is.null(exp) || is.null(exp$combinatorial)) {
    add("combinatorial", "Combinatorial declaration matches the baseline", "skip",
        "no `combinatorial` in expected.json")
  } else if (is.null(expDf) || !all(.cbKeys %in% names(expDf))) {
    add("combinatorial", "Combinatorial declaration matches the baseline", "fail",
        sprintf("the export is missing %s",
                paste(setdiff(.cbKeys, if (is.null(expDf)) character(0) else names(expDf)),
                      collapse = ", ")))
  } else {
    .norm <- function(x) { x <- trimws(as.character(x)); x[is.na(x)] <- ""; x }
    .gotT <- .norm(expDf[["Titration"]])
    .d <- unlist(lapply(names(exp$combinatorial), function(g) {
      w <- which(.gotT == g)
      if (!length(w)) return(sprintf("%s: no row in the export", g))
      e <- exp$combinatorial[[g]]
      gotG <- .norm(expDf[["Combinatorial_group (n)"]])[w[[1]]]
      gotL <- toupper(.norm(expDf[["Combinatorial (L)"]])[w[[1]]])
      wantG <- .norm(e[["group"]]); wantL <- toupper(.norm(e[["logical"]]))
      c(if (!identical(gotG, wantG))
          sprintf("%s group: expected '%s', got '%s'", g, wantG, gotG),
        if (!identical(gotL, wantL))
          sprintf("%s logical: expected %s, got %s", g, wantL, gotL))
    }))
    add("combinatorial", "Combinatorial declaration matches the baseline",
        if (!length(.d)) "pass" else "fail",
        if (!length(.d)) sprintf("%d titration(s) asserted, %d declared",
                                 length(exp$combinatorial),
                                 sum(vapply(exp$combinatorial,
                                            function(e) toupper(trimws(as.character(e[["logical"]]))) == "TRUE",
                                            logical(1))))
        else paste(.d, collapse = "; "))
  }

  ## ---- dashboard over Outputs/ --------------------------------------------------------------
  ## Scan root is the OQ folder, so Outputs/ is seen as one run; the html is written into Outputs/
  ## alongside everything else.
  progress("generating the dashboard ...")
  gen <- list.files(code_dir, pattern = "^ACTA_Dashboard.*[.]R$", full.names = TRUE)
  dashRows <- NA_integer_
  if (length(gen) == 1) {
    ## Sys.setenv, NOT system2(env=): that argument prepends VAR=value to the COMMAND LINE, which
    ## the shell then splits on whitespace -- and this repo lives under a path containing spaces and
    ## an "&". Setting it in this process instead lets the child inherit it intact.
    old <- Sys.getenv(c("ACTA_DASHBOARD_DIR", "ACTA_EXPORT_DIR"), unset = NA)
    ## ACTA_LAYOUT_DIR points the generator at the case's own workbook; its Template/ still
    ## resolves beside the generator, so the case needs no Template folder.
    ## ACTA_EXPORT_DIR is the case's PARENT (Diagnostics/), not the case itself, so the dashboard
    ## pools every case that has been run -- one combined view of the whole diagnostics suite
    ## rather than a per-case dashboard that hides the others. Pointed at oq_dir it saw exactly one
    ## run and OQ_Test2 wrote a 3-row dashboard while 4 rows were available.
    Sys.setenv(ACTA_DASHBOARD_DIR = outDir, ACTA_EXPORT_DIR = dirname(oq_dir),
               ACTA_LAYOUT_DIR = oq_dir)
    on.exit({ for (k in names(old)) if (is.na(old[[k]])) Sys.unsetenv(k) else
                do.call(Sys.setenv, setNames(list(old[[k]]), k)) }, add = TRUE)
    log <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
      c("--vanilla", shQuote(gen[[1]])), stdout = TRUE, stderr = TRUE))
    html <- list.files(outDir, pattern = "[.]html$", full.names = TRUE)
    if (length(html)) {
      txt <- paste(readLines(html[[1]], warn = FALSE), collapse = "\n")
      m <- regmatches(txt, regexpr("const DATA = \\[.*?\\];", txt))
      rows <- if (length(m))
        tryCatch(jsonlite::fromJSON(sub(";$", "", sub("const DATA = ", "", m)),
                                    simplifyVector = FALSE), error = function(e) NULL)
        else NULL
      ## Count only the rows THIS case contributed, matched on its ELN ID. The dashboard is pooled
      ## now, so a raw row count depends on which other cases happen to have been run and would
      ## make the baseline environment-dependent -- OQ_Test1 alone would assert 1, and 4 the moment
      ## OQ_Test2 had also run. Own-rows is deterministic; the pooled total is recorded as context.
      eln <- if (!is.null(exp) && !is.null(exp$eln_id)) as.character(exp$eln_id) else {
        ## Fallback only: the export filename is <date>_<ELN>_TitrationExport_<version>.xlsx.
        ef <- if (!is.null(res$export_file)) basename(res$export_file) else ""
        e2 <- regmatches(ef, regexpr("EXP[0-9]+", ef))
        if (length(e2)) e2 else NA_character_
      }
      pooled <- if (is.null(rows)) NA_integer_ else length(rows)
      dashRows <- if (is.null(rows) || is.na(eln)) NA_integer_ else
        sum(vapply(rows, function(r) identical(as.character(r[["eln_id"]]), eln), logical(1)))
      add("dashboard", "Dashboard written", "pass",
          sprintf("%s -- %s row(s) for %s of %s pooled from %s", basename(html[[1]]),
                  format(dashRows), eln, format(pooled), basename(dirname(oq_dir))))
    } else {
      add("dashboard", "Dashboard written", "fail", paste(utils::tail(log, 2), collapse = " | "))
    }
  } else add("dashboard", "Dashboard written", "skip", "no dashboard generator in the folder")
  want("dashboard_rows", "Dashboard: row count for this case", dashRows, "dashboard_rows")

  actaOQFinish(oq_dir, outDir, recs, proc.time()[["elapsed"]] - t0, exp)
}

## Verdict + a plain-text log beside the artefacts. Plain text on purpose: it has to be readable on
## a machine that may not have rendered anything yet.
actaOQFinish <- function(oq_dir, outDir, recs, elapsed, exp) {
  st  <- vapply(recs, function(r) r$status, "")
  ok  <- !any(st == "fail")
  ver <- if (any(st == "fail")) "FAIL" else if (any(st %in% c("warn", "skip"))) "PASS (with notes)" else "PASS"
  dir.create(outDir, recursive = TRUE, showWarnings = FALSE)
  logf <- file.path(outDir, "log.txt")
  ln <- c(sprintf("%s -- %s", basename(oq_dir), ver),
          strrep("=", 62),
          sprintf("run at      : %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
          sprintf("elapsed     : %.0f s", elapsed),
          sprintf("R           : %s", R.version.string),
          sprintf("machine     : %s / %s", Sys.info()[["sysname"]], Sys.info()[["nodename"]]),
          if (!is.null(exp$description)) sprintf("case        : %s", exp$description),
          "",
          sprintf("%-6s %-40s %s", "RESULT", "CHECK", "DETAIL"),
          strrep("-", 62))
  for (r in recs)
    ln <- c(ln, sprintf("%-6s %-40s %s", toupper(r$status), r$label,
                        if (is.na(r$detail)) "" else r$detail))
  ln <- c(ln, strrep("-", 62),
          sprintf("%d passed, %d failed, %d warn/skip", sum(st == "pass"), sum(st == "fail"),
                  sum(st %in% c("warn", "skip"))),
          sprintf("OVERALL: %s", ver), "",
          "Delete this Outputs/ folder once the result has been recorded.")
  writeLines(ln, logf)
  list(ok = ok, verdict = ver, recs = recs, log = logf, out_dir = outDir, elapsed_s = elapsed)
}


## ======================= COMPENSATION: PRE-FLIGHT AND OVERRIDE =======================
## The same lookup rule .actaReadCompCsv() uses, factored out so the app cannot disagree with the
## script about which file would be loaded: name CONTAINS "CompMatrix" (case-insensitive) and ends
## .csv, searched NON-RECURSIVELY in the working directory. Not "any csv in the folder".
actaCompCsvCandidates <- function(dir) {
  if (is.na(dir) || !nzchar(dir) || !dir.exists(dir)) return(character(0))
  h <- list.files(dir, pattern = "CompMatrix", ignore.case = TRUE, full.names = TRUE)
  h[grepl("[.]csv$", h, ignore.case = TRUE)]
}

## Is this csv usable as a matrix? .actaReadCompCsv() also rejects a non-square matrix, and that is
## currently a MID-RUN fatal -- cheap to catch here instead.
.actaCompCsvSquare <- function(path) {
  m <- try(utils::read.csv(path, row.names = 1, check.names = FALSE, nrows = 200), silent = TRUE)
  if (inherits(m, "try-error")) return(list(ok = FALSE, why = "could not be read as CSV"))
  if (nrow(m) != ncol(m))
    return(list(ok = FALSE, why = sprintf("is %d x %d -- a compensation matrix must be square",
                                          nrow(m), ncol(m))))
  list(ok = TRUE, why = NA_character_)
}

## Compensation pre-flight, for the ACTA layout check. Every mode gets the same protection: `self`
## used to be the only one with no pre-flight and would die mid-run naming a sample.
##
## A TYPO IS FATAL HERE, by decision. actaCompMode() warns and falls back to identity, which means
## a mistyped value produces a clean-looking run that is silently uncompensated -- the worst of the
## available outcomes. Blocking up front forces the author to say what they meant.
##
## `override` is the app's browsed file. The check validates the EFFECTIVE matrix, so a folder with
## no csv does not read as a failure when an override already supplies one.
actaCompensationRecords <- function(info, wd, fcs_root = NA_character_, override = NA_character_,
                                    antibody = character(0)) {
  raw  <- if (is.null(info) || !nrow(info)) NA_character_ else {
    hit <- match("compensation", tolower(trimws(names(info))))
    if (is.na(hit)) NA_character_ else trimws(as.character(info[[hit]][1]))
  }
  blank <- is.na(raw) || !nzchar(raw) || toupper(raw) == "NA"
  mode  <- if (blank) "" else tolower(raw)
  L <- "Compensation"

  if (blank)
    return(list(.appRec("compensation", L, "pass",
                        "Info!compensation is blank -- identity matrix, no compensation requested.", wd)))

  if (!mode %in% c("csv", "self"))
    return(list(.appRec("compensation", L, "fail",
      sprintf(paste0("Info!compensation is '%s', which is not recognised. Set it to blank, 'self' ",
                     "or 'csv' -- an unrecognised value would run UNCOMPENSATED and look like a ",
                     "clean run."), raw), wd)))

  if (identical(mode, "self")) {
    ## Acquisition-time matrix, per frame. Reading only the TEXT segment, so this is cheap.
    if (is.na(fcs_root) || !dir.exists(fcs_root))
      return(list(.appRec("compensation", L, "skip",
                          "'self' selected, but there is no FCS folder to check yet.", wd)))
    files <- unlist(lapply(antibody, function(ab) actaFindAntibodyFcs(fcs_root, ab)), use.names = FALSE)
    if (!length(files)) files <- list.files(fcs_root, pattern = "[.]fcs$", recursive = TRUE)
    if (!length(files))
      return(list(.appRec("compensation", L, "skip",
                          "'self' selected, but no FCS files were found to check.", fcs_root)))
    bad <- character(0)
    for (f in files) {
      kw <- tryCatch(flowCore::read.FCSheader(file.path(fcs_root, f))[[1]], error = function(e) NULL)
      keys <- c("$SPILLOVER", "$SPILL", "SPILL", "SPILLOVER", "spillover")
      has <- !is.null(kw) && any(keys %in% names(kw) &
                                 vapply(keys, function(k) k %in% names(kw) && nzchar(kw[[k]][1]), logical(1)))
      if (!has) bad <- c(bad, basename(f))
    }
    if (length(bad))
      return(list(.appRec("compensation", L, "fail",
        sprintf(paste0("'self' selected but %d of %d file(s) carry no acquisition matrix ",
                       "($SPILLOVER/$SPILL): %s. Re-export with the matrix, or use 'csv'."),
                length(bad), length(files),
                paste(utils::head(bad, 4), collapse = ", ")), fcs_root)))
    return(list(.appRec("compensation", L, "pass",
      sprintf("'self' -- acquisition-defined matrix found in all %d file(s).", length(files)), fcs_root)))
  }

  ## --- csv --------------------------------------------------------------------------------------
  if (!is.na(override) && nzchar(override)) {
    if (!file.exists(override))
      return(list(.appRec("compensation", L, "fail",
                          sprintf("Selected matrix does not exist: %s", override), override)))
    sq <- .actaCompCsvSquare(override)
    if (!sq$ok)
      return(list(.appRec("compensation", L, "fail",
                          sprintf("Selected matrix %s %s.", basename(override), sq$why), override)))
    return(list(.appRec("compensation", L, "pass",
      sprintf("'csv' -- using the selected matrix %s (overrides the working folder).",
              basename(override)), override)))
  }
  hits <- actaCompCsvCandidates(wd)
  if (!length(hits))
    return(list(.appRec("compensation", L, "fail",
      sprintf(paste0("'csv' selected but no *CompMatrix*.csv is present in '%s'. Put the exported ",
                     "matrix there, browse to one in the app, or set Info!compensation to 'self'."),
              wd), wd)))
  if (length(hits) > 1)
    return(list(.appRec("compensation", L, "fail",
      sprintf(paste0("'csv' selected but %d candidate matrices are present (%s). Leave exactly one, ",
                     "or browse to the one you want in the app."),
              length(hits), paste(basename(hits), collapse = ", ")), wd)))
  sq <- .actaCompCsvSquare(hits[1])
  if (!sq$ok)
    return(list(.appRec("compensation", L, "fail",
                        sprintf("%s %s.", basename(hits[1]), sq$why), hits[1])))
  list(.appRec("compensation", L, "pass",
               sprintf("'csv' -- %s", basename(hits[1])), hits[1]))
}

## Put `override` in place of whatever *CompMatrix*.csv is in `wd`, and hand back a restore
## function. The incumbents are MOVED to a temp folder rather than deleted, and put back on exit --
## copying alongside them would leave two candidates, and ">1 matrix" is fatal by design, so the
## override would break the very run it was meant to fix.
actaStageCompCsv <- function(wd, override) {
  noop <- function() invisible(NULL)
  if (is.na(override) || !nzchar(override) || !file.exists(override)) return(noop)
  incumbent <- actaCompCsvCandidates(wd)
  ## Already the file that would be picked up: nothing to stage.
  if (length(incumbent) == 1 &&
      identical(normalizePath(incumbent[1], mustWork = FALSE), normalizePath(override, mustWork = FALSE)))
    return(noop)
  stash <- file.path(tempdir(), sprintf("acta_comp_stash_%s", as.integer(runif(1, 1e6, 9e6))))
  dir.create(stash, recursive = TRUE, showWarnings = FALSE)
  moved <- character(0)
  for (f in incumbent) if (file.rename(f, file.path(stash, basename(f)))) moved <- c(moved, f)
  target <- file.path(wd, basename(override))
  copied <- file.copy(override, target, overwrite = TRUE, copy.date = TRUE)
  function() {
    if (copied && file.exists(target)) unlink(target)
    for (f in moved) file.rename(file.path(stash, basename(f)), f)
    unlink(stash, recursive = TRUE)
    invisible(NULL)
  }
}


## What the report needs to state about compensation: the mode, where the matrix came from, and the
## matrix itself. Resolved standalone rather than threaded out of actaCompensate(), which runs once
## per antibody group -- the mode is a run-level setting, so re-resolving is deterministic and gives
## the report one place to ask.
##
## NEVER throws. .actaReadCompCsv() is deliberately fatal on 0 / >1 / non-square, which is right for
## a run but would take the whole render down at the reporting stage, after all the work is done.
actaCompReport <- function(mode, wd, raw = NA_character_) {
  out <- list(mode = mode, raw = raw, source = NA_character_, matrix = NULL, note = NA_character_)
  if (!identical(mode, "csv")) {
    out$note <- if (identical(mode, "self"))
      "The acquisition-defined matrix ($SPILLOVER/$SPILL) recorded in each FCS file was applied."
      else "Identity matrix was applied -- the data were not compensated."
    return(out)
  }
  hits <- actaCompCsvCandidates(wd)
  if (length(hits) != 1) {
    out$note <- sprintf("compensation='csv' but %d matching *CompMatrix*.csv were found in %s.",
                        length(hits), wd)
    return(out)
  }
  m <- try(.actaReadCompCsv(wd), silent = TRUE)
  if (inherits(m, "try-error")) {
    out$note <- sprintf("Matrix at %s could not be read: %s", basename(hits[1]),
                        conditionMessage(attr(m, "condition")))
    return(out)
  }
  out$source <- hits[1]
  out$matrix <- m
  out$note   <- sprintf("Read from %s and applied to every sample.", basename(hits[1]))
  out
}
