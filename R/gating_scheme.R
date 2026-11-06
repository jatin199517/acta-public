## The gating scheme as a readable tree, replacing plot(gs).
##
## flowWorkspace's plot(gs) draws the hierarchy with Rgraphviz: it is serviceable on four nodes
## and unreadable past a dozen, it cannot be styled to match anything else in the report, and it
## drags in a graph-rendering stack to draw what is structurally a list of paths. ACFA draws the
## same thing as a dendrogram and it reads far better, so ACTA does too.
##
## NO igraph AND NO ggraph. The tree is a set of slash-separated paths; the layout is one pass to
## assign depths and one pass to stack leaves. Two new dependencies to draw five boxes would be a
## poor trade, and this package already pins macOS to an older R because of a dependency's
## packaging.


## COLOUR IS THE GATING METHOD, which is the one thing a hierarchy of names cannot show: two nodes
## called "Live" mean different things if one was drawn by flowMeans and the other by hand. ACFA's
## gating tree does the same and this mirrors it, so the two projects' figures read alike.
##
## Okabe-Ito, as ACFA uses: it stays distinguishable under the common colour-vision deficiencies,
## which matters for a figure whose entire job is telling categories apart. `boundary` and
## `singletGate` are openCyto's own methods rather than ACTA plugins; `other` is the fallback and
## is left white so an unrecognised method is visibly unclassified rather than quietly mis-coloured.
##
## OKABE-ITO IS EXHAUSTED AND THIS LIST HAS OUTGROWN IT. The set holds eight hues plus black; the
## seven usable ones are spent, and #8877FF, #006633 and the greys were already chosen outside it.
## Each addition past that point is a compromise rather than a guarantee, picked for the widest
## separation from its nearest neighbour in BOTH hue and saturation -- gate_costain's magenta
## against gate_flowclust_2d's desaturated pink is the current closest pair. Black is deliberately
## not used: the points are drawn shape = 21 with a grey25 stroke, so a black fill would swallow
## its own outline and read as `boundary`.
##
## IF A FOURTEENTH METHOD IS ADDED, colour alone stops carrying identity and the figure needs a
## second channel -- shape, or direct labels on the nodes. Worth doing before, not after.
ACTA_GATE_METHOD_PAL <- c(
  root                    = "grey30",
  boundary                = "#4D4D4D",
  singletGate             = "#0072B2",
  gate_flowmeans          = "#E69F00",
  gate_flowmeans_costain  = "#D55E00",
  gate_mindensity_costain = "#F0E442",
  gate_mind_fmeans_costain  = "#B15928",
  gate_mind2_fmeans_costain = "#6A3D9A",
  gate_costain            = "#DD3497",
  gate_auto2_costain      = "#009E73",
  gate_auto_costain_dip   = "#56B4E9",
  gate_auto_costain_gmm   = "#8877FF",
  gate_flowclust_2d       = "#CC79A7",
  gate_quad_tmix          = "#006633",
  other                   = "#FFFFFF")

## Which gating_template row produced each node.
##
## A node's name is NOT always its row's alias, and that is the whole difficulty:
##   plain gate   the node IS the alias                              -> "Live"
##   +/- split    openCyto names children "<alias>+" / "<alias>-"    -> "Reagent+"
##   quadrant     openCyto names children from the DIMS, ignoring
##                the alias entirely                                 -> "BV 605-A+APC (...)-A-"
##
## The quadrant case is why this cannot fall back to "the row that made my parent". Under one
## parent there are usually TWO rows -- the quad row and the reagent row -- with different
## methods, so attributing a quad child to its parent's row returns whatever drew the parent.
## In OQ_Test2 that happens to give the right answer by coincidence (Live and the quad row are
## both gate_flowmeans) and would be wrong the moment they differ. A quadrant child is matched
## instead against the DIMS of the sibling rows under the same parent, which is what names it.
.gsMethods <- function(nodes, parents, gt) {
  if (is.null(gt) || !is.data.frame(gt) || !nrow(gt)) return(rep("other", length(nodes)))
  nm <- tolower(trimws(names(gt)))
  gcol <- function(n) { i <- which(nm == n); if (length(i)) trimws(as.character(gt[[i[1]]])) else NULL }
  alias <- gcol("alias"); meth <- gcol("gating_method")
  if (is.null(alias) || is.null(meth)) return(rep("other", length(nodes)))
  par <- gcol("parent"); dims <- gcol("dims")
  keep <- !is.na(alias) & nzchar(alias) & !startsWith(alias, "#")
  alias <- alias[keep]; meth <- meth[keep]
  par <- if (is.null(par)) rep(NA_character_, length(alias)) else par[keep]
  dims <- if (is.null(dims)) rep(NA_character_, length(alias)) else dims[keep]

  leafName <- function(p) { z <- strsplit(p, "/", fixed = TRUE)[[1]]; z[length(z)] }
  out <- vapply(seq_along(nodes), function(i) {
    lab <- leafName(nodes[i])
    h <- match(tolower(lab), tolower(alias))                       # the alias itself
    if (is.na(h)) h <- match(tolower(sub("[+-]$", "", lab)), tolower(alias))   # a +/- child
    if (is.na(h) && !is.na(parents[i])) {
      ## A quadrant child: find the sibling row whose two dims spell this node's name. Compared
      ## as FIXED strings -- a dim is "APC (Allophycocyanin)-A" and those parentheses would be a
      ## capture group in a regex.
      pl <- leafName(parents[i])
      cand <- which(!is.na(par) & tolower(par) == tolower(pl))
      base <- sub("[+-]$", "", lab)
      for (k in cand) {
        if (is.na(dims[k])) next
        d <- trimws(strsplit(dims[k], ",", fixed = TRUE)[[1]])
        d <- d[nzchar(d)]
        ## The 2D quadrant child: "<dim1><sign><dim2><sign>".
        if (length(d) == 2L && startsWith(lab, d[1]) && grepl(d[2], lab, fixed = TRUE)) { h <- k; break }
        ## ...and its 1D MARGINALS, "<dim><sign>". openCyto emits these alongside the quadrants
        ## for a `+/-+/-` row, and they are the same gate -- without this they fell through to
        ## "other" and two nodes in every OQ_Test2 tree were drawn white as if the method were
        ## unknown, next to four siblings from the same template row that were coloured correctly.
        if (base %in% d) { h <- k; break }
      }
      ## Still nothing, and exactly one row sits under this parent: it must be that one.
      if (is.na(h) && length(cand) == 1L) h <- cand
    }
    if (is.na(h)) NA_character_ else meth[h]
  }, "")
  out[is.na(out) | !nzchar(out)] <- "other"
  out[!out %in% names(ACTA_GATE_METHOD_PAL)] <- "other"
  out
}

## Paths -> a layout, as a pure function of the paths. Separated from the drawing so the geometry
## can be tested without constructing a GatingSet, which needs FCS files on disk.
##
## x is DEPTH (root at the left) and y stacks the leaves in the order the paths arrive, with each
## parent centred on the children it owns. That ordering is deliberate: it follows the
## gating_template's own row order, so the picture matches the sheet somebody wrote.
actaGatingTreeLayout <- function(paths) {
  paths <- unique(as.character(paths))
  paths <- paths[!is.na(paths) & nzchar(paths)]
  paths <- setdiff(paths, c("root", "/"))
  if (!length(paths)) return(NULL)

  norm <- sub("^/+", "", paths)
  norm <- norm[nzchar(norm)]
  ## Every ancestor is a node too, even when the gating template never names it on its own row.
  all_nodes <- unique(unlist(lapply(norm, function(p) {
    parts <- strsplit(p, "/", fixed = TRUE)[[1]]
    vapply(seq_along(parts), function(i) paste(parts[seq_len(i)], collapse = "/"), "")
  })))
  ## Shallowest first, so a parent always precedes its children.
  depth <- lengths(strsplit(all_nodes, "/", fixed = TRUE))
  all_nodes <- all_nodes[order(depth, match(all_nodes, all_nodes))]
  depth <- lengths(strsplit(all_nodes, "/", fixed = TRUE))

  parent <- vapply(all_nodes, function(p) {
    parts <- strsplit(p, "/", fixed = TRUE)[[1]]
    if (length(parts) <= 1L) NA_character_ else paste(parts[-length(parts)], collapse = "/")
  }, "")
  label <- vapply(strsplit(all_nodes, "/", fixed = TRUE), function(p) p[length(p)], "")
  ## NOT WRAPPED. Wrapping existed when each node was a filled box and a 33-character quadrant
  ## name made the box wider than the panel. With the label beside the point, as ACFA draws it, a
  ## long name simply extends into the margin the canvas already pays for -- and an unwrapped name
  ## is the one openCyto actually assigned, which is what somebody reconciling this against a
  ## gating template needs to read.

  ## Leaves get consecutive y; internal nodes are centred on their descendants. Computed from the
  ## deepest level upward so a parent's children always have a y by the time it needs one.
  y <- stats::setNames(rep(NA_real_, length(all_nodes)), all_nodes)
  is_leaf <- !(all_nodes %in% parent)
  y[all_nodes[is_leaf]] <- seq_len(sum(is_leaf))
  for (d in rev(sort(unique(depth)))) {
    for (nd in all_nodes[depth == d & !is_leaf]) {
      kids <- all_nodes[!is.na(parent) & parent == nd]
      if (length(kids)) y[[nd]] <- mean(y[kids], na.rm = TRUE)
    }
  }
  data.frame(node = all_nodes, label = label, parent = unname(parent),
             x = depth, y = unname(y[all_nodes]),
             leaf = is_leaf, stringsAsFactors = FALSE)
}

## The population paths of a GatingSet, in the order flowWorkspace reports them.
.gsPaths <- function(gs) {
  p <- tryCatch(flowWorkspace::gs_get_pop_paths(gs, path = "full"), error = function(e) NULL)
  if (is.null(p)) p <- tryCatch(flowWorkspace::gs_get_pop_paths(gs), error = function(e) NULL)
  p
}

## Draw it, ACFA-style. Accepts a GatingSet or a bare character vector of paths, so the report
## passes `gs` and a test passes strings. `gating_template` supplies the method behind each node.
##
## NODES ARE POINTS, NOT BOXES, and the label sits beside the point -- to the LEFT for an internal
## node, to the RIGHT for a leaf. The first version drew a filled box per node and the quadrant
## names ("BV 605-A+APC (Allophycocyanin)-A-") made boxes wider than the panel. Beside-the-point
## labelling is what ACFA does and it removes the problem rather than wrapping around it: a long
## leaf name simply runs into the right margin, which the canvas pays for once.
actaGatingSchemePlot <- function(x, title = NULL, gating_template = NULL) {
  paths <- if (is.character(x)) x else .gsPaths(x)
  lay <- actaGatingTreeLayout(paths)
  if (is.null(lay) || !nrow(lay)) return(NULL)
  lay$method <- .gsMethods(lay$node, lay$parent, gating_template)

  ## Elbow connectors rather than straight diagonals: with several children on one parent the
  ## diagonals cross each other and the eye cannot tell which line belongs to which node.
  seg <- lay[!is.na(lay$parent), , drop = FALSE]
  if (nrow(seg)) {
    pi_ <- match(seg$parent, lay$node)
    seg$px <- lay$x[pi_]; seg$py <- lay$y[pi_]
    seg$mx <- (seg$px + seg$x) / 2
  }
  present <- intersect(names(ACTA_GATE_METHOD_PAL), unique(lay$method))
  ## Longest label on each side -- internal labels run left of their point, leaves run right.
  lw <- max(nchar(lay$label[!lay$leaf]), 1L)
  rw <- max(nchar(lay$label[lay$leaf]), 1L)

  p <- ggplot2::ggplot()
  if (nrow(seg)) {
    p <- p +
      ggplot2::geom_segment(data = seg, ggplot2::aes(x = px, y = py, xend = mx, yend = py),
                            colour = "grey70", linewidth = 0.3) +
      ggplot2::geom_segment(data = seg, ggplot2::aes(x = mx, y = py, xend = mx, yend = y),
                            colour = "grey70", linewidth = 0.3) +
      ggplot2::geom_segment(data = seg, ggplot2::aes(x = mx, y = y, xend = x, yend = y),
                            colour = "grey70", linewidth = 0.3)
  }
  p <- p +
    ggplot2::geom_point(data = lay, ggplot2::aes(x = x, y = y, fill = method),
                        shape = 21, size = 3.1, stroke = 0.35, colour = "grey25") +
    ggplot2::geom_text(data = lay[!lay$leaf, , drop = FALSE],
                       ggplot2::aes(x = x, y = y, label = label),
                       size = 3.0, hjust = 1, nudge_x = -0.08, colour = "grey15") +
    ggplot2::geom_text(data = lay[lay$leaf, , drop = FALSE],
                       ggplot2::aes(x = x, y = y, label = label),
                       size = 3.0, hjust = 0, nudge_x = 0.08, colour = "grey15") +
    ggplot2::scale_fill_manual(values = ACTA_GATE_METHOD_PAL, breaks = present,
                               name = "Gating method", drop = TRUE) +
    ## EXPANSION IN DATA UNITS, SIZED BY THE LABELS. A multiplicative expansion is a fraction of
    ## the depth range, so it does not grow when the labels do: a 33-character quadrant name ran
    ## off the panel and under the legend while a 4-character one had room to spare. One depth
    ## unit holds roughly 16 characters at this text size, so the margin each side is asked for in
    ## those terms. clip = "off" lets a label that still overshoots run into the plot margin
    ## rather than being cut mid-word.
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(add = c(lw / 16, rw / 16))) +
    ggplot2::scale_y_reverse(expand = ggplot2::expansion(mult = 0.16)) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_void(base_size = 10) +
    ggplot2::theme(legend.position = "right",
                   legend.key.size = ggplot2::unit(9, "pt"),
                   legend.text = ggplot2::element_text(size = 7),
                   legend.title = ggplot2::element_text(size = 7.5),
                   plot.margin = ggplot2::margin(6, 6, 6, 6))
  if (!is.null(title) && nzchar(title))
    p <- p + ggplot2::labs(title = title) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 11,
                                                        margin = ggplot2::margin(b = 8)))
  p
}

## Sensible canvas for a tree of this shape -- wide enough for the deepest chain, tall enough for
## the leaves. Returned rather than hardcoded so the report and the PNG agree.
actaGatingSchemeSize <- function(x) {
  lay <- actaGatingTreeLayout(if (is.character(x)) x else .gsPaths(x))
  if (is.null(lay)) return(c(width = 8, height = 5))
  ## Proportioned so a typical four-deep, three-leaf tree is a readable block rather than a
  ## letterbox: the first draft used 2.3in per level and produced a picture that was mostly
  ## whitespace with the boxes stranded at the edges.
  ## The deepest column's labels stick out to the RIGHT of their anchor point, so the canvas has
  ## to pay for the widest one or geom_label draws it off the panel. Measured in characters at the
  ## label's own point size rather than guessed.
  ## Labels sit BESIDE the points, so width pays for the longest internal label on the left, the
  ## longest leaf label on the right, and a fixed legend column. Height is one row per leaf.
  ## Capped so a pathological hierarchy cannot produce a figure the report cannot place.
  lw <- max(nchar(lay$label[!lay$leaf]), 0L)
  rw <- max(nchar(lay$label[lay$leaf]), 0L)
  ## 1.5in of the width is the legend column, which sits outside the panel.
  c(width  = max(6, min(15, 1.15 * max(lay$x) + 0.07 * (lw + rw) + 2.1)),
    height = max(2.6, min(11, 0.42 * sum(lay$leaf) + 1.1)))
}
