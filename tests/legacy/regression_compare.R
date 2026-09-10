## Compare two ACTA snapshots. Fields that MUST differ between any two runs
## (wall-clock, capture time) are excluded by name; everything else must match
## exactly. Numerics are compared with identical() first, then all.equal() with a
## zero tolerance, so a 1e-16 drift is still reported rather than rounded away.
args <- commandArgs(trailingOnly = TRUE)
a <- readRDS(args[[1]]); b <- readRDS(args[[2]])
VOLATILE <- c("captured_at", "elapsed_s")

diffs <- character(0)
cmp <- function(path, x, y) {
  if (is.list(x) && is.list(y) && !is.data.frame(x) && !is.data.frame(y)) {
    ## UNNAMED lists are walked BY INDEX. This branch used to iterate union(names(x), names(y))
    ## unconditionally, which on an unnamed list is a loop over NULL -- it compared nothing and
    ## reported nothing. qcList$<group> is exactly that shape (an unnamed list of check objects),
    ## so the gate was blind to every QC change: growing from 4 checks to 5 went unreported, and a
    ## verdict flipping PASS -> FAIL would have too. That is the one thing this gate exists to catch.
    if (!length(names(x)) && !length(names(y))) {
      if (length(x) != length(y)) {
        diffs <<- c(diffs, sprintf("%s: length %d vs %d", path, length(x), length(y)))
        return(invisible())
      }
      for (i in seq_along(x)) cmp(sprintf("%s[[%d]]", path, i), x[[i]], y[[i]])
      return(invisible())
    }
    for (n in union(names(x), names(y))) {
      if (n %in% VOLATILE) next
      if (!n %in% names(x)) { diffs <<- c(diffs, sprintf("%s$%s: missing in A", path, n)); next }
      if (!n %in% names(y)) { diffs <<- c(diffs, sprintf("%s$%s: missing in B", path, n)); next }
      cmp(paste0(path, "$", n), x[[n]], y[[n]])
    }
    return(invisible())
  }
  if (identical(x, y)) return(invisible())
  if (is.data.frame(x) && is.data.frame(y)) {
    if (!identical(dim(x), dim(y))) {
      diffs <<- c(diffs, sprintf("%s: dim %s vs %s", path, paste(dim(x),collapse="x"), paste(dim(y),collapse="x")))
      return(invisible())
    }
    for (cn in names(x)) {
      cx <- x[[cn]]; cy <- y[[cn]]
      if (identical(cx, cy)) next
      bad <- which(!(is.na(cx) & is.na(cy)) & (is.na(cx) != is.na(cy) | as.character(cx) != as.character(cy)))
      if (is.numeric(cx) && is.numeric(cy)) {
        d <- suppressWarnings(max(abs(cx - cy), na.rm = TRUE))
        diffs <<- c(diffs, sprintf("%s[,%s]: %d cell(s) differ, max abs delta %.3g", path, cn, length(bad), d))
      } else {
        diffs <<- c(diffs, sprintf("%s[,%s]: %d cell(s) differ", path, cn, length(bad)))
      }
    }
    return(invisible())
  }
  ae <- isTRUE(all.equal(x, y, tolerance = 0))
  diffs <<- c(diffs, sprintf("%s: differs%s", path, if (ae) " (all.equal ok, identical() not)" else ""))
}
cmp("snap", a, b)

if (!length(diffs)) {
  cat("IDENTICAL -- snapshots match on every compared field\n")
  quit(status = 0)
}
cat(sprintf("%d DIFFERENCE(S)\n", length(diffs)))
for (d in diffs) cat("  ", d, "\n")
quit(status = 1)
