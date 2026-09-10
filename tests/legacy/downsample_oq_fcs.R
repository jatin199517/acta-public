## Downsample every OQ FCS to N events so the case is small enough to live in git and fast enough
## that people actually run it.
##
## The subsample is SEEDED PER FILE (from the well id), so the same file always yields the same
## events. A regression test whose input changes between runs cannot assert on numbers at all --
## that determinism is the whole point.
suppressMessages(library(flowCore))
N <- 20000L
dir <- "OQ_Test1/Titration_FCS/PLATE_1/Strep_PE"
f <- list.files(dir, pattern = "[.]fcs$", full.names = TRUE)
stopifnot(length(f) > 0)
tot <- 0; kept <- 0
for (p in f) {
  ff <- suppressWarnings(read.FCS(p, truncate_max_range = FALSE, transformation = FALSE))
  n  <- nrow(ff); tot <- tot + n
  well <- toupper(trimws(keyword(ff)[["$WELLID"]]))
  if (n > N) {
    set.seed(sum(utf8ToInt(well)))                  # deterministic, and different per well
    idx <- sort(sample.int(n, N))
    ff  <- ff[idx, ]
  }
  kept <- kept + nrow(ff)
  ## Drop the path-shaped keywords write.FCS would otherwise re-add from this machine.
  kk <- keyword(ff)
  for (k in names(kk)[vapply(kk, function(v) is.character(v) && length(v) == 1 &&
                             grepl("^(/|[A-Za-z]:\\\\)", v), logical(1))]) keyword(ff)[[k]] <- NULL
  suppressWarnings(write.FCS(ff, p))
  cat(sprintf("  %-34s %7d -> %6d\n", basename(p), n, nrow(ff)))
}
cat(sprintf("total %d -> %d events\n", tot, kept))
