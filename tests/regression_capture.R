## ---------------------------------------------------------------------------
## ACTA regression snapshot.
##
## Sources a version's ACTA_Script_*.R in a clean session and captures everything
## a refactor could plausibly change: the per-well statistics, the QC verdicts, the
## full gate tree with counts and parent percentages, the Michaelis-Menten fits, and the
## titration export as written to disk.
##
## The pipeline seeds its RNG per antibody group, so two runs of the SAME code must
## produce an identical snapshot. That is what makes this a pass/fail gate rather
## than a judgement call: any difference after the refactor is a real behaviour
## change, not noise.
##
##   Rscript capture.R <version_dir> <out.rds>
## ---------------------------------------------------------------------------
args    <- commandArgs(trailingOnly = TRUE)
verDir  <- normalizePath(args[[1]], mustWork = TRUE)
outRds  <- args[[2]]

scriptFile <- list.files(verDir, pattern = "^ACTA_Script.*\\.R$", full.names = TRUE)
stopifnot(length(scriptFile) == 1)
message("[capture] sourcing ", basename(scriptFile))

t0 <- proc.time()[["elapsed"]]
source(scriptFile)                      # setwd()s to its own folder via this.path()
elapsed <- proc.time()[["elapsed"]] - t0
message(sprintf("[capture] run finished in %.1f s", elapsed))

pick <- function(nm) if (exists(nm, envir = globalenv())) get(nm, envir = globalenv()) else NULL

## --- gate tree: counts + parent percentages for every sample x population ------
gateStats <- NULL
gs <- pick("gs")
if (!is.null(gs)) {
  gateStats <- tryCatch({
    d <- flowWorkspace::gs_pop_get_count_fast(gs, statistic = "count")
    d <- as.data.frame(d)
    d$pct_of_parent <- d$Count / d$ParentCount
    d[order(d$name, d$Population), ]
  }, error = function(e) paste("ERROR:", conditionMessage(e)))
}

## --- the export, read back from disk exactly as written ------------------------
exportFile <- list.files(verDir, pattern = "TitrationExport.*\\.xlsx$", full.names = TRUE)
exportData <- if (length(exportFile)) {
  suppressMessages(as.data.frame(readxl::read_excel(exportFile[[which.max(file.mtime(exportFile))]],
                                                    .name_repair = "minimal")))
} else NULL

snap <- list(
  meta = list(script = basename(scriptFile), version = pick("ScriptVersion"),
              captured_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), elapsed_s = elapsed,
              r_version = R.version.string, seed = pick("ACTA_SEED")),
  stats       = pick("stats"),
  qcList      = pick("qcList"),
  gateStats   = gateStats,
  mmFits      = pick("mmFits"),
  maxValue    = pick("actaMaxValue"),
  transArgs   = pick("actaTransEff"),
  antibody    = pick("antibody"),
  exportFile  = if (length(exportFile)) basename(exportFile) else NULL,
  exportData  = exportData,
  plots       = sort(basename(list.files(file.path(verDir, "Plots"), pattern = "\\.png$")))
)
saveRDS(snap, outRds)
message("[capture] snapshot -> ", outRds)
for (n in c("stats","gateStats","exportData"))
  message(sprintf("[capture]   %-11s %s", n,
                  if (is.data.frame(snap[[n]])) paste(dim(snap[[n]]), collapse=" x ") else class(snap[[n]])[1]))
message("[capture]   qc groups   ", length(snap$qcList), " | plots ", length(snap$plots))
