## Same snapshot as regression_capture.R, but driven through run_acta() from a DIFFERENT working
## directory -- so this also proves the callable path does not depend on the caller's cwd.
args <- commandArgs(trailingOnly = TRUE)
verDir <- normalizePath(args[[1]], mustWork = TRUE); outRds <- args[[2]]
suppressMessages({library(flowCore); library(readxl)})
fn <- list.files(verDir, pattern = "^ACTA_Function.*\\.R$", full.names = TRUE)
helpers <- new.env(parent = globalenv()); suppressWarnings(sys.source(fn, envir = helpers))
setwd(tempdir())                                   # deliberately NOT the version folder
message("[via_run_acta] cwd before: ", getwd())
res <- helpers$run_acta(verDir)
message("[via_run_acta] cwd after : ", getwd(), "   (must equal cwd before)")
env <- res$env
pick <- function(nm) if (exists(nm, envir = env, inherits = FALSE)) get(nm, envir = env) else NULL
gateStats <- tryCatch({
  d <- as.data.frame(flowWorkspace::gs_pop_get_count_fast(res$gs, statistic = "count"))
  d$pct_of_parent <- d$Count / d$ParentCount; d[order(d$name, d$Population), ]
}, error = function(e) paste("ERROR:", conditionMessage(e)))
ex <- res$export_file
snap <- list(
  meta = list(script = res$script, version = res$script_version,
              captured_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), elapsed_s = res$elapsed_s,
              r_version = R.version.string, seed = res$seed),
  stats = res$stats, qcList = res$qcList, gateStats = gateStats, mmFits = res$mmFits,
  maxValue = res$maxValue, transArgs = res$transArgs, antibody = res$antibody,
  exportFile = if (!is.na(ex)) basename(ex) else NULL,
  exportData = if (!is.na(ex)) suppressMessages(as.data.frame(readxl::read_excel(ex, .name_repair="minimal"))) else NULL,
  plots = sort(basename(list.files(file.path(verDir, "Plots"), pattern = "\\.png$")))
)
saveRDS(snap, outRds)
message("[via_run_acta] artefacts:")
for (a in helpers$actaRunArtefacts(res))
  message(sprintf("   %-22s %s", a$label, if (is.na(a$path)) "MISSING" else basename(a$path)))
