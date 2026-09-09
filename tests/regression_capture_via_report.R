## Snapshot via run_acta(report = TRUE, plots = TRUE) -- the app's default mode.
args <- commandArgs(trailingOnly = TRUE)
verDir <- normalizePath(args[[1]], mustWork = TRUE); outRds <- args[[2]]
suppressMessages({library(flowCore); library(readxl)})
h <- new.env(parent = globalenv())
suppressWarnings(sys.source(list.files(verDir, pattern = "^ACTA_Function.*\\.R$", full.names = TRUE), envir = h))
setwd(tempdir())
res <- h$run_acta(verDir, report = TRUE, plots = TRUE, quiet = TRUE)
message(sprintf("[report mode] ok=%s report_ok=%s wrote_plots=%s %.0fs",
                res$ok, res$report_ok, res$wrote_plots, res$elapsed_s))
if (!is.na(res$report_error)) message("[report mode] report_error: ", res$report_error)
for (a in h$actaRunArtefacts(res))
  message(sprintf("   %-22s %s", a$label, if (is.na(a$path)) "MISSING" else basename(a$path)))
gateStats <- { d <- as.data.frame(flowWorkspace::gs_pop_get_count_fast(res$gs, statistic = "count"))
               d$pct_of_parent <- d$Count / d$ParentCount; d[order(d$name, d$Population), ] }
saveRDS(list(
  meta = list(script = res$script, version = res$script_version, captured_at = "x",
              elapsed_s = 0, r_version = R.version.string, seed = res$seed),
  stats = res$stats, qcList = res$qcList, gateStats = gateStats, mmFits = res$mmFits,
  maxValue = res$maxValue, transArgs = res$transArgs, antibody = res$antibody,
  exportFile = basename(res$export_file),
  exportData = suppressMessages(as.data.frame(readxl::read_excel(res$export_file, .name_repair = "minimal"))),
  plots = sort(basename(list.files(file.path(verDir, "Plots"), pattern = "[.]png$")))
), outRds)
message("[ready] ", h$actaReadyMessage(res, dashboard_ok = FALSE))
message("[ready] ", h$actaReadyMessage(res, dashboard_ok = TRUE))
