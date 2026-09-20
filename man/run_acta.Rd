\name{run_acta}
\alias{run_acta}
\title{Run one ACTA titration analysis}
\description{
Run the ACTA pipeline over a single working folder: read the instructions workbook and the FCS
files it points at, gate, compute a Stain Index per stain quantity, fit a Michaelis-Menten curve,
and optionally write the plots and render the PDF report.
}
\usage{
run_acta(version_dir = getwd(), report = TRUE, plots = TRUE, quiet = FALSE,
         code_dir = NULL)
}
\arguments{
  \item{version_dir}{the working folder: the instructions workbook, \code{Titration_FCS/} and
    everything the run writes. Must exist.}
  \item{report}{render the PDF report. Set \code{FALSE} to skip the LaTeX step, which is the slow
    half and the one with external dependencies.}
  \item{plots}{write the per-run PNGs into \code{version_dir/Plots}.}
  \item{quiet}{suppress the pipeline's progress messages.}
  \item{code_dir}{folder holding the pipeline script, report \code{.Rmd} and dashboard generator.
    \code{NULL} (the default) means resolve it: \code{version_dir} when it looks like a flat
    checkout, otherwise the installed package's \code{inst/pipeline}. The fallback is announced in
    a message and recorded in \code{$code_dir} of the result, never silent.}
}
\details{
\code{code_dir} is \emph{resolved}, not defaulted to \code{version_dir}. That was the old
behaviour and it is wrong for every package install: \code{library(ACTA); run_acta()} from a
folder of data went looking for the pipeline among the data and stopped.

Sitting in a clone whose code differs from the installed package, a bare call uses the
\emph{installed} code. That is why the fallback names the version and the path out loud, and why
\code{$code_dir} carries it into the result, where an automated caller can assert on it.
}
\value{
Invisibly, a list describing the run. The fields worth depending on are:
\describe{
  \item{\code{ok}}{\code{TRUE} if the analysis completed.}
  \item{\code{analysis_error}, \code{report_error}}{the first error from each stage, or \code{NA}.}
  \item{\code{report_ok}, \code{report_skipped}, \code{wrote_plots}}{what was attempted and what
    came out.}
  \item{\code{version_dir}, \code{code_dir}}{the folders actually used.}
  \item{\code{script}, \code{script_version}, \code{seed}, \code{elapsed_s}}{provenance.}
  \item{\code{report_file}, \code{export_file}, \code{plots_dir}}{paths to the artefacts, each
    \code{NA} unless \emph{this} run produced it.}
  \item{\code{stats}, \code{qcList}, \code{mmFits}, \code{gs}}{the per-sample statistics, the QC
    records, the Michaelis-Menten fits and the \code{GatingSet}.}
  \item{\code{env}}{the environment the pipeline ran in, for anything not listed above.}
}
Pass the whole list to \code{\link{actaRunArtefacts}} or \code{\link{actaReadyMessage}} to
describe the outcome, and to \code{\link{actaRunLogEntry}} to record it.
}
\seealso{
\code{\link{acta_app}} for the app, \code{\link{actaOQRun}} for a diagnostic case,
\code{\link{actaPrevalidateRecords}} for the pre-flight this run performs internally.
}
\examples{
\dontrun{
library(ACTA)
res <- run_acta("~/titrations/EXP00000001")
res$ok
res$report_file
}
}
