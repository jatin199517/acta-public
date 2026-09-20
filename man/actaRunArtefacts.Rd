\name{actaRunArtefacts}
\alias{actaRunArtefacts}
\alias{actaReadyMessage}
\title{Describe what a run actually produced}
\description{
Turn a \code{\link{run_acta}} result into a report of the artefacts on disk, and into the
one-sentence completion message.
}
\usage{
actaRunArtefacts(res)

actaReadyMessage(res, dashboard_ok = FALSE)
}
\arguments{
  \item{res}{the list returned by \code{\link{run_acta}}.}
  \item{dashboard_ok}{whether the dashboard was regenerated. The dashboard is written outside
    \code{run_acta()}, so its result cannot report on it.}
}
\details{
Both are built from what was produced, not from what was asked for. "Report and Dashboard are
ready" must not appear when the render failed.

An artefact is only claimed when \emph{this} run produced it. Globbing the folder finds a PDF left
by an earlier run, so a report-less run reported yesterday's report as ready; \code{Plots/} is
never emptied between runs, so a failed run found the previous set sitting there and reported
"Plots (8 PNG)" as its own. Any partial files are still on disk either way -- what changes is that
the summary stops implying this run wrote them.

\code{report_ok} alone is not sufficient either: it says a PDF was rendered, not that one is in
this folder now, and those came apart the moment a render could be staged elsewhere.

\code{actaReadyMessage()} has no unconditional fallback. "Titration export is ready" used to be
one, so a run that produced nothing at all still ended on a sentence naming an artefact.
}
\value{
\code{actaRunArtefacts()} returns a list with one entry per artefact class (\code{report},
\code{export}, \code{plots}), each a list of \code{label}, \code{path} and \code{skipped}.
\code{skipped} names the reason when there is no path.

\code{actaReadyMessage()} returns a single string, which may say that nothing was produced.
}
\seealso{\code{\link{run_acta}}, \code{\link{actaRunLogEntry}}}
\examples{
## A run that failed before writing anything:
actaReadyMessage(list(ok = FALSE, report_ok = FALSE))
}
