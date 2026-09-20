\name{actaDependencyChecks}
\alias{actaDependencyChecks}
\title{Check the machine can run ACTA}
\description{
Roll up everything outside the workbook that a run depends on: the R packages the analysis and the
app need, the LaTeX distribution, and Pandoc.
}
\usage{
actaDependencyChecks(version_dir,
                     app_pkgs = c("shiny", "processx", "this.path", "readxl",
                                  "openxlsx", "jsonlite", "rmarkdown"))
}
\arguments{
  \item{version_dir}{the folder whose pipeline script declares the analysis package list.}
  \item{app_pkgs}{packages the app itself needs, as distinct from the analysis.}
}
\details{
Packages are reported one row per \emph{source} rather than one row per package: about
thirty-five green ticks say nothing an operator can act on, whereas "all 35 present" and "3
missing: \dots" do.

This function calls \code{requireNamespace()}, which \emph{loads} packages, and a broken install
can take R down with it. \code{\link{actaDiagnosticBundle}} therefore calls it last, after
everything else has been collected.
}
\value{
A list of status records (see \link{ACTA-package}). A missing analysis package is a \code{"fail"}
whose detail names the packages and how they are normally installed; a missing Pandoc is a
\code{"warn"}, since it is needed only for the PDF report.
}
\seealso{\code{\link{actaRepairPath}}, \code{\link{actaDiagnosticBundle}}}
\examples{
recs <- actaDependencyChecks(system.file("pipeline", package = "ACTA"))
data.frame(id     = vapply(recs, `[[`, "", "id"),
           status = vapply(recs, `[[`, "", "status"))
}
