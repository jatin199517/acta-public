\name{acta_app}
\alias{acta_app}
\title{Launch the ACTA desktop app}
\description{
Start the Shiny app that drives ACTA: pick a working folder, run the pre-flight checks, run the
analysis, and read the report and dashboard. This is the supported way to launch the app from an
installed package.
}
\usage{
acta_app(work_dir = getwd(), launch.browser = TRUE, \dots)
}
\arguments{
  \item{work_dir}{the app's working folder: the instructions workbook, \code{Titration_FCS/}, the
    exports and the run log. Must exist. Defaults to the current directory.}
  \item{launch.browser}{passed to \code{\link[shiny]{runApp}}.}
  \item{\dots}{further arguments for \code{\link[shiny]{runApp}}.}
}
\details{
\code{shiny::runApp()} on the app file is \emph{not} equivalent. \code{runApp} sets the working
directory to the app's own folder before sourcing it, so the app would treat the package library
as the working folder. \code{acta_app()} records \code{work_dir} into the \code{ACTA_WORK_DIR}
environment variable first, which is what makes the code/work split hold: the code stays where it
was installed and is never written to. A package library is often read-only, and a run log inside
it would fail on a managed machine.

The shipped diagnostic cases are the one case where that used to break down, and it is the panel a
package-only user is most likely to press first. They live under
\code{system.file("extdata", "oq_small", package = "ACTA")}, and \code{\link{actaOQRun}} deletes
and rewrites \code{Outputs/} at the case root. The app therefore copies such a case into
\code{work_dir/Diagnostics/} and runs the copy.
}
\value{
No return value. Blocks until the app window is closed, like \code{\link[shiny]{runApp}}.
}
\seealso{\code{\link{run_acta}}, \code{\link{actaOQRun}}}
\examples{
\dontrun{
library(ACTA)
acta_app()                        # work in the current directory
acta_app("~/titrations/EXP123")   # or somewhere specific
}
}
