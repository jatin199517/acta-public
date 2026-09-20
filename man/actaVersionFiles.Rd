\name{actaVersionFiles}
\alias{actaVersionFiles}
\alias{actaVersionFile}
\title{Which ACTA files is this run about to use?}
\description{
Resolve the five files a run needs -- pipeline script, helper library, dashboard generator, report
\code{.Rmd} and the instructions workbook -- and report each as a status record.
\code{actaVersionFile()} pulls one resolved path back out of those records.
}
\usage{
actaVersionFiles(version_dir, code_dir = version_dir)

actaVersionFile(recs, id)
}
\arguments{
  \item{version_dir}{the working folder, searched for the instructions workbook.}
  \item{code_dir}{the folder holding the four shipped files. The same directory as
    \code{version_dir} in a flat checkout, hence the default, and different when ACTA runs from an
    installed package: the scripts live read-only in \code{inst/pipeline} while the workbook sits
    in the operator's own folder. Checking all five in one place is what made the app usable from
    a package at all.}
  \item{recs}{records returned by \code{actaVersionFiles()}.}
  \item{id}{which one to read: \code{"script"}, \code{"functions"}, \code{"dashboard"},
    \code{"report"} or \code{"layout"}.}
}
\details{
Exactly one match per pattern, which mirrors the rule the pipeline itself enforces. A leftover
\code{ACTA_Script_2_86_backup.R} beside the real one is fatal to the pipeline, so it has to be
fatal here too rather than the app silently driving whichever sorted first. Excel lock files
(\code{~$...}) are skipped. The resolved filename and its modification time are surfaced so an
operator can see \emph{which} version is about to run before pressing go.

The helper library is the one entry with a second legitimate source: running from an installed
package, \code{inst/pipeline} deliberately carries no \code{ACTA_Functions.R} and the helpers come
from the namespace instead. Reporting that as a missing file would make a correct install look
broken in the app's first check.
}
\value{
\code{actaVersionFiles()} returns a list of five status records (see \link{ACTA-package}), in the
order script, functions, dashboard, report, layout.

\code{actaVersionFile()} returns the resolved path for \code{id}, or \code{NA_character_} unless
exactly one record carries that \code{id} and it passed.
}
\examples{
recs <- actaVersionFiles(tempdir(), system.file("pipeline", package = "ACTA"))
basename(actaVersionFile(recs, "script"))
actaVersionFile(recs, "layout")   # NA: tempdir() holds no instructions workbook
}
