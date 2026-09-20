\name{actaCompensationRecords}
\alias{actaCompensationRecords}
\alias{actaCompCsvCandidates}
\alias{actaStageCompCsv}
\title{Compensation pre-flight and override}
\description{
Check, before a run starts, that the compensation the workbook asks for can actually be applied;
find the spillover CSV the pipeline would pick up; and temporarily put a different one in its
place.
}
\usage{
actaCompensationRecords(info, wd, fcs_root = NA_character_,
                        override = NA_character_, antibody = character(0))

actaCompCsvCandidates(dir)

actaStageCompCsv(wd, override)
}
\arguments{
  \item{info}{the \code{Info} sheet as a data frame. Its \code{compensation} cell selects the
    mode: blank (identity, no compensation), \code{"self"} (the acquisition-time matrix in each
    FCS file) or \code{"csv"}.}
  \item{wd}{the working folder, searched for the spillover CSV.}
  \item{fcs_root}{path to \code{Titration_FCS}, needed to check the \code{"self"} mode by reading
    the FCS TEXT segments.}
  \item{override}{a CSV chosen by the operator, used instead of whatever is in \code{wd}.
    \code{NA} for none.}
  \item{antibody}{the titration group names, used to locate representative FCS files.}
  \item{dir}{folder to search for the spillover CSV.}
}
\details{
\code{actaCompCsvCandidates()} is the lookup rule the pipeline itself uses, factored out so the
app cannot disagree with the analysis about which file would be loaded: the name \emph{contains}
\code{"CompMatrix"} case-insensitively and ends \code{.csv}, searched non-recursively. Not "any
CSV in the folder".

An unrecognised \code{compensation} value is fatal here by decision. The pipeline's own fallback
warns and uses an identity matrix, which means a typo produces a clean-looking run that is
silently uncompensated -- the worst of the available outcomes. Blocking up front forces the author
to say what they meant.

\code{actaStageCompCsv()} \emph{moves} the incumbent CSVs to a temporary folder rather than
deleting them, and puts them back when the returned function is called. Copying the override
alongside them would leave two candidates, and more than one matrix is fatal by design -- so the
override would break the very run it was meant to fix.
}
\value{
\code{actaCompensationRecords()} returns a list of status records (see \link{ACTA-package}); it
validates the \emph{effective} matrix, so a folder with no CSV does not read as a failure when an
override supplies one.

\code{actaCompCsvCandidates()} returns a character vector of matching paths, possibly empty.

\code{actaStageCompCsv()} returns a function of no arguments that restores the folder. It is a
no-op when \code{override} is absent or is already the file that would be picked up. Call it on
exit.
}
\examples{
d <- tempfile("acta_comp"); dir.create(d)
file.create(file.path(d, c("MyPanel_CompMatrix.csv", "notes.csv")))
basename(actaCompCsvCandidates(d))   # only the CompMatrix one
unlink(d, recursive = TRUE)
}
