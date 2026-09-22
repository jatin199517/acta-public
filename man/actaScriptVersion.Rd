\name{actaScriptVersion}
\alias{actaScriptVersion}
\title{The version stamped on a run}
\description{
Resolve the version recorded against a run: the export's \code{ScriptVersion} column, the export
filename, the dashboard's default tabbing column, and \code{acta_script_version} in
\code{acta_version.tsv}.
}
\usage{
actaScriptVersion(code_dir = NULL)
}
\arguments{
  \item{code_dir}{the folder the pipeline's code was loaded from. Resolution walks up from here,
    so either an installed package's \code{pipeline/} or a source tree's \code{inst/pipeline}
    works without being told which.}
}
\details{
This was parsed from the script's own filename, \code{ACTA_Script_<n>_<n>.R}, so it changed only
when somebody created a new version folder. That claimed more than it delivered: in the six days
the \code{3_1} folder existed the analysis library took 27 commits and +726/-28 lines -- an export
that lost six sheets, a report that would not compile on Windows at all, two changes to the plate
map -- and every one of those runs was stamped \code{"3_1"}. A stamp asserting "same analysis" when
the analysis differs is a false negative, which is the wrong direction for a record whose job is
telling two runs apart.

It is now the package version, which moved 3.1.0 to 3.2.6 across exactly those changes. It errs the
other way, changing on a release that touched nothing analytical, and that false positive is the
safe direction: it over-reports a difference rather than hiding one.

Resolution describes \emph{the code that ran}, not whatever happens to be installed -- the two
differ whenever the pipeline sources \code{ACTA_Function*.R} out of \code{code_dir} while a
different version sits in the library. In order:
\enumerate{
  \item the \code{DESCRIPTION} governing \code{code_dir}, searched up to three levels up. This
    covers both layouts: \code{<lib>/ACTA/pipeline} finds \code{<lib>/ACTA/DESCRIPTION}, and
    \code{<ver>/inst/pipeline} finds \code{<ver>/DESCRIPTION}.
  \item the script filename, for the flat pre-3.0 layouts that have no \code{DESCRIPTION}. For
    those it is still the honest answer, and it is deliberately preferred over an installed package
    that did not supply the code.
  \item \code{utils::packageVersion("ACTA")}, last rather than first, for the same reason.
}
}
\value{
A single string, or \code{NA_character_} if no version can be resolved from any of the three.
}
\examples{
\dontrun{
actaScriptVersion(system.file("pipeline", package = "ACTA"))
}
}
