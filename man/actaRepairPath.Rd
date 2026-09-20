\name{actaRepairPath}
\alias{actaRepairPath}
\title{Put the standard system directories back on PATH}
\description{
Prepend the standard system directories to \code{PATH} when they are genuinely missing, so the
external tools a run shells out to can be found.
}
\usage{
actaRepairPath(quiet = FALSE)
}
\arguments{
  \item{quiet}{suppress the message naming what was added and how to fix it permanently.}
}
\details{
Reporting this was not enough, which is why it repairs rather than only diagnoses. A colleague's
\code{PATH} read \code{/Users/<user>/.local/bin:/usr/local/bin:/user/bin:/bin}, where
\code{/user/bin} is a one-character typo for \code{/usr/bin}. So \code{perl} at
\code{/usr/bin/perl} was invisible, \code{tlmgr} is a Perl script, and the PDF could not be built.
They were given a message naming the typo exactly, and hit the same fault again the next day.
\code{Setup.R} repairs it, but only inside its own process; the app and the pipeline run in
separate R sessions, each inheriting the same broken \code{PATH} and each only reporting it.

Deliberately conservative: it only adds directories that are standard, absent from \code{PATH},
and actually exist. It never removes or reorders anything, so it cannot shadow a deliberately
chosen toolchain.

The two classes go on different ends. \code{/usr/bin}, \code{/bin}, \code{/usr/sbin} and
\code{/sbin} are root-owned and are \emph{prepended}, which is what repairs the fault above.
\code{/usr/local/bin} and \code{/opt/homebrew/bin} are routinely user- or admin-group-writable, so
they are \emph{appended}: still found when nothing else provides them, and never able to shadow a
system tool for \code{pandoc}, \code{kpsewhich}, \code{pdflatex} or the \code{git} that stamps the
provenance record.

No-op on Windows.
}
\value{
A character vector of the directories added, empty if nothing was missing. Returned rather than
hidden so callers can say so out loud: silently working around a broken environment would conceal
a machine-level fault the operator still needs to fix permanently.
}
\seealso{\code{\link{actaDependencyChecks}}}
\examples{
\dontrun{
## Modifies PATH for the current session.
actaRepairPath()
}
}
