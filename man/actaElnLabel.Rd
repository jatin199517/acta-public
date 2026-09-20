\name{actaElnLabel}
\alias{actaElnLabel}
\title{One label for a run's ELN identifiers}
\description{
Collapse the ELN (Benchling) IDs of a run or group into a single label, for a filename or for
display.
}
\usage{
actaElnLabel(x, for_file = FALSE)
}
\arguments{
  \item{x}{the ELN IDs, typically the \code{ELN_ID} column of the layout. \code{NA} and empty
    entries are dropped.}
  \item{for_file}{build a filename-safe label instead of a display one.}
}
\details{
\code{unique(ELN_ID)} used to be used directly, which assumed one entry per run. A layout that
legitimately draws its wells from two notebook entries made that a length-2 vector, and each
consumer then failed in its own way: the export died at the very end of the run inside
\code{openxlsx::saveWorkbook()} with "'length = 2' in coercion to logical(1)"; the run-level PNGs
were written from element one only, so the wells belonging to the other entry were silently
misattributed in the filename -- the worse half, since it looks fine; and three plot titles took
element one too.

\code{for_file = TRUE} joins with \code{"+"} rather than \code{", "} and strips anything outside
\code{[A-Za-z0-9._+-]}. A comma and a space survive every filesystem in use here but make the name
awkward to read and to quote in a shell.

A single ID -- the ordinary case, and every diagnostic case -- comes back unchanged either way.
}
\value{
A single string. With nothing usable in \code{x}: \code{"not recorded"}, or \code{"NA"} when
\code{for_file = TRUE}.
}
\examples{
actaElnLabel("EXP00000001")
actaElnLabel(c("EXP00000001", "EXP00000002"))
actaElnLabel(c("EXP00000001", "EXP00000002"), for_file = TRUE)
actaElnLabel(c(NA, ""))
}
