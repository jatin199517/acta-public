\name{actaStripHeaderAnnotation}
\alias{actaStripHeaderAnnotation}
\title{Strip the type and slot annotations from layout headers}
\description{
Remove a trailing parenthetical annotation from \code{Layout_Plate} column names, once, at read
time.
}
\usage{
actaStripHeaderAnnotation(nms)
}
\arguments{
  \item{nms}{the header names as read from the sheet.}
}
\details{
The sheet's headers document themselves for whoever fills them in -- which FCS keyword a column is
matched against, and what type belongs in it:
\preformatted{Markername ($PnS)   Channel ($PnN)   Fix (L)   StainQty (N)}

Stripping them once here is what keeps that free. The rest of the pipeline goes on referring to
\code{StainQty}, \code{Fix} and \code{Markername}, so an annotation can be added, reworded or
removed without touching a line of code. The alternative -- teaching every read site about
suffixes -- would mean chasing dozens of literal column names, and missing one fails silently, as
a dropped well or a defaulted flag, rather than loudly.

Only a trailing \code{(...)} is removed, so a name that legitimately contains parentheses
mid-string survives.
}
\value{
The stripped names, trimmed. A collision after stripping is a hard error naming both original
columns: two columns reduced to the same name would make every downstream lookup ambiguous and
silently pick one.
}
\examples{
actaStripHeaderAnnotation(c("Markername ($PnS)", "Channel ($PnN)", "StainQty (N)"))

## A collision is fatal, not resolved:
try(actaStripHeaderAnnotation(c("StainQty (N)", "StainQty (uL)")))
}
