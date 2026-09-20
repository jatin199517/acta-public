\name{actaPrevalidateRecords}
\alias{actaPrevalidateRecords}
\title{Pre-flight the instructions workbook and the FCS tree}
\description{
A single guard over the whole workbook -- \code{gating_template}, \code{Info} and
\code{Layout_Plate} -- plus the FCS tree the layout points at. Runs before a single FCS file is
read, and collects every problem it finds rather than stopping at the first.
}
\usage{
actaPrevalidateRecords(metadata, gating_template, info, groups, fcs_root,
                       marker_col = NA_character_, channel_col = NA_character_)
}
\arguments{
  \item{metadata}{the \code{Layout_Plate} sheet as a data frame, headers already stripped by
    \code{\link{actaStripHeaderAnnotation}}.}
  \item{gating_template}{the \code{gating_template} sheet as a data frame.}
  \item{info}{the \code{Info} sheet as a data frame.}
  \item{groups}{the \code{\link{actaTitrationGroups}} frame -- one row per titrated marker. A bare
    character vector of \code{Dirname}s is still accepted and means one titration per folder.}
  \item{fcs_root}{path to \code{Titration_FCS}.}
  \item{marker_col}{name of the marker column in \code{metadata}, or \code{NA} to resolve it.}
  \item{channel_col}{name of the channel column in \code{metadata}, or \code{NA} to resolve it.}
}
\details{
Every check here is cheap, and the failures it catches otherwise surface minutes later as
something obscure: a "missing sample: NA", an \code{-Inf} gate floor, an empty \code{flowSet}.
Fixing one layout mistake per run, each time paying the gating cost to find the next, is what this
exists to avoid.

Two entry points share one implementation. \code{actaPrevalidateRecords()} returns records, which
is what a UI renders; the internal console path stops with the same message it always has.
Records exist because a tick/cross panel has to show checks that passed and checks that were
\emph{skipped}, not only the failures a \code{stop()} can carry.
}
\value{
A list of status records (see \link{ACTA-package}). A \code{"fail"} record's \code{detail} is the
exact string the console path has always used, and \code{where} names the sheet or folder to go
and edit.
}
\seealso{\code{\link{actaTitrationGroups}}, \code{\link{actaCompensationRecords}},
  \code{\link{actaStripHeaderAnnotation}}}
