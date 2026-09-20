\name{actaScanExports}
\alias{actaScanExports}
\alias{actaCopyDestReachable}
\title{Will the dashboard see these exports?}
\description{
Count the titration exports the dashboard generator would index under a scan folder, and check
that a chosen copy destination is one of the places it looks.
}
\usage{
actaScanExports(dir, dashboard_html = NA_character_)

actaCopyDestReachable(dest, scan)
}
\arguments{
  \item{dir}{the scan folder -- the parent whose immediate subfolders are the runs.}
  \item{dashboard_html}{an existing dashboard, so the count can say how many exports are newer
    than it. \code{NA} to skip that.}
  \item{dest}{the folder a run is about to copy its output into.}
  \item{scan}{the scan folder the dashboard indexes.}
}
\details{
Both functions exist to keep the app's pre-flight and the generator from disagreeing, and both
have been wrong in that exact way before.

\code{actaScanExports()} matches the generator's rule: each \emph{immediate} subfolder of
\code{dir} is one run, and each run is searched at its root and in its \code{Outputs/} subfolder,
nothing deeper. It once passed \code{recursive = TRUE}, and when the diagnostic runner started
collecting into \code{Outputs/} it found three exports and reported a pass while the generator
found none and wrote an empty dashboard. A green pre-flight followed by an empty artefact is worse
than either half being wrong alone.

Paths matching \code{"archive"} case-insensitively are excluded, which is the same rule the
generator applies and the reason \code{\link{actaArchiveThenCopy}} names its folder
\code{Archived/}.

Neither function filters sync-conflict copies. That filter existed, kept dropping genuine runs,
and was removed deliberately -- see the note in the sources before putting it back.
}
\value{
\code{actaScanExports()} returns a list with \code{recs} (status records; see \link{ACTA-package})
and \code{files} (the export paths found). Duplicate ELN IDs are a \code{"warn"}: the newest wins.

\code{actaCopyDestReachable()} returns a single status record. A destination the generator would
not index is a \code{"warn"}, not a failure -- the run still succeeds, the dashboard just will not
show it.
}
\seealso{\code{\link{actaArchiveThenCopy}}}
\examples{
scan <- tempfile("acta_scan"); dir.create(file.path(scan, "run1"), recursive = TRUE)
file.create(file.path(scan, "run1", "EXP00000001_TitrationExport.xlsx"))
length(actaScanExports(scan)$files)
actaCopyDestReachable(file.path(scan, "run1"), scan)$status   # pass
actaCopyDestReachable(tempdir(), scan)$status                 # warn
unlink(scan, recursive = TRUE)
}
