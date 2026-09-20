\name{actaArchiveThenCopy}
\alias{actaArchiveThenCopy}
\title{Copy run output to a shared folder, archiving what is already there}
\description{
Copy a run's artefacts into a destination folder without ever overwriting in place: anything
already at the target is moved into \code{Archived/<timestamp>_<eln_id>/} first.
}
\usage{
actaArchiveThenCopy(src_paths, dest_dir, eln_id = "run", stamp = NULL,
                    dry_run = FALSE)
}
\arguments{
  \item{src_paths}{files to copy. Missing and empty paths are dropped.}
  \item{dest_dir}{where to copy them.}
  \item{eln_id}{the run's ELN (Benchling) ID, used to name the archive subfolder.}
  \item{stamp}{timestamp for the archive subfolder. \code{NULL} means now.}
  \item{dry_run}{report what would happen without moving or copying anything.}
}
\details{
A flat \code{Archived/} would only relocate the problem -- the second re-run would overwrite the
first archive -- hence the per-run discriminator in the folder name.

The name \code{Archived} is load-bearing, not cosmetic. Both dashboard generators skip any path
matching \code{"archive"} case-insensitively, so archived runs drop out of the dashboard
automatically; renaming this folder to anything without \code{"archive"} in it would resurface
every superseded run as a live dashboard row.

Sources that are already \emph{inside} the destination tree are left alone, and this is checked
against the whole tree rather than the immediate parent. Comparing only \code{dirname()} meant a
run's plots -- which live in \code{<root>/Plots} -- failed the test with a blank copy path and
were copied to \code{<root>/}, duplicating all eight PNGs at the top level beside the dashboard
and the export. Separately, because the archive step runs \emph{first} and moves a clashing target
aside, a source equal to the destination previously had the run's own export archived and the copy
then fail: the working folder ended up empty and the output sat in \code{Archived/}, where nobody
would look.

Files are copied with their modification times preserved. The generator orders runs by export
modification time, so without that every copied file is stamped "now" and copy order silently
outranks analysis order.
}
\value{
A list with \code{ok}, \code{why} (why not, when \code{ok} is \code{FALSE}), \code{archived} (what
was moved aside) and \code{copied} (what was written).
}
\seealso{\code{\link{actaScanExports}}, \code{\link{actaCopyDestReachable}}}
