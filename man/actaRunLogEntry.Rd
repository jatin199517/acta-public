\name{actaRunLogEntry}
\alias{actaRunLogEntry}
\alias{actaRunLogAppend}
\title{The append-only ACTA run log}
\description{
Build one row describing a run, and append it to the tab-separated run log.
}
\usage{
actaRunLogEntry(action, outcome, version_dir, res = NULL,
                layout = NA_character_, report = NA, plots = NA,
                titer_edited = FALSE, outputs = character(0), detail = "")

actaRunLogAppend(log_path, entry)
}
\arguments{
  \item{action}{what was attempted, e.g. \code{"run"} or \code{"oq"}.}
  \item{outcome}{\code{"started"}, or a terminal outcome once one is known.}
  \item{version_dir}{the run folder, recorded as a short trailing label rather than an absolute
    path.}
  \item{res}{the \code{\link{run_acta}} result, or \code{NULL} when there is not one yet.}
  \item{layout}{path to the instructions workbook. Its basename and MD5 are recorded, which is
    what lets a result be tied to the exact workbook that produced it.}
  \item{report, plots}{whether each was requested.}
  \item{titer_edited}{whether an operator wrote a titer back into the workbook.}
  \item{outputs}{artefact paths; only their basenames are recorded.}
  \item{detail}{free text. When empty, the first analysis or report error from \code{res} is used
    instead.}
  \item{log_path}{the \code{.tsv} to append to. Created, with a header, if absent.}
  \item{entry}{a list as returned by \code{actaRunLogEntry()}.}
}
\details{
The log records failed and aborted runs too. A ledger holding only successes implies a clean
history that never happened.

Neither the OS account nor the machine name goes in. This used to record
\code{Sys.info()[["user"]]} and \code{Sys.info()[["nodename"]]} -- on a managed machine, the
operator's e-mail address and a hostname derived from it -- once per row, in a file sitting in the
working folder beside \code{Outputs/}. Zipping a run folder as a validation package carried it,
and the \code{Outputs/} identity scan structurally could not see it because it scans
\code{Outputs/}. \code{operator} now comes from the workbook, the same way the report's
attribution does, and \code{platform} replaces the hostname.

\code{actaRunLogAppend()} selects the row from \code{entry} \emph{by name}, so the two functions
have to agree on the column set. It also refuses to append to a log written before that rename: an
older file is moved aside intact rather than appended to, because putting \code{operator} under a
column headed \code{user} is wrong in the direction that matters -- a reader would believe the OS
account is in there.
}
\value{
\code{actaRunLogEntry()} returns a named list, one element per column.
\code{actaRunLogAppend()} returns \code{log_path} invisibly.
}
\examples{
log <- tempfile(fileext = ".tsv")
actaRunLogAppend(log, actaRunLogEntry("run", "started", tempdir()))
read.delim(log)[, c("action", "outcome", "platform")]
unlink(log)
}
