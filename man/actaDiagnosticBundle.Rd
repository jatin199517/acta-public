\name{actaDiagnosticBundle}
\alias{actaDiagnosticBundle}
\alias{actaDiagLogPath}
\alias{actaDiagAppend}
\alias{actaFirstError}
\title{Capture one pasteable diagnostic when a run fails}
\description{
The question these answer: when a run fails on someone else's machine, they should be able to send
\emph{one} artefact and it should be enough to troubleshoot from. \code{actaDiagLogPath()} decides
where the per-run log goes, \code{actaDiagAppend()} writes to it, \code{actaFirstError()} finds
the error that actually mattered, and \code{actaDiagnosticBundle()} assembles the lot into a
single text file.
}
\usage{
actaDiagLogPath(work_dir, action = "run")

actaDiagAppend(path, lines)

actaFirstError(lines)

actaDiagnosticBundle(work_dir, version_dir, run_log = NA_character_,
                     child_log = NA_character_, layout = NA_character_,
                     res = NULL, tsv = NA_character_, tlmgr_timeout = 10)
}
\arguments{
  \item{work_dir}{the operator's working folder. The log lands in
    \code{work_dir/Diagnostics_log/}, falling back to \code{tempdir()} when that is not writable
    -- a sync client holding the folder must never abort a run.}
  \item{action}{used in the filename, so a diagnostic run and an analysis run do not share a file.}
  \item{path}{a log path as returned by \code{actaDiagLogPath()}.}
  \item{lines}{character vector to append, or the text to search.}
  \item{version_dir}{the code folder, recorded for provenance.}
  \item{run_log}{path to the run-log \code{.tsv}, included in the bundle if present.}
  \item{child_log}{the analysis child process's captured output -- the single most useful input.}
  \item{layout}{path to the instructions workbook.}
  \item{res}{the \code{\link{run_acta}} result, if the run got far enough to produce one.}
  \item{tsv}{path to a \code{validated_packages.tsv} to compare against.}
  \item{tlmgr_timeout}{seconds to allow the TeX Live status collector.}
}
\details{
Filenames are per-run and timestamped, and every write is an append. Files were once deleted under
a live run and produced seven spurious failures, so nothing here rewrites a shared file.

There are three write points, because both mechanisms that came before wrote their report only
after the patient had recovered: a header row at the start (the only moment guaranteed to exist --
a \code{"started"} row with no terminal row \emph{is} the crash signal), the child's output teed
line by line as the poller reads it, and a closing marker at the end. A missing marker means
"incomplete", which is information rather than an absence.

\code{actaDiagAppend()} opens an explicit UTF-8 connection and never uses \code{sink()}, and it
swallows write failures on purpose: a diagnostic that aborts the thing it is diagnosing is worse
than no diagnostic.

\code{actaFirstError()} returns the \emph{first} error and the four lines after it, not the last.
One missing PNG has cascaded into four downstream failures where only the first line explained
anything, so a tail-only report actively misleads.

In \code{actaDiagnosticBundle()} every collector is individually wrapped and its failure recorded
in the bundle, because a diagnostic that dies while diagnosing hides the original error. Two are
known hazards and are treated as such: the TeX Live status collector shells out to a Perl script
and is time-boxed through \pkg{processx} rather than merely wrapped, since a hang is not an error;
and \code{\link{actaDependencyChecks}} runs last because it loads packages and a broken install
can take R down with it. The text is scrubbed of absolute paths, login names and e-mail addresses
before it is written, and the header says so.
}
\value{
\code{actaDiagLogPath()} returns the resolved path, or \code{NA_character_} if nowhere was
writable. The path is always returned so the caller can say it out loud.

\code{actaDiagAppend()} returns \code{TRUE} or \code{FALSE} invisibly.

\code{actaFirstError()} returns a single string, or \code{""} when nothing in the text looks like
an error -- rather than guessing.

\code{actaDiagnosticBundle()} returns a list with \code{ok}, \code{path} and \code{lines}.
}
\examples{
p <- actaDiagLogPath(tempdir(), action = "example")
actaDiagAppend(p, c("started", "Error in foo(): bar", "downstream noise"))
actaFirstError(readLines(p))
unlink(dirname(p), recursive = TRUE)
}
