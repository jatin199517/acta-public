## Publish/releases/LATEST_PUBLISHED.txt must agree with the tags.
##
## That file exists so a consumer never has to read .git -- .git lives inside a OneDrive-synced
## library and a session enumerating it saw HEAD, packed-refs and refs/tags reported absent on
## three consecutive reads while the repo was intact. The cost of that convenience is that the
## file is WRITTEN BY HAND and can therefore lie. This is what stops it lying.
##
## SKIPS in the public release tree, which ships tests/ but not Publish/, and whose own git init
## has no tags. Banner says "TESTS SKIPPED" because run_all.R greps for exactly that -- a gate
## whose skip banner says anything else is scored PASS while asserting nothing.
source(file.path(this.path::this.dir(), "acta_test_paths.R"))
ok <- TRUE
chk <- function(what, cond) { if (!isTRUE(cond)) ok <<- FALSE
                              cat(sprintf("  [%s] %s\n", if (isTRUE(cond)) "ok" else "FAIL", what)) }

## ACTA_REPO_ROOT is the PACKAGE root; the git repository is above it, and Publish/ is a sibling
## of the package folder. Walk up rather than assuming a depth -- the package folder was renamed
## from 3_1 to ACTA_package, and hard-coding either would break at the next rename.
root <- ACTA_REPO_ROOT
repeat {
  if (dir.exists(file.path(root, ".git"))) break
  up <- dirname(root); if (identical(up, root)) break
  root <- up
}
ptr  <- file.path(root, "Publish", "releases", "LATEST_PUBLISHED.txt")
gdir <- file.path(root, ".git")
if (!file.exists(ptr) || !dir.exists(gdir)) {
  cat("  [skip] needs Publish/releases/LATEST_PUBLISHED.txt and a .git with tags;",
      sprintf("pointer:%s git:%s\n", file.exists(ptr), dir.exists(gdir)))
  cat("\nRELEASE POINTER TESTS SKIPPED\n")
  quit(status = 0L)
}

kv <- function(k) {
  ln <- grep(sprintf("^%s[[:space:]]", k), readLines(ptr, warn = FALSE), value = TRUE)
  if (!length(ln)) NA_character_ else trimws(sub(sprintf("^%s[[:space:]]+", k), "", ln[1]))
}
packed <- if (file.exists(file.path(gdir, "packed-refs")))
            readLines(file.path(gdir, "packed-refs"), warn = FALSE) else character(0)
refSha <- function(ref) {
  f <- file.path(gdir, ref)
  if (file.exists(f)) return(trimws(readLines(f, warn = FALSE)[1]))
  hit <- grep(paste0(" ", ref, "$"), packed, value = TRUE)
  if (length(hit)) sub(" .*", "", hit[1]) else NA_character_
}
tags <- unique(sub("\\^\\{\\}$", "", c(list.files(file.path(gdir, "refs", "tags")),
              sub(".*refs/tags/", "", grep("refs/tags/", packed, value = TRUE)))))
rel  <- grep("^v[0-9].*-public$", tags, value = TRUE)

cat("=== the pointer agrees with the tags ===\n")
chk("this repo has at least one v<version>-public tag to check against", length(rel) > 0L)
if (length(rel)) {
  newest <- rel[order(numeric_version(sub("-public$", "", sub("^v", "", rel))))][length(rel)]
  ## THE ASSERTION THAT MATTERS. A pointer left behind at the previous release still parses, still
  ## names a real tag, and is simply wrong -- so compare against the HIGHEST tag, not any tag.
  chk(sprintf("`version` is the newest release (%s says %s, tags say %s)",
              basename(ptr), kv("version"), sub("-public$", "", sub("^v", "", newest))),
      identical(kv("version"), sub("-public$", "", sub("^v", "", newest))))
  chk(sprintf("`dev_tag` names that tag (%s)", kv("dev_tag")), identical(kv("dev_tag"), newest))
  chk(sprintf("`dev_sha` is the commit it points at (%s)", substr(kv("dev_sha"), 1, 8)),
      identical(kv("dev_sha"), refSha(file.path("refs", "tags", newest))))
  chk("`tag` is the public form of `version`", identical(kv("tag"), paste0("v", kv("version"))))
}
cat(if (ok) "\nALL RELEASE POINTER TESTS PASS\n" else "\nFAILURES ABOVE\n")
quit(status = if (ok) 0 else 1)
