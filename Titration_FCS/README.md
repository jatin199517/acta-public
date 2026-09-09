# Titration_FCS

Put this run's raw `.fcs` files here, one subfolder per antibody group, named to match the
`Dirname` column of the instructions workbook:

    Titration_FCS/<Dirname>/*.fcs

The folder is tracked but its contents are NOT: `.gitignore` excludes everything in here except
this file. Flow data is the run's own material and does not belong in the repository -- the
sanitised diagnostic cases under `Diagnostics/OQ_Test*/Titration_FCS/` are the only flow data that
ships, and they exist so a fresh install can be verified without any real data.

An EMPTY folder is the expected state of a fresh clone. The pre-flight will say so rather than
guessing, and the app's Files check reports it as the one thing still missing before a run.
