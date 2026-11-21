#!/bin/bash
# Double-click to launch the ACTA app. First time only:  chmod +x "ACTA App.command"
# Double-click starts in $HOME, not here, so cd to this file's folder.
cd "$(dirname "$0")" || exit 1
# Non-login shell: PATH has no ~/.zshrc additions, so resolve Rscript explicitly.
RSCRIPT=""
for c in /usr/local/bin/Rscript /opt/homebrew/bin/Rscript /Library/Frameworks/R.framework/Resources/bin/Rscript; do
  [ -x "$c" ] && RSCRIPT="$c" && break
done
[ -z "$RSCRIPT" ] && RSCRIPT="$(command -v Rscript)"
if [ -z "$RSCRIPT" ]; then echo "R not found. Install R from https://cran.r-project.org"; read -n 1 -s; exit 1; fi
# The app moved under inst/pipeline/ when 3.0 became a package, and this launcher still asked for
# ACTA_App.R at the root -- where there is none, so double-clicking did nothing but print an error.
# The fallback keeps it working in a flat checkout too.
APP="inst/pipeline/ACTA_App.R"
[ -f "$APP" ] || APP="ACTA_App.R"
if [ ! -f "$APP" ]; then echo "ACTA_App.R not found next to this launcher."; read -n 1 -s; exit 1; fi
# The app reads its working folder from here. Without it the app would take its own code directory,
# which in the package layout is inst/pipeline/ -- not where the workbook and Titration_FCS live.
export ACTA_WORK_DIR="$PWD"
echo "Launching ACTA from $(pwd)"
"$RSCRIPT" -e "shiny::runApp('$APP', launch.browser = TRUE)"
echo ""; echo "ACTA closed. Press any key."; read -n 1 -s
