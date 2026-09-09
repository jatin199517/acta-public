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
echo "Launching ACTA from $(pwd)"
"$RSCRIPT" -e 'shiny::runApp("ACTA_App.R", launch.browser = TRUE)'
echo ""; echo "ACTA closed. Press any key."; read -n 1 -s
