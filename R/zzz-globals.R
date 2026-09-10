## DATA-MASKING COLUMN NAMES, DECLARED.
##
## dplyr and ggplot2 evaluate bare column names inside the data, so `mutate(well = ...)` refers to a
## COLUMN called `well`, not to an R object. `R CMD check` cannot tell the two apart and reports each
## as "no visible binding for global variable". Declaring them here says "these are data columns,
## not forgotten objects" -- and, more usefully, it means any NEW name of this shape shows up in the
## check output instead of hiding in a list of known noise.
##
## This is NOT a way to silence a real missing symbol. Anything that is genuinely a function belongs
## in NAMESPACE as an importFrom, and every one of those has been declared there rather than parked
## in this list.
utils::globalVariables(c(
  ".data",
  "Column", "PlateID", "Row",          # plate geometry, from the Layout_Plate sheet
  "a", "compo", "ctrl", "dose",        # make_plate_layout_great_again(), per-well aesthetics
  "maximum", "minimum",                # biexp axis limits
  "row_num", "well"                    # plate map row index and well id
))
