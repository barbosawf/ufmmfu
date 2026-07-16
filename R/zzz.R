# Suppress R CMD check NOTEs of the form "no visible binding for global
# variable 'X'". These are not real global variables:
#
#   - 'ee' is the Python Earth Engine module object created at runtime by
#     rgee::ee_Initialize() (or an equivalent setup call). It lives in the
#     calling environment, not inside this package, so R CMD check cannot
#     see where it comes from.
#   - The rest (Year, Phase, start_date, end_date, Plot, Region, ImageIndex,
#     total_cells, NA_count, valid_ratio, x, y) are data frame column names
#     referenced via dplyr's non-standard evaluation (e.g. inside
#     dplyr::select(), dplyr::group_by(), dplyr::summarise()). They are
#     intentional and correct; R CMD check has no way to know they resolve
#     to columns at runtime rather than free variables.
utils::globalVariables(c(
  "ee",
  "Year", "Phase", "start_date", "end_date", "Plot", "Region",
  "ImageIndex", "total_cells", "NA_count", "valid_ratio", "x", "y"
))
