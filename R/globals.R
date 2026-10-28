# Suppress R CMD check NOTEs of the form "no visible binding for global
# variable 'X'": these are data frame column names referenced via dplyr's
# non-standard evaluation.
utils::globalVariables(c("Frequency", "n", "Correlation"))
