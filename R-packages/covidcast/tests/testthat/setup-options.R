# Many tests use hand-made reference data frames. In R < 4.0, columns like
# `geo_values` and `data_source`, containing strings, are interpreted as
# factors. To prevent us from having to set stringsAsFactors = FALSE in every
# test case, set the option while saving the default to restore in the teardown
# (in teardown-options.R).
.defaultStringsAsFactors <- options(stringsAsFactors = FALSE)
