# Global variables for passing R cmd check.

# Note: copied text from R cmd check and then
# ran the following:
# txt %>%
#   str_extract_all("global variable ‘[a-z_0-9]+’") %>%
#   unlist() %>%
#   paste(collapse = ",\n") %>%
#   str_replace_all("global variable ‘|’", "\"") %>%
#   unique() %>%
#   cat()
# or same idea but with \n after "global variable"
global_vars  <- c(
  "probs",
  "quantiles",
  "err",
  "signal",
  "geo_value",
  "value",
  "summed",
  "resid",
  "quantiles",
  "start",
  "end",
  "geo_value",
  "value",
  "upper",
  "lower",
  "alpha",
  "width",
  "time_value",
  "forecast_date",
  "actual",
  "forecast_distribution",
  "forecast_date",
  "dist_from_interval"
)
##utils::globalVariables(global_vars)
