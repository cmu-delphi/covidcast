# covidcast 0.2.0

## Major changes

- New plotting and mapping features provide convenient visualizations of
  signals. See `vignette("plotting-signals")` for examples.

- **Name change:** The package is now called covidcast, not covidcastR. Users
  should remove covidcastR using `remove.packages()` and then install covidcast
  using the instructions in `vignette("covidcastR")`.

- **Incompatible change:** The `covidcast_signal()` function now takes
  `start_day` and `end_day` arguments as strings in the form YYYY-MM-DD, rather
  than YYYYMMDD, for consistency with `as.Date()` and other common R usage.

## Minor changes

- The object returned from `covidcast_signal()` is now a data frame augmented
  with extra attributes indicating the data source, signal, and geography type.
  Methods for `print` and `summary` are provided (as `print.covidcast_signal()`
  and `summary.covidcast_signal()`) to conveniently view these objects. They can
  still be used as ordinary data frames as well.


# covidcast 0.1.0

- First major release.
