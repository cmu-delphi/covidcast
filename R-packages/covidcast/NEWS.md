# covidcast 0.4.2

Changes to prepare for release on CRAN. Released May 4, 2021.

## Major changes

- Removed extra variables in the metadata tables.

- Storing geographic data as compressed geojson files.

- Split `fips_to_name` into two functions: `state_fips_to_name` and
  `county_fips_to_name` to make the behavior more consistent.

# covidcast 0.4.1

Released January 19, 2021.

## Major changes

- Removed the dependency on the `usmap` package, which was removed from CRAN on
  January 10, 2021.

- `plot.covidcast_signal()` now supports mapping Puerto Rico in bubble plots.

- The `county_geo` and `state_geo` built-in datasets have been removed. These
  were used to support bubble plots, but the changes to
  `plot.covidcast_signal()` rendered them unnecessary.

## Minor changes

- Geographic lookup functions, such as `fips_to_name()` or `name_to_cbsa()`, are
  now much faster when given input vectors with many duplicate values.

# covidcast 0.4.0

Released January 13, 2021.

## Major changes

- The `plot.covidcast_signal()` method now supports making choropleth plots for
  metropolitan areas and hospital referral regions.

- A new `as.covidcast_signal()` generic function makes it easy to turn any
  data frame, such as data loaded from an external source, into a
  `covidcast_signal` object that can be plotted and wrangled using the functions
  in this package. See `vignette("external-data")` for examples.

- The new `latest_issue()` and `earliest_issue()` functions make it easy to
  filter data frames with multiple issues of each observation, obtaining only
  the latest or earliest issue of each.

- `covidcast_signal()`, `covidcast_signals()`, and `covidcast_meta()` now
  support signals with `time_type = "week"`. Select signals in the API are
  offered at weekly resolution. They also now support signals with `geo_types`
  of "nation" and "hhs", corresponding to national estimates and Department of
  Health & Human Services Regional Offices ("HHS regions").

- `covidcast_signal()` now batches requests, so that many days of data can be
  fetched in one API call. This dramatically improves the speed of fetching
  state-, MSA-, and HRR-level data, since many days of data can be fetched in
  one API call. County-level signals, such as cases and deaths, may still
  require one API call per day, since the API's row limit is only slightly
  larger than the number of counties in the United States.

- `covidcast_signal()` now fetches data from the API server in CSV format,
  rather than JSON, which requires less bandwidth and parsing.

- `covidcast_cor()` is dramatically faster than before.

# covidcast 0.3.1

Released October 31, 2020.

## Breaking changes

- The `covidcast_cor()` function now interprets its `dt_x` and `dt_y` arguments
  in the opposite way as before, i.e. what was once a positive lag is now a
  negative lag. See the documentation for details.

## Major changes

- The new `covidcast_signals()` function allows users to query multiple signals
  from the API at once, returning a list of data frames. The
  `aggregate_signals()` function can convert this list into a "wide" or "long"
  format data frame for further analysis, while `covidcast_wider()` and
  `covidcast_longer()` can convert from "long" to "wide" format and vice versa.

# covidcast 0.3.0

Released August 22, 2020.

## Major changes

- New `covidcast_cor()` function for calculating correlations between COVIDcast
  signals. See `vignette("correlation-utils")` for examples.

- New utility functions `name_to_fips()` and `name_to_cbsa()` to look up FIPS
  codes (for counties) or CBSA codes (for metropolitan statistical areas) based
  on the names of the counties or MSAs.

- New utility functions `fips_to_name()` and `cbsa_to_name()` to find the names
  corresponding to FIPS codes (for counties) or CBSA codes (for metropolitan
  statistical areas).

- New `summary.covidcast_meta()` S3 method for `summary()`, to print summaries
  of objects returned from `covidcast_meta()`.

# covidcast 0.2.0

Released July 26, 2020.

## Major changes

- **Name change:** The package is now called covidcast, not covidcastR. If
  already installed, users should remove covidcastR using `remove.packages()`
  and then install covidcast using the instructions in
  `vignette("covidcast")`.

- **Incompatible change:** The `covidcast_signal()` function now takes
  `start_day` and `end_day` arguments as strings in the form YYYY-MM-DD, rather
  than YYYYMMDD, for consistency with `as.Date()` and other common R usage. The
  arguments can also be passed as `Date` objects directly.

- New plotting and mapping features provide convenient visualizations of
  signals. See `vignette("plotting-signals")` for examples.

- New feature: The `covidcast_signal()` function now supports arguments `as_of`,
  `issues`, and `lag`, allowing users to request specific versions of data in
  the API. This allows users to track revisions of data and to see only data
  that was available as of a specific date. See `vignette("covidcast")` for
  examples.

## Minor changes

- The object returned from `covidcast_signal()` is now a data frame augmented
  with extra attributes indicating the geography type and signal metadata.
  Additional `data_source` and `signal` columns are provided, in anticipation of
  options to request more than one signal at once. Methods for `print` and
  `summary` are provided (as `print.covidcast_signal()` and
  `summary.covidcast_signal()`) to conveniently view these objects. They can
  still be used as ordinary data frames as well.


# covidcast 0.1.0

- First major release.
