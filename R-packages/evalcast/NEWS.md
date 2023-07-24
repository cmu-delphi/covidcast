# evalcast 0.3.4

- Change `get_forecaster_predictions_alt` to read forecaster input files using
  `data.table::fread`. This replaces a several-years-old version of
  `arrow::open_dataset`. `data.table::fread` is faster, doesn't require a
  separate download step, and has more flexibility in column specification.

# evalcast 0.3.3

- Fix `get_covidhub_forecast_dates`, likely broken by GitHub website format
  change, by refactoring it to use GitHub API. If requesting many dates, this
  may require authentication with a
  [GitHub API key](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens)

# evalcast 0.3.2

- Fix DESCRIPTION remotes so `remotes` and `devtools` work
- Fix `plot_calibration` by removing badly specified titles in the `lab` argument
- Pin `bettermc` to the GitHub version 1.1.2 to avoid a CRAN error

# evalcast 0.3.1

- Added a download_signal caching folder option

# evalcast 0.3.0

- Added pkgdown
- Lives on `main`

# evalcast 0.2.0

- Major overhaul of functionality
- Forecasters and evaluation scripts now operate on and result in "long" data
frames
- Increased plotting flexibility
- Various utility functions added for increased evaluation/visualization
flexibility
- Integration with [zoltr](https://docs.zoltardata.com/zoltr/) for some forecast
retrieval as well as [covidHubUtils](https://github.com/reichlab/covidHubUtils)


# evalcast 0.1.0

- First major release.
