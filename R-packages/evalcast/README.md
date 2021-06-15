# evalcast R package

This package provides functionality for accurately evaluating forecaster
performance: crucially, evalcast leverages the [COVIDcast R package's](https://cmu-delphi.github.io/covidcast/covidcastR/index.html) "as of"
capability, which allows one to get the data that would have been known as
of a particular date in the past. This is important for honest evaluation of
COVID-19 forecasters because data sources often perform "backfill" in which
previous estimates about the past are updated. Without properly accounting
for backfill, traditional backtesting can lead to overly optimistic
evaluations of one's forecaster. Furthermore, naively training on historical
data that has already been backfilled may lead a trained model to rely too
heavily on the most recent data that has yet to settle. Such forecasters may
end up performing far worse in prospective evaluation than in backtesting.


## Installing

This package is not on CRAN yet, so it can be installed using the
[`remotes`](https://cran.r-project.org/package=remotes) package:

```r
remotes::install_github("cmu-delphi/covidcast", ref = "main",
                         subdir = "R-packages/evalcast")
```

## Documentation

The package documentation and examples are [available
online](https://cmu-delphi.github.io/covidcast/evalcastR/index.html).

## Getting started

To get started using this package, view the Getting Started guide at
`vignette("intro-evalcast")`.

It may also help to try using the `baseline_forecaster()`
