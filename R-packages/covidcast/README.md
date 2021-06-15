# covidcast R package

Provides R access to the [COVIDcast
API](https://cmu-delphi.github.io/delphi-epidata/api/covidcast.html) published
by the [Delphi group](https://delphi.cmu.edu/) at [Carnegie Mellon
University](https://www.cmu.edu). This API provides daily access to a range of
COVID-related signals Delphi that builds and maintains, from sources like
symptom surveys and medical claims data, and also standard signals that we
simply mirror, like confirmed cases and deaths.

Consult the [COVIDcast API
documentation](https://cmu-delphi.github.io/delphi-epidata/api/covidcast.html)
for details on the data included in the API, licensing, and how to cite this
data in your work. **You should also consider subscribing to the [API mailing
list](https://lists.andrew.cmu.edu/mailman/listinfo/delphi-covidcast-api)** to
be notified of package updates, new data sources, corrections, and other
updates.

The [signals
documentation](https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html)
lists all the data sources and signals available through this API.

## Installing

This package is [available on
CRAN](https://cran.r-project.org/package=covidcast), so the easiest way to
install it is simply

```r
install.packages("covidcast")
```

Then use `vignette("covidcast")` to view the Getting Started guide.

The latest development version can be installed using the
[`devtools`](https://cran.r-project.org/package=devtools) package:

```r
devtools::install_github("cmu-delphi/covidcast", ref = "main",
                         subdir = "R-packages/covidcast")
```

## Documentation

The package documentation and examples are [available
online](https://cmu-delphi.github.io/covidcast/covidcastR/index.html).

## Developing

See `DEVELOP.md` for details on how to set up, test, and document this package.
