# covidcast R package

Provides R access to the [COVIDcast Epidata
API](https://cmu-delphi.github.io/delphi-epidata/api/covidcast.html) published
by the [Delphi group](https://delphi.cmu.edu/) at [Carnegie Mellon
University](https://www.cmu.edu). This API provides daily access to a range of
COVID-related signals Delphi that builds and maintains, from sources like
symptom surveys and medical claims data, and also standard signals that we
simply mirror, like confirmed cases and deaths.

Consult the [COVIDcast Epidata API
documentation](https://cmu-delphi.github.io/delphi-epidata/api/covidcast.html)
for details on the data included in the API, licensing, and how to cite this
data in your work. The [signals
documentation](https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html)
lists all the data sources and signals available through this API.

**To get started** using this package, view the Getting Started guide at
`vignette("covidcast")`.

## Get updates

**You should consider subscribing to the [API mailing
list](https://lists.andrew.cmu.edu/mailman/listinfo/delphi-covidcast-api)** to
be notified of package updates, new data sources, corrections, and other
updates.

## Usage terms

**Warning:** If you use data from the COVIDcast API to power a product,
dashboard, app, or other service, please download the data you need and store it
centrally rather than making API requests for every user. Our server resources
are limited and cannot support high-volume interactive use.

See also the [COVIDcast Terms of
Use](https://covidcast.cmu.edu/terms-of-use.html), noting that the data is a
research product and not warranted for a particular purpose.
