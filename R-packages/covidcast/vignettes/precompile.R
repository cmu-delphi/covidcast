# Vignettes are precompiled so CRAN does not have to make remote calls that may
# give occasional errors

library(knitr)
knit("correlation-utils.Rmd.orig", "correlation-utils.Rmd")
knit("covidcast.Rmd.orig", "covidcast.Rmd")
knit("external-data.Rmd.orig", "external-data.Rmd")
knit("multi-signals.Rmd.orig", "multi-signals.Rmd")
knit("plotting-signals.Rmd.orig", "plotting-signals.Rmd")
