% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_width.R
\name{compute_width}
\alias{compute_width}
\title{Summarize forecast interval widths}
\usage{
compute_width(
  predictions_cards,
  grp_vars = c("forecaster", "forecast_date", "ahead"),
  avg_vars = c("geo_value"),
  levels = c(0.5, 0.7, 0.9)
)
}
\arguments{
\item{predictions_cards}{predictions cards (or a data frame with columns
\code{quantile}, \code{value} and any grouping vars and averaging vars)}

\item{grp_vars}{variables over which to compare widths}

\item{avg_vars}{variables over which we compute quantiles.}

\item{levels}{Quantile levels to summarize the interval widths. For example,
\code{levels = c(0.5, 0.7, 0.9)}, the default, computes the
median, 70\% and 90\% quantiles of interval widths within each combination
of \code{grp_vars} over the \code{avg_vars}}
}
\value{
a summarized data frame (by grp_vars)
}
\description{
Summarize forecast interval widths
}
