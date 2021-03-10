#' Simple quantile autoregressive forecaster based on `quantgen`
#'
#' A simple quantile autoregressive forecaster based on `quantgen`, to be used
#' with `evalcast`, via [evalcast::get_predictions()]. See the [quantgen
#' forecast
#' vignette](https://cmu-delphi.github.io/covidcast/modeltoolsR/articles/quantgen-forecast.html)
#' for examples.
#' 
#' @param df Data frame of signal values to use for forecasting, of the format
#'   that is returned by [covidcast::covidcast_signals()].
#' @param forecast_date Date object or string of the form "YYYY-MM-DD",
#'   indicating the date on which forecasts will be made. For example, if
#'   `forecast_date = "2020-05-11"`, `incidence_period = "day"`, and `ahead =
#'   3`, then, forecasts would be made for "2020-05-14". 
#' @param signals Tibble with columns `data_source` and `signal` that specifies
#'   which variables are being fetched from the COVIDcast API, and populated in
#'   `df`. Each row of `signals` represents a separate signal, and first row is
#'   taken to be the response. An optional column `start_day` can also be
#'   included. This can be a Date object or string in the form "YYYY-MM-DD",
#'   indicating the earliest date of data needed from that data source.
#'   Importantly, `start_day` can also be a function (represented as a list
#'   column) that takes a forecast date and returns a start date for model
#'   training (again, Date object or string in the form "YYYY-MM-DD"). The
#'   latter is useful when the start date should be computed dynamically from
#'   the forecast date (e.g., when the forecaster only trains on the most recent
#'   4 weeks of data). 
#' @param incidence_period One of "day or "epiweek", indicating the period over
#'   which forecasts are being made. Default is "day".
#' @param ahead Vector of ahead values, indicating how many days/epiweeks ahead
#'   to forecast. If `incidence_period = "day"`, then `ahead = 1` means the day
#'   after forecast date. If `incidence_period = "epiweek"` and the forecast
#'   date falls on a Sunday or Monday, then `ahead = 1` means the epiweek that 
#'   includes the forecast date; if `forecast_date` falls on a Tuesday through
#'   Saturday, then it means the following epiweek.
#' @param n Size of the local training window (in days/weeks, depending on
#'   `incidence_period`) to use. For example, if `n = 14`, and `incidence_period
#'   = "day"`, then to make a 1-day-ahead forecast on December 15, we train on
#'   data from December 1 to December 14.
#' @param lags Vector of lag values to use as features in the autoregressive
#'   model. For example, when `incidence_period = "day"`, setting `lags = c(0,
#'   7, 14)`means we use the current value of each signal (defined by a row of
#'   the `signals` tibble), as well as the values 7 and 14 days ago, as the 
#'   features. Recall that the response is defined by the first row of the
#'   `signals` tibble. Note that `lags` can also be a list of vectors of lag
#'   values, this list having the same length as the number of rows of
#'   `signals`, in order to apply a different set of shifts to each signal. 
#'   Default is 0, which means no additional lags (only current values) for each
#'   signal.
#' @param tau Vector of quantile levels for the probabilistic forecast. If not
#'   specified, defaults to the levels required by the COVID Forecast Hub.
#' @param transform,inv_trans Transformation and inverse transformations to use
#'   for the response/features. The former `transform` can be a function or a
#'   list of functions, this list having the same length as the number of rows
#'   in the `signals` tibble, in order to apply the same transformation or a
#'   different transformation to each signal. These transformations will be
#'   applied before fitting the quantile model. The latter argument `inv_trans`
#'   specifies the inverse transformation to use on the response variable
#'   (inverse of `transform` if this is a function, or of `transform[[1]]` if
#'   `transform` is a list), which will be applied post prediction from the
#'   quantile model. Several convenience functions for transformations exist as
#'   part of the `quantgen` package. Default is `NULL` for both `transform` and
#'   `inv_trans`, which means no transformations are applied.
#' @param featurize Function to construct custom features before the quantile
#'   model is fit. As input, this function must take a data frame with columns
#'   `geo_value`, `time_value`, then the transformed, lagged signal values. This
#'   function must return a data frame with columns `geo_value`, `time_value`,
#'   then any custom features. The rows of the returned data frame *must not* be
#'   reordered.
#' @param noncross Should noncrossing constraints be applied? These force the
#'   predicted quantiles to be properly ordered across all quantile levels being
#'   considered. The default is `FALSE`. If `TRUE`, then noncrossing constraints
#'   are applied to the estimated quantiles at all points specified by the next
#'   argument.
#' @param noncross_points One of "all", "test", "train" indicating which points
#'   to use for the noncrossing constraints: the default "all" means to use both
#'   training and testing sets combined, while "test" or "train" means to use
#'   just one set, training or testing, respectively.
#' @param cv_type One of "forward" or "random", indicating the type of
#'   cross-validation to perform. If "random", then `nfolds` folds are chosen by
#'   dividing training data points randomly (the default being `nfolds = 5`). If
#'   "forward", the default, then we instead use a "forward-validation" approach
#'   that better reflects the way predictions are made in the current time
#'   series forecasting context. Roughly, this works as follows: the data points
#'   from the first `n - nfolds` time values are used for model training, and
#'   then predictions are made at the earliest possible forecast date after this 
#'   training period. We march forward one time point at a time and repeat. In
#'   either case ("random" or "forward"), the loss function used for computing
#'   validation error is quantile regression loss (read the documentation for
#'   `quantgen::cv_quantile_lasso()` for more details); and the final quantile
#'   model is refit on the full training set using the validation-optimal tuning
#'   parameter.
#' @param verbose Should progress be printed out to the console? Default is
#'   `FALSE`. 
#' @param ... Additional arguments. Any parameter accepted by
#'   `quantgen::cv_quantile_lasso()` (for model training) or by
#'   `quantgen:::predict.cv_quantile_genlasso()` (for model prediction) can be
#'   passed here. For example, `nfolds`, for specifying the number of folds used
#'   in cross-validation, or `lambda`, for specifying the tuning parameter
#'   values over which to perform cross-validation (the default allows
#'   `quantgen::cv_quantile_lasso()` to set the lambda sequence itself). Note
#'   that fixing a single tuning parameter value (such as `lambda = 0`)
#'   effectively disables cross-validation and fits a quantile model at the
#'   given tuning parameter value (here unregularized quantile autoregression).
#' 
#' @return Data frame with columns `ahead`, `geo_value`, `quantile`, and
#'   `value`. The `quantile` column gives the probabilities associated with
#'   quantile forecasts for that location and ahead. 
#' 
#' @importFrom dplyr filter select pull summarize between bind_cols
#' @importFrom tidyr pivot_longer
#' @export
quantgen_forecaster = function(df, forecast_date, signals, incidence_period,
                               ahead, geo_type,
                               n = 4 * ifelse(incidence_period == "day", 7, 1),
                               lags = 0, tau = modeltools::covidhub_probs,
                               transform = NULL, inv_trans = NULL,
                               featurize = NULL, noncross = FALSE, 
                               noncross_points = c("all", "test", "train"),
                               cv_type = c("forward", "random"),
                               verbose = FALSE, ...) {   
  # Check lags vector or list
  if (any(unlist(lags) < 0)) stop("All lags must be nonnegative.")
  if (!is.list(lags)) lags = rep(list(lags), nrow(signals))
  else if (length(lags) != nrow(signals)) {
    stop(paste("If `lags` is a list, it must have length equal to the number",
               "of signals."))
  }
    
  # Check transform function or list, and apply them if we need to
  if (!is.null(transform)) {
    if (!is.list(transform)) {
      transform = rep(list(transform), nrow(signals))
    }
    else if (length(transform) != nrow(signals)) {
      stop(paste("If `transform` is a list, it must have length equal to the",
                 "number of signals."))
    }
    if (is.null(inv_trans)) {
      stop("If `transform` is specified, then `inv_trans` must be as well.")
    }
    for (i in 1:length(df)) {
      df[[i]] = df[[i]] %>% mutate(value = transform[[i]](value))
    }
  }

  # Define dt by flipping the sign of lags, include dt = +ahead as a response 
  # shift, for each ahead value, for convenience later 
  dt = lapply(lags, "-")
  dt[[1]] = c(dt[[1]], ahead)
    
  # Append shifts, and aggregate into wide format
  df_wide = covidcast::aggregate_signals(df, dt = dt, format = "wide")
  
  # Separate out into feature data frame, featurize if we need to
  df_features = df_wide %>%
    select(geo_value, time_value, tidyselect::matches("^value(\\+0|-)"))
  feature_end_date = df_features %>%
    summarize(max(time_value)) %>% pull()
  if (!is.null(featurize)) df_features = featurize(df_features)
  
  # Identify params for quantgen training and prediction functions
  params = list(...)
  params$tau = tau
  params$noncross = noncross

  # perform CV if (i) lambda not provided, or (ii) more than one lambda
  # value provided
  cv = is.null(params$lambda) || length(params$lambda) > 1
  if (cv) {
    train_fun = quantgen::cv_quantile_lasso
    predict_fun = quantgen:::predict.cv_quantile_genlasso
  }
  else {
    train_fun = quantgen::quantile_lasso
    predict_fun = quantgen:::predict.quantile_genlasso
  }
  
  train_names = names(as.list(args(train_fun)))
  predict_names = names(as.list(args(predict_fun)))
  train_params = params[names(params) %in% train_names]
  predict_params = params[names(params) %in% predict_names]

  # Check noncross_points and cv_type
  if (noncross) noncross_points = match.arg(noncross_points)
  if (cv) cv_type = match.arg(cv_type)
  
  # Test objects that remain invariant over ahead values
  test_geo_value = df_features %>%
    filter(time_value == max(time_value)) %>%
    select(geo_value) %>% pull()
  newx = df_features %>%
    filter(time_value == max(time_value)) %>%
    select(-c(geo_value, time_value)) %>% as.matrix()
    
  # Loop over ahead values, fit model, make predictions 
  result = vector(mode = "list", length = length(ahead))
  for (i in 1:length(ahead)) {
    a = ahead[i]
    if (verbose) cat(sprintf("%s%i", ifelse(i == 1, "\nahead = ", ", "), a))
    
    # Training end date    
    response_end_date = df_wide %>%
      select(time_value, tidyselect::starts_with(sprintf("value+%i:", a))) %>%
      tidyr::drop_na() %>%
      summarize(max(time_value)) %>% pull()
    train_end_date = min(feature_end_date, response_end_date)

    # Training x and y
    x = df_features %>%
      filter(between(time_value,
                     train_end_date - n + 1,
                     train_end_date)) %>%
      select(-c(geo_value, time_value)) %>% as.matrix()
    y = df_wide %>%
      filter(between(time_value,
                     train_end_date - n + 1,
                     train_end_date)) %>%
      select(tidyselect::starts_with(sprintf("value+%i:", a))) %>% pull()
    
    # Define noncrossing constraints, if we need to
    if (noncross) {
      if (noncross_points == "all") x0 = rbind(x, newx)
      else if (noncross_points == "test") x0 = newx
      else if (noncross_points == "train") x0 = x
      x0 = na.omit(x0) # Just in case, since quantgen won't check this
      train_params$x0 = x0
    }
      
    # Define forward-validation folds, if we need to
    if (cv && cv_type == "forward") {
      # Training time values
      train_time_value = df_wide %>%
        filter(between(time_value,
                       train_end_date - n + 1,
                       train_end_date)) %>%
        select(time_value) %>% pull()

      # Training and test folds
      nfolds = ifelse(!is.null(params$nfolds), params$nfolds, 5)
      train_test_inds = list(train = vector(mode = "list", length = nfolds),
                             test = vector(mode = "list", length = nfolds))
      for (k in 1:nfolds) {
        train_test_inds$train[[k]] = which(
          between(train_time_value,
                  train_end_date - n + k,
                  train_end_date - nfolds + k - 1))
        train_test_inds$test[[k]] = which(
          train_time_value == train_end_date - nfolds + k) 
      }
      train_params$train_test_inds = train_test_inds
    }
    
    # Add training x and y to training params list, fit model
    train_params$x = x
    train_params$y = y; 
    train_obj = do.call(train_fun, train_params)

    # Add training object and newx to test params list, make predictions
    predict_params$object = train_obj
    predict_params$object$inv_trans = inv_trans # Let quantgen handle this
    predict_params$newx = newx
    predict_mat = do.call(predict_fun, predict_params)
    
    # Do some wrangling to get it into evalcast "long" format
    colnames(predict_mat) = tau
    predict_df = bind_cols(geo_value = test_geo_value,
                           predict_mat) %>%
      pivot_longer(cols = -geo_value,
                   names_to = "quantile",
                   values_to = "value") %>%
      mutate(ahead = a)

    # TODO: allow train_obj to be appended to forecaster's output. This would
    # be nice for post-hoc analysis of the fitted models, etc. Two options for
    # doing so: 1. append as a column to returned df; but this would be a big 
    # waste of memory since it'd get repeated for each forecast made from the
    # same fitted model, 2. include it as an attribute of the returned df, but  
    # unless we're super careful this would get squashed by downstream uses of 
    # rbind(), map(), etc. We could be careful here, but then we'd also have
    # to keep track of what evalcast does
    result[[i]] = predict_df
  }
  if (verbose) cat("\n")

  # Collapse predictions into one big data frame, and return
  return(do.call(rbind, result))
}
