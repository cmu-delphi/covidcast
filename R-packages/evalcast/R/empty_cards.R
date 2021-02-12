
empty_actual <- function(){
  out <- tibble(geo_value = character(0), forecast_date = lubridate::ymd(),
                actual=double(0), start = lubridate::ymd(), 
                end = lubridate::ymd()) %>% group_by(.data$geo_value)
  attr(out, "as_of") <- Sys.Date()
  out
}


#' @importFrom rlang :=
empty_score_card <- function(pcards, err_measures){
  # Creates a score card with nothing in it in case there's no available data
  # to evaluate some particular forecast task. Avoids errors later on.
  out <- pcards[0,]
  out$actual <- double(0)
  for(iter in names(err_measures)){
    out <- bind_cols(out, tibble(!!iter := double(0)))
  }
}
