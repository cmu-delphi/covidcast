library(covidcast)
library(ggplot2)
library(lubridate)
library(plotly)
library(dplyr)


#' Transforms a lubridate date format into string "YYYYMMDD" ready to be fed to the Delphi API
#' 
#' @return list with population per county/state/msa
#' TODO code is relying on github csv that maps state IDs to letters
get_population <- function(){
  population_mapping = list()
  
  census_url = "https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv"
  county_pop = read.csv(census_url) %>% 
    transmute (geo_value = 1000*as.numeric(STATE) + as.numeric(COUNTY),
               population = POPESTIMATE2019)
  county_pop$geo_value <- sprintf("%05d", county_pop$geo_value)
  population_mapping$county = county_pop
  
  state_pop = read.csv(census_url) %>% 
    mutate(geo_value = as.numeric(STATE)) %>% 
    group_by(geo_value) %>% 
    summarise(population = sum(POPESTIMATE2019))
  state_crosswalk <- read.csv("https://raw.githubusercontent.com/cmu-delphi/covid-19/main/geographical_scope/StateToHHS.csv?token=AHQABX4BXLJ3S3SJFSOGPFC7EBAIA") %>% 
    select(State, FIPS) %>% distinct()
  state_pop <- state_pop %>% 
    inner_join(state_crosswalk, by = c("geo_value" = "FIPS")) %>% 
    select(geo_value = State, population) %>% 
    mutate(geo_value = tolower(geo_value))
  population_mapping$state = state_pop
  
  msa_pop = read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/metro/totals/cbsa-est2019-alldata.csv") %>% 
    transmute(geo_value = as.character(CBSA),
              population = POPESTIMATE2019)
  population_mapping$msa = msa_pop
  
  
  return(population_mapping)
}

#' Transforms a lubridate date format into string "YYYYMMDD" ready to be fed to the Delphi API
#' 
#' @param date_var lubridate date
#' @return string "YYYYMMDD"
date_to_char <- function(date_var){
  as.character(date_var)
}

#' Transforms a character string "YYYYMMDD" in a lubridate date
#' 
#' @param char_var string with date in form "YYYYMMDD"
#' @return lubridate date format
char_to_date <- function(char_var){
  as.Date(char_var)
}

#' Get average responses variable (to compute correlations) from the API
#' over a fixed time widnow. 
#' Common response variables are 'number of cases' and 'number of deaths'
#' 
#' @param response string vector with two elements, first the source and second the signal namefor the API
#' @param date_var last day to get response variable 
#' @param window number of days to average response; API call will be from date_var - window to date_var
#' @param geo_type geographical granularity. "county", "state", or "msa"
#' @return list with average signal over (date_var - window + 1, date_var). Each element of the list refers to one geographical granularity
get_response <- function(response, date_var, window, geo_type){
  
  ## make sure date_var is date format
  if(!is.Date(date_var)) date_var <- char_to_date(date_var)
  
  ## for geo_type, get data from API for the source and signal of interest
  ## from date_var-window+1 until date_var and average signal per geo location
  ## same will be done for state and msa
  cases_mapping <- suppressWarnings(covidcast_signal(data_source = response[1], 
                                          signal = response[2],
                                          start_day = date_to_char(date_var - window + 1), 
                                          end_day = date_to_char(date_var), 
                                          geo_type = geo_type) )
  
  if(nrow(cases_mapping) == 0) stop("No available data for this response variable and these specifications. ")
  
  cases_mapping <- cases_mapping %>% 
    select(geo_value, time_value, value) %>% 
    group_by(geo_value) %>% 
    summarise(avgcases = mean(value))
  
  return(cases_mapping)
}

#' Get average sensors (to compute correlations) from the API
#' over a fixed time widnow. 
#' 
#' @param signals_to_plot string matrix with two columns, first the source and second the signal name for the API. each row is one signal
#' @param date_var last day to get response variable. combined with lag and window, will tell us when to get signal values
#' @param lag temporal spacing between signal and response to compute correlation. eg: correlation between deaths today and GHT two weeks ago. lag = 2 weeks
#' @param window number of days to average signal; API call will be from date_var - window - lag + 1 to date_var - lag
#' @param geo_type geographical granularity. "county", "state", or "msa"
#' @return data frame with columns geo_value, avgsignal, source, signal
get_sensors <- function(signals_to_plot, date_var, lag, window, geo_type){
  
  ## make sure date_var is date format
  if(!is.Date(date_var)) date_var <- char_to_date(date_var)
  
  df <- data.frame()

  no_data_indices <- NULL  
  ## for each signal that will be considered, create dataframe with average signal value
  for(i in 1:nrow(signals_to_plot)){
    aux <- suppressWarnings(covidcast_signal(data_source = signals_to_plot[i,1], 
                            signal = signals_to_plot[i,2],
                            start_day = date_to_char(date_var - window - lag + 1), 
                            end_day = date_to_char(date_var - lag),  
                            geo_type = geo_type))
    
    if(nrow(aux)==0){
      warning(paste("No data available for ", signals_to_plot[i,1],": ", signals_to_plot[i,2], sep = ""))
      no_data_indices <- c(no_data_indices, i)
    }
    
    if(nrow(aux)>0){
      aux <- aux %>% 
        select(geo_value, time_value, value) %>% 
        group_by(geo_value) %>% 
        summarise(avgsignal = mean(value))
      ## make sure we are saving source and signal names
      aux$source = signals_to_plot[i,1]
      aux$signal = signals_to_plot[i,2]
      
      ## concatenate in a large dataset with all signals
      df <- bind_rows(df, aux)
    }
  }
  
  if(nrow(df) == 0) stop("No available data for these signals and their specifications. ")

  if(length(no_data_indices)==1) warning(paste("Signal not available: \n", 
                                              paste(signals_to_plot[no_data_indices,], collapse = ": ")))
  
  if(length(no_data_indices)>1) warning(paste("Signals not available: \n", 
                                              paste(apply(signals_to_plot[no_data_indices,], 1, paste, collapse = ": "), collapse = " \n")))
  
  
  return(df)
}

#' Calculate rank correlations per population threshold for ONE SIGNAL. Re-implemented as a helper function for
#' ggplots.
#' Credits to Addison :)
#' 
#' @param tbbl A tibble with columns: geo_value, avgsignal, avgcases, population
#' @param min_obs Minimum number of observations
#' @param n_labels number of "number of counties that survived the threshold" to be presented in a plot - not currently used!!!
#' @return A tibble with columns: pop_threshold, rank_corr, num_obs
rank_cor <- function(tbbl, min_obs=100#, 
                     #n_labels = 7
                     ) {
  n = nrow(tbbl)
  tbbl = tbbl %>% arrange(population)
  if (n < min_obs) {
    pop_threshold = NA
    rank_corr = NA
    num_obs = NA
  } else {
    pop_threshold = tbbl$population[min_obs:n]
    rank_corr = rep(NA, n-min_obs+1)
    for (i in 1:(n-min_obs+1)) {
      rank_corr[i] = cor(tbbl$avgsignal[i:n], tbbl$avgcases[i:n],
                         method="spearman")
      num_obs = n:min_obs
    }
  }
  
  #labels = num_obs
  #labels[!(1:length(num_obs) %in% round(seq(1,length(num_obs),length.out = n_labels)))] = ""
  
  return (data.frame(
    pop_threshold = pop_threshold,
    rank_corr = rank_corr,
    num_obs = num_obs#, 
    #labels = labels
  ))
}

#' Plotting function for correlation between sensors and response (usually deaths or case counts)
#' 
#' @param sensors output of get_sensors() or dataframe with columns geo_value, avgsignal, source, signal
#' @param response output of get_response()
#' @param population output of get_population()
#' @param geo_type geographical granularity. "county", "state", or "msa"
#' @param min_obs Minimum number of observations
#' @return plot
plot_corr <- function(sensors, response, population, geo_type, freescales = FALSE){
  ## join sensors, response, and population size
  df <- sensors %>% inner_join(response, by = c("geo_value")) %>% inner_join(population[[geo_type]], by = "geo_value")
  
  if(nrow(df) == 0) stop("No geographical intersection between response and signals. ")
  
  if(geo_type == "state"){min_obs = 25} else{min_obs = 100}
  if(nrow(df) < min_obs) stop("Not enough observed geographical locations. ")
  
  ## compute correlations considering population threshold per signal
  df_plot <- plyr::ddply(df, c("source", "signal"), rank_cor, min_obs = min_obs) %>% mutate(title = paste(source, ", ", signal, sep = ""))
  
  p1 <- ggplot(df_plot, aes(x=pop_threshold, y=rank_corr)) + 
    geom_line() + 
    #  scale_x_log10(sec.axis = sec_axis(~., breaks = df_oneplot$pop_threshold, labels = df_oneplot$labels)) + 
    scale_x_log10() + 
    theme_bw(base_size = 14) + 
    xlab('Population threshold') +
    ylab('Rank correlation')
  
  ## plotting parameter. if TRUE, scales are free, that is, axis have different ranges for different plots
  ## if FALSE, they are all fixed to be the same. y axis is fixed to be between -1 and 1, with horizontal red line on 0
  if(freescales){
    p1 <- p1 + facet_wrap(~title, ncol = 3, scales = "free")
  } else {
    p1 <- p1 + 
      ylim(-1,1) + 
      geom_hline(yintercept=0, col = "red") + 
      facet_wrap(~title, ncol = 3)
  }

  return(p1)
}

#' Wrapper to plot correlations
#' 
#' @param signals_to_plot string matrix with two columns, first the source and second the signal name for the API. each row is one signal
#' @param response string vector with two elements, first the source and second the signal namefor the API
#' @param date_var last day to get response variable. combined with lag and window, will tell us when to get signal values
#' @param lag temporal spacing between signal and response to compute correlation. eg: correlation between deaths today and GHT two weeks ago. lag = 2 weeks
#' @param window number of days to average signal; API call will be from date_var - window - lag + 1 to date_var - lag
#' @param geo_type geographical granularity. "county", "state", or "msa"
#' @return plot
signal_correlations <- function(signals_to_plot, response, date_var, lag, window, geo_type, freescales = FALSE){
  
  if(!exists("population_mapping"))  population_mapping <- get_population()
  
  responses <- get_response(response, date_var, window, geo_type)
  
  sensors <- get_sensors(signals_to_plot, date_var, lag, window, geo_type)
  
  plot_corr(sensors, responses, population_mapping, geo_type, freescales)
}


#' Wrapper to plot correlations - signals from df
#' 
#' @param signals_df data frame with cols geo_val, time_value, value, source, signal
#'                   time_value should be string 'YYYY-MM-DD'
#'                   date_var will be set to be the latest date value on the dataset
#'                   window will be the date range in the dataset
#' @param response string vector with two elements, first the source and second the signal namefor the API
#' @param lag temporal spacing between signal and response to compute correlation. eg: correlation between deaths today and GHT two weeks ago. lag = 2 weeks
#' @param geo_type geographical granularity. "county", "state", or "msa"
#' @return plot
plot_new_signal <- function(signals_df, response, lag, geo_type, freescales = FALSE){
  
  if(sum(names(signals_df) %in% c("geo_value", "time_value", "value", "source", "signal")) != 5) stop("file does not have correct column names")
  
  date_var <- max(as.Date(signals_df$time_value)) + lag
  window <- length(unique(as.Date(signals_df$time_value)))
  
  signals <- signals_df %>% 
    select(geo_value, time_value, value, source, signal) %>% 
    group_by(geo_value, source, signal) %>% 
    summarise(avgsignal = mean(value))
  
  response <- get_response(response, date_var, window, geo_type)
  
  if(!exists("population_mapping"))  population_mapping <- get_population()
  
  plot_corr(signals, response, population_mapping, geo_type, freescales)
}


#' Wrapper to plot correlations - signals from csv file
#' 
#' @param csv_file_path path to csv file with cols geo_val, time_value, value, source, signal
#'                      time_value should be string 'YYYY-MM-DD'
#'                      date_var will be set to be the latest date value on the dataset
#'                      window will be the date range in the dataset
#' @param response string vector with two elements, first the source and second the signal namefor the API
#' @param lag temporal spacing between signal and response to compute correlation. eg: correlation between deaths today and GHT two weeks ago. lag = 2 weeks
#' @param geo_type geographical granularity. "county", "state", or "msa"
#' @return plot
plot_from_csv <- function(csv_file_path, response, lag, geo_type, freescales = FALSE){
  
  signals_df <- read.csv(csv_file_path, header = T) %>% select(geo_value, value, time_value, source, signal)
  
  plot_new_signal(signals_df, response, lag, geo_type, freescales)
}




# covidcast_meta() %>% select(data_source, signal) %>% distinct()
# 
# lag = 0
# window = 7
# date_var = ymd("20200417")
# geo_type = "msa"
# #response = c("google-survey", "smoothed_cli") ## source and signal
# response = c("jhu-csse", "confirmed_incidence_prop") ## source and signal
# signals_to_plot <- rbind(c("doctor-visits", "smoothed_cli"),
#                          c("fb-survey", "smoothed_cli"),
#                          c("fb-survey", "smoothed_hh_cmnty_cli"),
#                          c("google-survey", "smoothed_cli"),
#                          c("ght", "smoothed_search"),
#                          c("indicator-combination", "nmf_day_doc_fbs_ght"))
# 
# population_mapping <- get_population()
# 
# signal_correlations(signals_to_plot, response, date_var, lag, window, geo_type, freescales = TRUE)
# signal_correlations(signals_to_plot, response, date_var, lag, window, geo_type = "state")
# signal_correlations(signals_to_plot, response, date_var, lag, window, geo_type = "msa")
# csv_file <- read.csv("~/Downloads/testquidel_state_raw.csv", header = T)
# csv_file$time_value <- as.Date(csv_file$time_value)
# csv_file <- csv_file %>% filter(time_value > max(csv_file$time_value) - 7)
# write.csv(csv_file, "~/Desktop/file_to_app.csv")

