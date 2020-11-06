fc_round <- function(x){
  idx = rep(c(FALSE,TRUE), length.out=length(x))
  floor(x)*idx + ceiling(x)*(!idx)
}

na_replace <- function(a,b){
  stopifnot(length(a)==length(b) || length(b)==1)
  a[is.na(a)] = b[is.na(a)]
  a
}

vectorized_ifelse <- function(test, yes, no){
  tmp = yes
  tmp[!test] = no[!test]
  tmp
}


exp_w <- function(x, std_decay=30, b0=8, a=exp(1)/2){
  stopifnot(length(x) <= std_decay)
  w = (1:std_decay)/std_decay
  w = tail(w, length(x))
  1 / (1 + exp(-w*b0 + a))
}

missing_future <- function(selector, time_value, excess, preds){
  local_tail = (selector & time_value > time_value[max(which(excess>0))])
  if(!any(local_tail)) return(0L)
  tot = round(sum(preds[local_tail]))
  rmultinom(1, tot, as.numeric(local_tail))
}

corrections_multinom_roll <- function(
  x, excess, flag, time_value, max_lag=Inf, expectations = NULL, 
  inc_out_time = TRUE, reweight = function(x) rep(1, length(x))
  # seed_inc = 3500L
){
  stopifnot(length(x)==length(excess), length(excess)==length(flag))
  stopifnot(is.logical(flag), max_lag==floor(max_lag), max_lag >= 1)
  if(!is.null(expectations) && length(expectations)==1) expectations=NULL
  if(length(expectations) > 1) stopifnot(length(expectations) == length(x))
  
  locs = which(flag)
  if(length(locs) == 0) return(x)
  
  if(is.null(expectations)) expectations = rep(1, length(x))
  
  
  for(ii in locs) {
    if(ii <= max_lag){
      ii_lag = 1:(ii-1+inc_out_time)
    } else {
      ii_lag <- seq(ii-max_lag+1, ii-1+inc_out_time)
    }
    
    bin_w = pmax(expectations[ii_lag] / sum(expectations[ii_lag], na.rm = TRUE), 0)
    bin_w[is.na(bin_w)] = 0
    
    if(all(bin_w==0)) bin_w = rep(1/length(ii_lag), times=length(ii_lag))
    
    #reweight bin_w
    zz <- reweight(bin_w)
    bin_w <- zz*bin_w
    
    
    # set.seed(as.integer(ymd(time_value[ii]))*seed_inc + FIPS[ii])
    
    x[ii] = x[ii] - excess[ii]
    prop = x[ii_lag] + sign(excess[ii]) * rmultinom(1, abs(excess[ii]), bin_w)
    # possibly deal with negatives here
    x[ii_lag] = prop
    
    
    #rounded version
    #repl = fc_round(excess[ii] * bin_w)
    #x[ii] = x[ii] - sum(repl)
    #x[ii_lag] = x[ii_lag] + repl
  }
  x
}


STATE_TO_FIPS = c( ## copied from somewhere in the bowels of the code
  'WA'='53', 'DE'='10', 'DC'='11', 'WI'='55', 'WV'='54', 'HI'='15',
  'FL'='12', 'WY'='56', 'PR'='72', 'NJ'='34', 'NM'='35', 'TX'='48',
  'LA'='22', 'NC'='37', 'ND'='38', 'NE'='31', 'TN'='47', 'NY'='36',
  'PA'='42', 'AK'='02', 'NV'='32', 'NH'='33', 'VA'='51', 'CO'='08',
  'CA'='06', 'AL'='01', 'AR'='05', 'VT'='50', 'IL'='17', 'GA'='13',
  'IN'='18', 'IA'='19', 'MA'='25', 'AZ'='04', 'ID'='16', 'CT'='09',
  'ME'='23', 'MD'='24', 'OK'='40', 'OH'='39', 'UT'='49', 'MO'='29',
  'MN'='27', 'MI'='26', 'RI'='44', 'KS'='20', 'MT'='30', 'MS'='28',
  'SC'='45', 'KY'='21', 'OR'='41', 'SD'='46',
  'AS'='60', 'GU'='66', 'MP'='69', 'VI'='78', 'UM'='74'
)

nv_state_to_fips = as.numeric(STATE_TO_FIPS)
names(nv_state_to_fips) = names(STATE_TO_FIPS)

aldens_corrections = tibble::tibble(
  fips = as.numeric(c("34","17","25","25","48","29","10","34","34","48",
                      "25","34","39","39","29", "19", "16", "16", "16", "54",
                      "54")),
  time_value = ymd("2020-06-25","2020-07-07","2020-06-01","2020-06-30",
                   "2020-07-27","2020-07-23","2020-07-24","2020-07-22",
                   "2020-07-29","2020-08-02","2020-08-20","2020-08-26",
                   "2020-08-29","2020-08-30","2020-09-05", "2020-08-29",
                   "2020-09-03", "2020-09-04", " 2020-09-05", "2020-08-29", 
                   "2020-08-30"),
  varname = c(rep(TRUE, 9), FALSE, rep(TRUE,11)),
  imputed_val = c(40,37,62,29,194,8,1,17,18, 207, 344,5,22,22, 14,
                  14, 10, 10, 10, 5, 6),
  geo_value = tolower(names(nv_state_to_fips)[match(fips,nv_state_to_fips)])) %>%
  filter(varname)