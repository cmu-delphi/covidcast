library(RcppRoll)
source("process-funs.R")
window_size=14
start_date="2020-03-01"
sig_cut=3
size_cut=20
sig_consec=2.25
outlier_start_date="2020-03-15"
backfill_lag=30
excess_cut=0
riga <- riga %>% group_by(geo_value) %>% mutate(
  fmean = roll_meanr(value, window_size),
  smean = roll_mean(value, window_size, fill = NA),
  fmedian = roll_medianr(value, window_size),
  smedian = roll_median(value, window_size, fill = NA),
  fsd = roll_sdr(value, window_size),
  ssd = roll_sd(value, window_size,fill = NA),
  fmad = roll_medianr(abs(value-fmedian), window_size),
  smad = roll_median(abs(value-smedian), window_size, fill=NA),
  ftstat = abs(value-fmedian)/fsd, # mad in denominator is wrong scale, 
  ststat = abs(value-smedian)/ssd, # basically results in all the data flagged
  flag = 
    (abs(value) > size_cut & !is.na(ststat) & ststat > sig_cut) | # best case
    (is.na(ststat) & abs(value) > size_cut & !is.na(ftstat) & ftstat > sig_cut) | 
    # use filter if smoother is missing
    (value < 0 & !is.na(ststat) & !is.na(ftstat)), # big negative
  #(fmean > 10 & fmean< 20 & value > 2*sig_cut*fmean)
  flag = flag | # these allow smaller values to also be outliers if they are consecutive
    (dplyr::lead(flag) & !is.na(ststat) & ststat > sig_consec) | 
    (dplyr::lag(flag) & !is.na(ststat) & ststat > sig_consec) |
    (dplyr::lead(flag) & is.na(ststat) & ftstat > sig_consec) |
    (dplyr::lag(flag) & is.na(ststat) & ftstat > sig_consec),
  FIPS = as.numeric(STATE_TO_FIPS[toupper(geo_value)])
) %>% 
  mutate(
    state = toupper(geo_value),
    excess = value,
    flag_bad_RI = (state == "RI"  & value > 0 & lag(value) == 0),
    corrected = corrections_multinom_roll(
      value, value, flag_bad_RI, time_value, 7),
    corrected = corrections_multinom_roll(
      corrected, excess, (flag & !flag_bad_RI), time_value, 
      backfill_lag, 
      reweight=function(x) exp_w(x, backfill_lag)),
    corrected = corrected + 
      missing_future(state=="RI", time_value, excess, fmean)
  )
