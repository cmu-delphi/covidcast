
data_corrector <- function(df, apply_corrections) {
  corrected <- apply_corrections(df)
  assertthat::assert_that(all(
    c("location","signal","time_value","issue", "data_source","corrected") %in%
      names(corrected)),
    msg = "requested corrections drops some required columns"
  )
  corrected <- dplyr::select(corrected,
                             dplyr::all_of(
                               c("location","signal","time_value","issue",
                                 "data_source","corrected")))

  nr <-  nrow(df)
  df <-  dplyr::left_join(df,
                          corrected,
                          by = c("location","signal","time_value",
                                 "issue","data_source"))

  assertthat::assert_that(nr == nrow(df),
                          msg="requested corrections generate duplicate rows.")
  df <-  mutate(df, value = ifelse(!is.na(corrected),corrected,.data$value),
                corrected = NULL)
  df
}
