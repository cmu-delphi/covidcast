test_that("scale_by_forecaster works on score_cards", {
    score_card <- tibble(
        forecaster = c("f1", "f2", "f1", "f2"),
        ahead = c(1, 1, 2, 2),
        geo_value = c("mo", "mo", "ar", "ar"),
        forecast_date = c("d1", "d1", "d2", "d2"),
        data_source = "source",
        signal = "signal",
        target_end_date = "date",
        incidence_period = "epiweek",
        actual = seq(1, 4),
        coverage_80 = c(1, 1, 0, 0),
        ae = c(1, 2, 4, 16),
        wis = c(3, 5, 7, 9)
    )
    scaled_card <- scale_by_forecaster(score_card, c("ae", "wis"), "f2")

    expect_equal(colnames(scaled_card), 
                 c("ahead", "geo_value", "forecast_date", "data_source", "signal", "target_end_date", "incidence_period", "forecaster", "ae", "wis"))
    expect_equal(scaled_card$forecaster, c("f1", "f1"))
    expect_equal(scaled_card$ahead, c(1, 2))
    expect_equal(scaled_card$geo_value, c("mo", "ar"))
    expect_equal(scaled_card$forecast_date, c("d1", "d2"))
    expect_equal(scaled_card$data_source, c("source", "source"))
    expect_equal(scaled_card$signal, c("signal", "signal"))
    expect_equal(scaled_card$target_end_date, c("date", "date"))
    expect_equal(scaled_card$incidence_period, c("epiweek", "epiweek"))
    expect_equal(scaled_card$ae, c(0.5, 0.25))
    expect_equal(scaled_card$wis, c(0.6, 7/9))
})

test_that("scale_by_forecaster works on non-score_cards", {
    score_card <- tibble(
        forecaster = c("f1", "f2", "f1", "f2"),
        ahead = c(1, 1, 2, 2),
        coverage_80 = c(1, 1, 0, 0),
        ae = c(1, 2, 4, 16),
        wis = c(3, 5, 7, 9)
    )
    scaled_card <- scale_by_forecaster(score_card, c("ae", "wis"), "f2", c("forecaster", "ahead"))

    expect_equal(colnames(scaled_card), 
                 c("ahead", "forecaster", "ae", "wis"))
    expect_equal(scaled_card$forecaster, c("f1", "f1"))
    expect_equal(scaled_card$ahead, c(1, 2))
    expect_equal(scaled_card$ae, c(0.5, 0.25))
    expect_equal(scaled_card$wis, c(0.6, 7/9))
})

test_that("scale_by_forecaster detects base_forecaster_name argument errors", {
    score_card <- tibble(
        forecaster = "f",
        ahead = c(1, 1, 2, 2),
        coverage_80 = c(1, 1, 0, 0),
        ae = c(1, 2, 4, 16),
        wis = c(3, 5, 7, 9)
    )

    expect_error(scale_by_forecaster(score_card, c("ae"), "not_f", c("forecaster", "ahead")),
                 "score_card has no forecaster named not_f.")

    expect_error(scale_by_forecaster(score_card, c("ae"), "f", c("forecaster", "ahead")), 
                 "scale_by_forecaster requires the score card to have forecasters other than f")
})

test_that("scale_by_forecaster dies with bad columns", {
    score_card <- tibble(
        forecaster = c("f1", "f2", "f1", "f2"),
        ahead = c(1, 1, 2, 2),
        coverage_80 = c(1, 1, 0, 0),
        ae = c(1, 2, 4, 16),
        wis = c(3, 5, 7, 9)
    )

    expect_error(scale_by_forecaster(score_card %>% select(-forecaster),
                                     c("ae"),
                                     "f1",
                                     c("forecaster", "ahead")),
                'score_card must have a column named "forecaster"')

    expect_error(scale_by_forecaster(score_card,
                                     c("ae", "X", "Y"),
                                     "f1",
                                     c("forecaster", "ahead")), 
                 "score_cols contains columns X, Y not present in the columns of score_card")

    expect_error(scale_by_forecaster(score_card,
                                     c("ae"),
                                     "f1",
                                     c("A", "forecaster", "B", "ahead")), 
                 "id_cols contains columns A, B not present in the columns of score_card")

    expect_error(scale_by_forecaster(score_card,
                                     c("ae", "coverage_80"),
                                     "f1",
                                     c("forecaster", "ahead")), 
                 "divide-by-zero error in column coverage_80")
})

test_that("scale_by_forecaster keeps base forecast rows", {
    score_card <- tibble(
        forecaster = c("f1", "f2", "f1", "f2"),
        ahead = c(1, 1, 2, 2),
        coverage_80 = c(1, 1, 0, 0),
        ae = c(1, 2, 4, 16),
        wis = c(3, 5, 7, 9)
    )
    scaled_card <- scale_by_forecaster(score_card,
                                       c("ae", "wis"),
                                       "f2",
                                       c("forecaster", "ahead"),
                                       FALSE)

    expect_equal(colnames(scaled_card), 
                 c("ahead", "forecaster", "ae", "wis"))
    expect_equal(scaled_card$forecaster, c("f1", "f2", "f1", "f2"))
    expect_equal(scaled_card$ahead, c(1, 1, 2, 2))
    expect_equal(scaled_card$ae, c(0.5, 1, 0.25, 1))
    expect_equal(scaled_card$wis, c(0.6, 1, 7/9, 1))
})
