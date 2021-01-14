test_that("scale_by_forecaster works", {
    score_card <- tibble(
        forecaster = c("f1", "f2", "f1", "f2"),
        ahead = c(1, 1, 2, 2),
        coverage_80 = c(1, 1, 0, 0),
        ae = c(1, 2, 4, 16),
        wis = c(3, 5, 7, 9)
    )
    scaled_card <- scale_by_forecaster(score_card, c("ae", "wis"), "f2")

    expect_equal(colnames(scaled_card), 
                 c("ahead", "coverage_80", "forecaster", "ae", "wis"))
    expect_equal(scaled_card$ahead, c(1, 2))
    expect_equal(scaled_card$coverage_80, c(1, 0))
    expect_equal(scaled_card$forecaster, c("f1", "f1"))
    expect_equal(scaled_card$ae, c(0.5, 0.25))
    expect_equal(scaled_card$wis, c(0.6, 7/9))
})

test_that("scale_by_forecaster removes excess variables", {
    score_card <- tibble(
        forecaster = c("f1", "f2", "f1", "f2"),
        ahead = c(1, 1, 2, 2),
        coverage_80 = c(1, 1, 0, 0),
        ae = c(1, 2, 4, 16),
        wis = c(3, 5, 7, 9)
    )
    # wis should be removed from output
    scaled_card <- scale_by_forecaster(score_card, c("ae"), "f2")

    expect_equal(colnames(scaled_card), 
                 c("ahead", "coverage_80", "forecaster", "ae"))
    expect_equal(scaled_card$ahead, c(1, 2))
    expect_equal(scaled_card$coverage_80, c(1, 0))
    expect_equal(scaled_card$forecaster, c("f1", "f1"))
    expect_equal(scaled_card$ae, c(0.5, 0.25))
})

test_that("scale_by_forecaster detects base_forecaster_name argument errors", {
    score_card <- tibble(
        forecaster = "f",
        ahead = c(1, 1, 2, 2),
        coverage_80 = c(1, 1, 0, 0),
        ae = c(1, 2, 4, 16),
        wis = c(3, 5, 7, 9)
    )
    expect_error(scale_by_forecaster(score_card, c("ae"), "not_f"),
                 "score_card has no forecaster named not_f.")

    expect_error(scale_by_forecaster(score_card, c("ae"), "f"), 
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
                                     "f1"),
                'score_card must have a column named "forecaster"')

    expect_error(scale_by_forecaster(score_card, c("ae", "X", "Y"), "f1"), 
                 "score_cols contains columns X, Y not present in the columns of score_card")

    expect_error(scale_by_forecaster(score_card, c("ae", "ahead", "coverage_80"), "f1"), 
                 "score_cols contains columns ahead, coverage_80 that are not numeric errors")
})
