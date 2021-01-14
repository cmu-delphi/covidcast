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
    expect_equal(scaled_card$ahead,  c(1, 2))
    expect_equal(scaled_card$coverage_80,  c(1, 0))
    expect_equal(scaled_card$forecaster,  c("f1", "f1"))
    expect_equal(scaled_card$ae,  c(0.5, 0.25))
    expect_equal(scaled_card$wis,  c(0.6, 7/9))
})