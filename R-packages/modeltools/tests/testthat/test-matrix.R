library(tibble)

test_that("training and prediction matrices are created", {
    df <- tibble(
        geo_value = rep(c("az", "wv"), 5),
        time_value = rep(
            as.Date(c("2021-01-25", "2021-01-26", "2021-01-27", "2021-01-28", "2021-01-29")),
            each = 2),
        `value-2:signal_1` = seq(-3, 6),
        `value-2:signal_2` = seq(7, 16),
        `value-1:signal_1` = seq(-1, 8),
        `value-1:signal_2` = seq(9, 18),
        `value+0:signal_1` = seq(1, 10),
        `value+0:signal_2` = seq(11, 20),
        `response+1:signal_2` = c(seq(13, 20), rep(NA, 2)),
        `response+2:signal_2` = c(seq(15, 20), rep(NA, 4))
    )

    out <- create_train_and_predict_matrices(df, ahead = 2, 
                                             training_window_size = 1)

    expect_equal(names(out), c("train_x", "train_y", "train_geo_values",
                               "train_time_values", "train_end_date",
                               "predict_x", "predict_geo_values", "predict_time_value"))
    expect_equal(out$train_x,
                 as.matrix(tibble(
                    `value-2:signal_1` = c(1, 2),
                    `value-2:signal_2` = c(11, 12),
                    `value-1:signal_1` = c(3, 4),
                    `value-1:signal_2` = c(13, 14),
                    `value+0:signal_1` = c(5, 6),
                    `value+0:signal_2` = c(15, 16)))
    )
    expect_equal(out$train_y, c(19, 20))
    expect_equal(out$train_geo_values, c("az", "wv"))
    expect_equal(out$train_time_values, rep(as.Date("2021-01-27"), 2))
    expect_equal(out$train_end_date, as.Date("2021-01-27"))
    expect_equal(out$predict_x,
                 as.matrix(tibble(
                    `value-2:signal_1` = c(5, 6),
                    `value-2:signal_2` = c(15, 16),
                    `value-1:signal_1` = c(7, 8),
                    `value-1:signal_2` = c(17, 18),
                    `value+0:signal_1` = c(9, 10),
                    `value+0:signal_2` = c(19, 20)))
    )
    expect_equal(out$predict_geo_values, c("az", "wv"))
    expect_equal(out$predict_time_value, as.Date("2021-01-29"))
})

test_that("training and prediction matrices for multiple aheads (separate)", {
    df <- tibble(
        geo_value = rep(c("az", "wv"), 5),
        time_value = rep(
            as.Date(c("2021-01-25", "2021-01-26", "2021-01-27", "2021-01-28", "2021-01-29")),
            each = 2),
        `value-2:signal_1` = seq(-3, 6),
        `value-2:signal_2` = seq(7, 16),
        `value-1:signal_1` = seq(-1, 8),
        `value-1:signal_2` = seq(9, 18),
        `value+0:signal_1` = seq(1, 10),
        `value+0:signal_2` = seq(11, 20),
        `response+1:signal_2` = c(seq(13, 20), rep(NA, 2)),
        `response+2:signal_2` = c(seq(15, 20), rep(NA, 4))
    )
    
    out <- create_train_and_predict_matrices(df, ahead = c(1, 2), 
                                             training_window_size = 1)
    
    expect_equal(length(out), 2)
    expect_equal(out[[1]], create_train_and_predict_matrices(df, ahead = 1, 
                                                             training_window_size = 1))
    expect_equal(out[[2]], create_train_and_predict_matrices(df, ahead = 2, 
                                                             training_window_size = 1))
})

test_that("training and prediction matrices for multiple aheads (together)", {
    df <- tibble(
        geo_value = rep(c("az", "wv"), 5),
        time_value = rep(
            as.Date(c("2021-01-25", "2021-01-26", "2021-01-27", "2021-01-28", "2021-01-29")),
            each = 2),
        `value-2:signal_1` = seq(-3, 6),
        `value-2:signal_2` = seq(7, 16),
        `value-1:signal_1` = seq(-1, 8),
        `value-1:signal_2` = seq(9, 18),
        `value+0:signal_1` = seq(1, 10),
        `value+0:signal_2` = seq(11, 20),
        `response+1:signal_2` = c(seq(13, 20), rep(NA, 2)),
        `response+2:signal_2` = c(seq(15, 20), rep(NA, 4))
    )
    
    out <- create_train_and_predict_matrices(df, ahead = c(1, 2), 
                                             training_window_size = 1,
                                             aheads_separate = FALSE)
    
    expect_equal(names(out), c("train_x", "train_y", "train_geo_values",
                               "train_time_values", "train_end_date",
                               "predict_x", "predict_geo_values", "predict_time_value"))
    expect_equal(out$train_x,
                 as.matrix(tibble(
                     `value-2:signal_1` = c(1, 2, 3, 4),
                     `value-2:signal_2` = c(11, 12, 13, 14),
                     `value-1:signal_1` = c(3, 4, 5, 6),
                     `value-1:signal_2` = c(13, 14, 15, 16),
                     `value+0:signal_1` = c(5, 6, 7, 8),
                     `value+0:signal_2` = c(15, 16, 17, 18)))
    )
    expect_equal(out$train_y,
                 as.matrix(tibble(
                     `response+1:signal_2` = c(17, 18, 19, 20),
                     `response+2:signal_2` = c(19, 20, NA, NA)))
    )
    expect_equal(out$train_geo_values, rep(c("az", "wv"), 2))
    expect_equal(out$train_time_values, 
                 rep(as.Date(c("2021-01-27", "2021-01-28")), each = 2))
    expect_equal(out$train_end_date, as.Date("2021-01-28"))
    expect_equal(out$predict_x,
                 as.matrix(tibble(
                     `value-2:signal_1` = c(5, 6),
                     `value-2:signal_2` = c(15, 16),
                     `value-1:signal_1` = c(7, 8),
                     `value-1:signal_2` = c(17, 18),
                     `value+0:signal_1` = c(9, 10),
                     `value+0:signal_2` = c(19, 20)))
    )
    expect_equal(out$predict_geo_values, c("az", "wv"))
    expect_equal(out$predict_time_value, as.Date("2021-01-29"))
})

test_that("fails with multiple responses", {
    df <- tibble(
        geo_value = rep(c("az", "wv"), 5),
        time_value = rep(
            as.Date(c("2021-01-25", "2021-01-26", "2021-01-27", "2021-01-28", "2021-01-29")),
            each = 2),
        `value-2:signal_1` = seq(-3, 6),
        `value-2:signal_2` = seq(7, 16),
        `value-1:signal_1` = seq(-1, 8),
        `value-1:signal_2` = seq(9, 18),
        `value+0:signal_1` = seq(1, 10),
        `value+0:signal_2` = seq(11, 20),
        `response+1:signal_1` = c(seq(3, 10), rep(NA, 2)),
        `response+1:signal_2` = c(seq(13, 20), rep(NA, 2)),
        `response+2:signal_1` = c(seq(5, 10), rep(NA, 4)),
        `response+2:signal_2` = c(seq(15, 20), rep(NA, 4))
    )
    expect_error(
        create_train_and_predict_matrices(df, 2, 1),
        "multiple responses at ahead = 2"
    )
})
