library(tibble)
test_that("aggregate_cards aggregates prediction cards", {
    # Base data for prediction cards
    prediction_cards <- list(
        tibble(
            location = c("01", "02"),
            forecast_distribution = list(
                tibble(
                    probs = seq(0.05, 0.95, 0.1),
                    quantiles = seq(1, 10)
                ),
                tibble(
                    probs = seq(0.05, 0.95, 0.1),
                    quantiles = seq(11, 20)
                )
            )
        ),
        tibble(
            location = c("01", "02"),
            forecast_distribution = list(
                tibble(
                    probs = seq(0.05, 0.95, 0.1),
                    quantiles = seq(21, 30)
                ),
                tibble(
                    probs = seq(0.05, 0.95, 0.1),
                    quantiles = seq(31, 40)
                )
            )
        )   
    )

    # Attributes for each card in `prediction_cards`.
    card_attributes <- list(
        list(
            ahead = 1,
            forecast_date = "2020-10-05",
            geo_type = "state",
            geo_values = "OH",
            incidence_period = "epiweek",
            name_of_forecaster = "f1",
            signals = list(
                data_source = "source 1",
                signal = "sig 1"
            )
        ),
        list(
            ahead = 4,
            forecast_date = "2020-11-05",
            geo_type = "state",
            geo_values = "MN",
            incidence_period = "epiweek",
            name_of_forecaster = "f2",
            signals = list(
                data_source = "source 2",
                signal = "sig 2"
            )
        )
    )

    # Assign attributes to the prediction cards.
    for (i in seq_along(prediction_cards)) {
        class(prediction_cards[[i]]) <- c("prediction_card", class(prediction_cards[[i]]))
        attributes(prediction_cards[[i]]) <- c(attributes(prediction_cards[[i]]),
                                               card_attributes[[i]])
    }

    expected <- tibble(
        location = rep(c(rep("01", 10), rep("02", 10)), 2),
        probs = rep(seq(0.05, 0.95, 0.1), 4),
        quantiles = seq(1, 40),
        ahead = c(rep(1, 20), rep(4, 20)),
        data_source = c(rep("source 1", 20), rep("source 2", 20)),
        forecast_date = c(rep("2020-10-05", 20), rep("2020-11-05", 20)),
        geo_type = c(rep("state", 40)),
        geo_values = c(rep("OH", 20), rep("MN", 20)),
        incidence_period = c(rep("epiweek", 40)),
        name_of_forecaster = c(rep("f1", 20), rep("f2", 20)),
        signal = c(rep("sig 1", 20), rep("sig 2", 20))
    )
    actual <- aggregate_cards(prediction_cards)
    expect_identical(actual, expected)
})

test_that("aggregate_cards aggregates evaluation cards", {
    # Base data for evaluation cards
    evaluation_cards <- list(
        tibble(
            location = c("01", "02"),
            forecast_date = "2020-02-14",
            start = "2020-02-09",
            end = "2020-02-15",
            actual = c(5, 15),
            forecast_distribution = list(
                tibble(
                    probs = seq(0.05, 0.95, 0.1),
                    quantiles = seq(1, 10)
                ),
                tibble(
                    probs = seq(0.05, 0.95, 0.1),
                    quantiles = seq(11, 20)
                )
            ),
            wis = c(10, 10),
            ae = c(5, 5),
            coverage_80 = c(TRUE, TRUE)
        ),
        tibble(
            location = c("01", "02"),
            forecast_date = "2020-02-29",
            start = "2020-02-24",
            end = "2020-03-01",
            actual = c(25, 35),
            forecast_distribution = list(
                tibble(
                    probs = seq(0.05, 0.95, 0.1),
                    quantiles = seq(21, 30)
                ),
                tibble(
                    probs = seq(0.05, 0.95, 0.1),
                    quantiles = seq(31, 40)
                )
            ),
            wis = c(10, 10),
            ae = c(5, 5),
            coverage_80 = c(TRUE, TRUE)
        )   
    )

    # Attributes for each card in `prediction_cards`.
    card_attributes <- list(
        list(
            ahead = 1,
            as_of = "2020-08-08",
            backfill_buffer = 10,
            geo_type = "state",
            incidence_period = "epiweek",
            name_of_forecaster = "f1",
            response = list(
                data_source = "source 1",
                signal = "sig 1"
            )
        ),
        list(
            ahead = 4,
            as_of = "2020-09-09",
            backfill_buffer = 14,
            geo_type = "state",
            incidence_period = "epiweek",
            name_of_forecaster = "f2",
            response = list(
                data_source = "source 2",
                signal = "sig 2"
            )
        )
    )

    # Assign attributes to the evaluation cards.
    for (i in seq_along(evaluation_cards)) {
        class(evaluation_cards[[i]]) <- c("evaluation_card", class(evaluation_cards[[i]]))
        attributes(evaluation_cards[[i]]) <- c(attributes(evaluation_cards[[i]]),
                                               card_attributes[[i]])
    }

    expected <- tibble(
        location = rep(c(rep("01", 10), rep("02", 10)), 2),
        forecast_date = c(rep("2020-02-14", 20), rep("2020-02-29", 20)),
        start = c(rep("2020-02-09", 20), rep("2020-02-24", 20)),
        end = c(rep("2020-02-15", 20), rep("2020-03-01", 20)),
        actual = c(rep(5, 10), rep(15, 10), rep(25, 10), rep(35, 10)),
        probs = rep(seq(0.05, 0.95, 0.1), 4),
        quantiles = seq(1, 40),
        wis = rep(10, 40),
        ae = rep(5, 40),
        coverage_80 = rep(TRUE, 40),
        ahead = c(rep(1, 20), rep(4, 20)),
        as_of = c(rep("2020-08-08", 20), rep("2020-09-09", 20)),
        backfill_buffer = c(rep(10, 20), rep(14, 20)),
        data_source = c(rep("source 1", 20), rep("source 2", 20)),
        geo_type = c(rep("state", 40)),
        incidence_period = c(rep("epiweek", 40)),
        name_of_forecaster = c(rep("f1", 20), rep("f2", 20)),
        signal = c(rep("sig 1", 20), rep("sig 2", 20))
    )
    actual <- aggregate_cards(evaluation_cards)
    expect_identical(actual, expected)
})