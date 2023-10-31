# RoB2 plotting
test_rob2_summary <- data.frame(
    bias = rep(
        c(
            "Overall",
            "Bias from randomization process",
            "Deviations from intended interventions",
            "Missing outcome",
            "Outcome measurement",
            "Selection of reported result"
        ),
        each = 3
    ),
    assess = rep(c("Low", "Some concerns", "High risk of bias"),
                 times = 3),
    percentage = c(
        0.1,
        0.1,
        0.8,
        0.5,
        0.5,
        0.0,
        0.4,
        0.3,
        0.3,
        0.9,
        0.05,
        0.05,
        0.7,
        0.25,
        0.05,
        0.2,
        0.2,
        0.6
    )
)

saveRDS(test_rob2_summary,
        file = testthat::test_path("fixtures", "test_rob2_summary.rds"))

# PSA test data
test_psa <- data.frame(
    delta_qalys = rnorm(2000, 2, 2),
    delta_costs = rnorm(2000, 10000, 1000),
    country = rep(c("Country A", "Country B"), each = 1000)
)

saveRDS(test_psa, file = testthat::test_path("fixtures", "test_psa.rds"))
