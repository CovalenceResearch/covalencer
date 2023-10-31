
# PSA test data
test_psa <- data.frame(
    delta_qalys = rnorm(2000, 2, 2),
    delta_costs = rnorm(2000, 10000, 1000),
    country = rep(c("Country A", "Country B"), each = 1000)
)

saveRDS(test_psa, file = testthat::test_path("fixtures", "test_psa.rds"))
