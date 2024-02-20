test_that("calculating qx from a survfit without strata works", {
  data_no_strata <- readRDS(
    test_path(
      "fixtures",
      "test_lt_no_strata.rds"
    )
  )

  test_os1 <- with(
    survival::cancer,
    survival::Surv(time = time, event = status)
  )
  test_fit1 <- survival::survfit(test_os1 ~ 1, data = survival::cancer)
  expect_equal(calc_qx_from_surv(test_fit1), data_no_strata)
})

test_that("calculating qx from a survfit with strata works for days", {
  data_strata_days <- readRDS(
    test_path(
      "fixtures",
      "test_lt_strata_days.rds"
    )
  )

  test_os2 <- with(
    survival::cancer,
    survival::Surv(time = time, event = status)
  )
  test_fit2 <- survival::survfit(test_os2 ~ sex, data = survival::cancer)
  expect_equal(calc_qx_from_surv(test_fit2), data_strata_days)
})

test_that("calculating qx from a survfit with strata works for years to age 50", {
  data_strata_years <- readRDS(
    test_path(
      "fixtures",
      "test_lt_strata_years_to_age_50.rds"
    )
  )

  test_os3 <- with(
    survival::cancer,
    survival::Surv(time = time, event = status)
  )
  test_fit3 <- survival::survfit(test_os3 ~ sex, data = survival::cancer)
  expect_equal(
    calc_qx_from_surv(test_fit3,
      age_max_years = 50L,
      time_in = "years"
    ), data_strata_years
  )
})
