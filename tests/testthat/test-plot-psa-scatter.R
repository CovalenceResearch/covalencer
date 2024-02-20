test_that("psa scatterplot works", {
  data_psa <- readRDS(test_path("fixtures", "test_psa.rds"))

  p_nowtp_nomean <- plot_psa_scatter(
    data_psa,
    delta_qalys = delta_qalys,
    delta_costs = delta_costs,
    show_mean = FALSE
  )

  p_wtp_mean <- plot_psa_scatter(
    data_psa,
    delta_qalys = delta_qalys,
    delta_costs = delta_costs,
    plot_wtp_at = 10000
  )

  p_facet <- p_wtp_mean +
    ggplot2::facet_wrap(ggplot2::vars(country))

  vdiffr::expect_doppelganger("PSA scatterplot wo WTP and mean", p_nowtp_nomean)
  vdiffr::expect_doppelganger("PSA scatterplot with WTP and mean", p_wtp_mean)
  vdiffr::expect_doppelganger("Faceted PSA scatterplot", p_facet)
})
