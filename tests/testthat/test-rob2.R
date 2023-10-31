test_that("rob2 summary plot works", {
    data_rob2_summary <-
        readRDS(test_path("fixtures", "test_rob2_summary.rds"))

    # Full plot
    p_full <- plot_rob2_summary(
        data = data_rob2_summary,
        domain = bias,
        judgement = assess,
        study_share = percentage
    )

    vdiffr::expect_doppelganger("Full RoB2 summary plot", p_full)

    # One domain missing - warning
    expect_warning(
        plot_rob2_summary(
            data = data_rob2_summary[-(1:3),],
            domain = bias,
            judgement = assess,
            study_share = percentage
        )
    )
})
