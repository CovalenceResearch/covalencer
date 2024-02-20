test_that("scales work", {
  p_discrete_scatter <- ggplot2::ggplot(
    ggplot2::diamonds,
    ggplot2::aes(x = carat, y = depth)
  ) +
    ggplot2::geom_point(ggplot2::aes(color = color)) +
    scale_color_covalence_discrete(
      palette = "rainbow_12_bit",
      reverse = TRUE
    )

  p_discrete_histogram <- ggplot2::ggplot(
    ggplot2::diamonds,
    ggplot2::aes(x = carat)
  ) +
    ggplot2::geom_histogram(ggplot2::aes(fill = cut)) +
    scale_fill_covalence_discrete(
      palette = "complete",
      reverse = TRUE
    )

  p_diverging <- ggplot2::ggplot(
    ggplot2::diamonds,
    ggplot2::aes(x = carat, y = depth)
  ) +
    ggplot2::geom_point(ggplot2::aes(color = price)) +
    scale_color_covalence_diverging(palette = "orange_purple")

  p_sequential <- ggplot2::ggplot(
    ggplot2::diamonds,
    ggplot2::aes(x = cut, y = color)
  ) +
    ggplot2::geom_tile(ggplot2::aes(fill = depth)) +
    scale_fill_covalence_sequential(palette = "purples") +
    ggplot2::theme(panel.grid = ggplot2::element_blank())

  vdiffr::expect_doppelganger(
    "discrete color scale scatter",
    p_discrete_scatter
  )
  vdiffr::expect_doppelganger(
    "discrete color scale histogram",
    p_discrete_histogram
  )
  vdiffr::expect_doppelganger("diverging color scale", p_diverging)
  vdiffr::expect_doppelganger("sequential color scale", p_sequential)
})
