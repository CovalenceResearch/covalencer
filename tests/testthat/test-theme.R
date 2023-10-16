test_that("ggplot theme works", {
    p <- ggplot2::ggplot(mtcars,
                         ggplot2::aes(x = mpg, y = wt)) +
        ggplot2::geom_point() +
        theme_covalence()

    p_facet <- p +
        ggplot2::facet_wrap(ggplot2::vars(cyl))

    vdiffr::expect_doppelganger("themed scatterplot", p)
    vdiffr::expect_doppelganger("faceted themed scatterplot", p_facet)
})
