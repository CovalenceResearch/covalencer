test_that("ggplot theme works", {
    p <- ggplot2::ggplot(mtcars,
                         ggplot2::aes(x = mpg, y = wt)) +
        ggplot2::geom_point() +
        theme_covalence()
    vdiffr::expect_doppelganger("themed scatterplot", p)
})
