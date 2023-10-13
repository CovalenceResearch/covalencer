plot_psa_scatter <- function(data,
                             delta_qalys = NULL,
                             delta_costs = NULL,
                             ...) {
    p <- ggplot2::ggplot(data = data,
                         ggplot2::aes(x = {
                             {
                                 delta_qalys
                             }
                         },
                         y = {
                             {
                                 delta_costs
                             }
                         })) +
        ggplot2::geom_point() +
        theme_covalence() %+replace%
        theme(
            panel.grid = ggplot2::element_blank()
        )

    p
}
