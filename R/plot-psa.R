plot_psa_scatter <- function(data,
                             delta_qalys = NULL,
                             delta_costs = NULL,
                             currency = "$",
                             point_alpha = 0.2,
                             point_color = "#249bc9",
                             point_shape = 16,
                             point_size = 2,
                             ...) {
    p <- ggplot2::ggplot(data = data,
                         mapping = ggplot2::aes(x = {
                             {
                                 delta_qalys
                             }
                         },
                         y = {
                             {
                                 delta_costs
                             }
                         })) +
        ggplot2::geom_hline(
            yintercept = 0,
            color = "#000000",
            linewidth = 0.5
        ) +
        ggplot2::geom_vline(
            xintercept = 0,
            color = "#000000",
            linewidth = 0.5
        ) +
        ggplot2::geom_jitter(
            alpha = point_alpha,
            color = point_color,
            shape = point_shape,
            size = point_size
        ) +
        ggplot2::scale_x_continuous(
            "Incremental quality-adjusted life-years",
            labels = scales::number_format(big.mark = ",")
        ) +
        ggplot2::scale_y_continuous("Incremenctal costs",
                                    labels = scales::label_dollar(prefix = currency, big.mark = ",")) +
        theme_covalence() %+replace%
        theme(panel.grid.major = ggplot2::element_blank())

    p
}
