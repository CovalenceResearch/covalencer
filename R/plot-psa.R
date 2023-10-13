plot_psa_scatter <- function(data,
                             delta_qalys = NULL,
                             delta_costs = NULL,
                             currency = "$",
                             point_alpha = 0.4,
                             point_color = "#249bc9",
                             point_shape = 16,
                             point_size = 2,
                             ...) {
    # Calculate symmetrical plot limits
    col_qalys <- rlang::as_string(rlang::ensym(delta_qalys))
    col_costs <- rlang::as_string(rlang::ensym(delta_costs))

    max_qalys <- max(abs(data[[col_qalys]]))
    max_costs <- max(abs(data[[col_costs]]))

    limits_qalys <- c(-max_qalys, max_qalys)
    limits_costs <- c(-max_costs, max_costs)

    # Create scatter plot
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
            labels = scales::number_format(big.mark = ","),
            limits = limits_qalys
        ) +
        ggplot2::scale_y_continuous(
            "Incremenctal costs",
            labels = scales::label_dollar(prefix = currency, big.mark = ","),
            limits = limits_costs
        ) +
        theme_covalence() %+replace%
        theme(panel.grid.major = ggplot2::element_blank())

    p
}
