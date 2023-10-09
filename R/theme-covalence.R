theme_covalence <- function(base_size = 12,
                            base_family = "Lato",
                            struct_color = "#f0f0f0",
                            text_color = "#000000") {
    ggplot2::theme_bw(base_size = base_size,
                      base_family = base_family) +
        ggplot2::theme(
            axis.ticks = ggplot2::element_line(color = struct_color,
                                               linewidth = ggplot2::rel(0.5)),
            axis.ticks.length = ggplot2::unit(2, "pt"),
            axis.title = ggtext::element_markdown(face = "bold",
                                                  size = ggplot2::rel(1.0)),
            axis.text = ggtext::element_markdown(color = text_color,
                                                 size = ggplot2::rel(0.80)),
            legend.background = ggplot2::element_blank(),
            legend.direction = "horizontal",
            legend.justification = "right",
            legend.position = "top",
            legend.text = ggtext::element_markdown(size = ggplot2::rel(1.0)),
            legend.title = ggtext::element_markdown(face = "bold",
                                                    size = ggplot2::rel(1.1)),
            legend.margin = ggplot2::margin(0, 0, 0, 0, "pt"),
            panel.border = ggplot2::element_rect(color = struct_color),
            panel.grid.minor = ggplot2::element_line(),
            panel.grid.major = ggplot2::element_line(color = struct_color,
                                                     linewidth = ggplot2::rel(0.5)),
            panel.spacing = ggplot2::unit(1, "lines"),
            plot.caption = ggtext::element_markdown(
                margin = ggplot2::margin(4, 0, 0, 0, "pt"),
                size = ggplot2::rel(0.80)
            ),
            plot.caption.position = "plot",
            plot.margin = ggplot2::margin(6, 6, 6, 6, "pt"),
            plot.subtitle = ggtext::element_markdown(
                margin = ggplot2::margin(4, 0, 0, 0, "pt"),
                size = ggplot2::rel(1.1)
            ),
            plot.title = ggtext::element_markdown(face = "bold",
                                                  size = ggplot2::rel(1.5)),
            plot.title.position = "plot",
            strip.background = ggplot2::element_rect(fill = struct_color),
            strip.text = ggtext::element_markdown(
                color = text_color,
                face = "italic",
                hjust = 0,
                size = ggplot2::rel(1.1)
            )
        )
}
