#' The default Covalence [ggplot2] theme
#'
#' @description
#' This theme provides the defaults required to generate plots, using [ggplot2],
#' that are aligned with the Covalence Research house style. The theme can be
#' modified further by using `theme()`. Also consider using the various color
#' palettes to ensure the house style is met.
#'
#' @param base_size        Base font size, in points. Default is 12.
#' @param base_family      Base font family. Default is "sans".
#' @param strip_bg_color   Color to fill strip backgrounds. Default is #326aa0.
#' @param strip_text_color Color for strip labels. Default is #FFFFFF (white).
#'
#' @return A ggplot2 [theme][ggplot2::theme] object.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(data = msleep, aes(x = brainwt, y = bodywt)) +
#'    geom_point() +
#'    theme_covalence()
#'
#' ggplot(data = msleep, aes(x = awake, y = name)) +
#'    geom_col() +
#'    facet_wrap(vars(vore)) +
#'    theme_covalence()
theme_covalence <- function(base_size = 12,
                            base_family = "sans",
                            strip_bg_color = "#326aa0",
                            strip_text_color = "#FFFFFF") {
    ggplot2::theme_light(base_size = base_size,
                         base_family = base_family) +
        ggplot2::theme(
            axis.ticks = ggplot2::element_line(
                color = "#FAFAFA",
                linewidth = ggplot2::rel(0.5)
            ),
            axis.ticks.length = ggplot2::unit(2, "pt"),
            axis.title = ggtext::element_markdown(face = "bold",
                                                  size = ggplot2::rel(1.0)),
            axis.text = ggtext::element_markdown(color = "#000000",
                                                 size = ggplot2::rel(0.80)),
            legend.background = ggplot2::element_blank(),
            legend.direction = "horizontal",
            legend.justification = "right",
            legend.margin = ggplot2::margin(0, 0, 0, 0, "pt"),
            legend.position = "top",
            legend.text = ggtext::element_markdown(size = ggplot2::rel(1.0)),
            legend.title = ggtext::element_markdown(face = "bold",
                                                    size = ggplot2::rel(1.1)),
            panel.background = ggplot2::element_rect(fill = "#FFFFFF"),
            panel.grid.minor = ggplot2::element_blank(),
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
            strip.background =
                ggplot2::element_rect(fill = strip_bg_color),
            strip.text = ggtext::element_markdown(
                color = strip_text_color,
                face = "bold",
                hjust = 0,
                size = ggplot2::rel(1.0)
            )
        )
}
