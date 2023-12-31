#' The default Covalence [ggplot2] theme
#'
#' @description
#' This theme provides the defaults required to generate plots, using [ggplot2],
#' that are aligned with the Covalence Research house style. The theme can be
#' modified further by using `theme()`. Also consider using the various colour
#' palettes to ensure the house style is met.
#'
#' @param base_size        Base font size, in points. Default is 12.
#' @param base_family      Base font family. Default is "sans".
#' @param strip_bg_colour   colour to fill strip backgrounds. Default is #326aa0.
#' @param strip_text_colour colour for strip labels. Default is #FFFFFF (white).
#' @param ...              Additional arguments to be passed to the theme
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
#' ggplot(data = diamonds, aes(x = price, y = carat)) +
#'    geom_point(colour = covalence_colours("cov_lightblue"), alpha = 0.4) +
#'    facet_wrap(vars(cut)) +
#'    theme_covalence()
theme_covalence <- function(base_size = 12,
                            base_family = "sans",
                            strip_bg_colour = "#FFFFFF",
                            strip_text_colour = "#326aa0",
                            ...) {
    # Check arguments
    if (!is.numeric(base_size)) {
        cli::cli_abort("{.var base_size} must be numeric.")
    }

    if (!(is_hex(strip_bg_colour) |
          strip_bg_colour %in% grDevices::colours())) {
        cli::cli_abort("{.var strip_bg_colour} must be a hex colour or in the R colours.")
    }

    if (!(is_hex(strip_text_colour) |
          strip_text_colour %in% grDevices::colours())) {
        cli::cli_abort("{.var strip_text_colour} must be a hex colour or in the R colours.")
    }

    # ggplot2 theme
    ggplot2::theme_light(...,
                         base_size = base_size,
                         base_family = base_family) +
        ggplot2::theme(
            text = ggplot2::element_text(lineheight = 1.1),
            axis.line = ggplot2::element_line(colour = "#000000"),
            axis.ticks = ggplot2::element_blank(),
            axis.title = ggplot2::element_text(face = "bold",
                                               size = ggplot2::rel(1.0)),
            axis.text = ggplot2::element_text(colour = "#000000",
                                              size = ggplot2::rel(1.0)),
            axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 2, unit = "pt")),
            axis.text.y = ggplot2::element_text(
                hjust = 1,
                margin = ggplot2::margin(r = 2, unit = "pt")),
            legend.background = ggplot2::element_blank(),
            legend.direction = "horizontal",
            legend.justification = "right",
            legend.margin = ggplot2::margin(0, 0, 0, 0, "pt"),
            legend.position = "top",
            legend.text = ggplot2::element_text(size = ggplot2::rel(1.0)),
            legend.title = ggplot2::element_text(face = "bold",
                                                 size = ggplot2::rel(1.1)),
            panel.background = ggplot2::element_rect(fill = "#FFFFFF"),
            panel.border = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_line(colour = "#e5e5e5",
                                                     linewidth = 0.5),
            panel.grid.minor = ggplot2::element_blank(),
            panel.spacing = ggplot2::unit(1, "lines"),
            plot.caption = ggplot2::element_text(
                margin = ggplot2::margin(4, 0, 0, 0, "pt"),
                size = ggplot2::rel(0.80)
            ),
            plot.caption.position = "plot",
            plot.margin = ggplot2::margin(6, 6, 6, 6, "pt"),
            plot.subtitle = ggplot2::element_text(
                margin = ggplot2::margin(4, 0, 0, 0, "pt"),
                size = ggplot2::rel(1.1)
            ),
            plot.title = ggplot2::element_text(face = "bold",
                                               size = ggplot2::rel(1.5)),
            plot.title.position = "plot",
            strip.background =
                ggplot2::element_rect(fill = strip_bg_colour),
            strip.text = ggplot2::element_text(
                colour = strip_text_colour,
                face = "bold",
                hjust = 0,
                vjust = 0,
                size = ggplot2::rel(1.0)
            )
        )
}
