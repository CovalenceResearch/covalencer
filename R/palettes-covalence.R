#' Covalence colors
#'
#' @description
#' The Covalence brand colors, including the primary colors
#' ('lightblue', 'darkblue', 'teal', and 'grey') and the accent colors
#' ('red', 'green', 'purple', and 'orange').
#'
#' @details
#' This function is taken from a 2022 [blog post](https://meghan.rbind.io/blog/2022-10-11-creating-custom-color-palettes-with-ggplot2/#defining-custom-colors-and-palettes) by Meghan Hall.
#'
#' @param ... Access specific colors
#'
#' @return A named vector of brand colors
#' @export
#'
#' @examples
#' covalence_colors("darkblue")
covalence_colors <- function(...) {
    covalence_cols <- c(
        `lightblue` = "#249bc9",
        `darkblue`  = "#326aa0",
        `teal`      = "#154754",
        `gray`      = "#848088",
        `red`       = "#c30026",
        `green`     = "#008040",
        `purple`    = "#7a0d66",
        `orange`    = "#ffb300"
    )


    dots <- c(...)

    if (is.null(dots))
    {
        return(covalence_cols)
    }

    covalence_cols[dots]
}

#' Covalence palette
#'
#' @description
#' The various Covalence palettes:
#'   * The _complete_ palette that contains all brand colors.
#'   * The _main_ palette that contains the main, mostly blue-ish, brand colors.
#'   * The _accent_ palette that contains additional, more prominent colors.
#'
#' @details
#' This function is slightly modified from a 2022 [blog post](https://meghan.rbind.io/blog/2022-10-11-creating-custom-color-palettes-with-ggplot2/#defining-custom-colors-and-palettes) by Meghan Hall.
#'
#' @param palette Palette to use. One of 'complete', 'main', or 'accent'.
#' @param ... Dots.
#'
#' @return A named vector of brand colors.
#' @export
#'
#' @examples
#' scales::show_col(covalence_palette("main"))
covalence_palette <-
    function(palette = c("complete", "main", "accent"),
             ...) {
        # Check arguments
        rlang::arg_match(palette, values = c("complete", "main", "accent"))

        # Palette
        covalence_pals <- list(
            `complete` = covalence_colors(),
            `main` = covalence_colors("lightblue", "darkblue", "teal", "gray"),
            `accent` = covalence_colors("red", "green", "orange", "purple")
        )

        covalence_pals[[palette]]
    }

#' Function factory to generate palette
#'
#' @description
#' This function generates a second function, for use in the 'ggplot' scales.
#'
#' @details
#' This function is slightly modified from a 2022 [blog post](https://meghan.rbind.io/blog/2022-10-11-creating-custom-color-palettes-with-ggplot2/#defining-custom-colors-and-palettes) by Meghan Hall.
#' @inheritParams covalence_palette
#' @param type Type of palette. Either 'discrete' or 'continuous'.
#' @param reverse Should the palette be reversed? Either 'TRUE' or 'FALSE'.
#' @param ... Dots.
#'
#' @return A palette-generating function.
generate_pal <- function(palette = "main",
                         type = "discrete",
                         reverse = FALSE,
                         ...) {
    # Check arguments
    rlang::arg_match(palette, values = c("complete", "main", "accent"))
    rlang::arg_match(type, values = c("discrete", "continuous"))
    if (!is.logical(reverse)) {
        cli::cli_abort("{.var reverse} must be TRUE or FALSE.")
    }

    # Generate palette
    function(n) {
        if (n > length(covalence_palette(palette))) {
            cli::cli_alert_warning("Not enough colors in the chosen palette.")
        }

        pal <- covalence_palette(palette)
        if (reverse)
            pal <- rev(pal)

        if (type == "discrete") {
            list_colors <- unname(unlist(pal))[1:n]
        } else {
            grDevices::colorRampPalette(pal, ...)
        }
    }
}

#' Covalence color and fill scale for ggplot2
#'
#' @description
#' The `scale_*_covalence_*` functions provide discrete (`_d`) and continuous
#' (`_c`) scales of Covalence colors for use in [ggplot2] plots.
#'
#' @details
#' This function is modified from a 2022 [blog post](https://meghan.rbind.io/blog/2022-10-11-creating-custom-color-palettes-with-ggplot2/#defining-custom-colors-and-palettes) by Meghan Hall.
#'
#' @inheritParams generate_pal
#'
#' @return Discrete/continuous color/fill scales for ggplot2.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(data = msleep, aes(x = brainwt, y = bodywt)) +
#'   geom_point(aes(color = vore)) +
#'   scale_colour_covalence_d()
scale_colour_covalence_d <- function(palette = "main",
                                     reverse = FALSE,
                                     ...) {
    ggplot2::discrete_scale(
        "color", "covalence_d",
        generate_pal(palette = palette, type = "discrete", reverse = reverse),
        ...
    )
}

#' @rdname scale_colour_covalence_d
scale_color_covalence_d <- scale_colour_covalence_d
