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
        `cov_lightblue` = "#0099cc",
        `cov_darkblue`  = "#326aa0",
        `cov_teal`      = "#154754",
        `cov_gray`      = "#848088",
        `cov_red`       = "#c30026",
        `cov_green`     = "#008040",
        `cov_purple`    = "#7a0d66",
        `cov_orange`    = "#ffb300"
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
#' The [12-bit rainbow palette](https://iamkate.com/data/12-bit-rainbow/) was developed by Kate Morley and released under the terms of the Creative Commons CC0 1.0 Universal Legal Code.
#'
#' @param ... Arguments to be passed on.
#' @param palette Palette to use. One of 'complete', 'main', 'accent', or 'rainbow_12_bit'.
#'
#' @return A named vector of brand colors.
#' @export
#'
#' @examples
#' require(scales)
#' show_col(covalence_palette(palette = "discrete_main"))
covalence_palette <-
    function(...,
             palette = c(
                 "discrete_complete",
                 "discrete_main",
                 "discrete_accent",
                 "discrete_rainbow_12_bit",
                 "diverging_OrTe"
             )) {
        # Check arguments
        pal <-
            rlang::arg_match(
                palette,
                values = c(
                    "discrete_complete",
                    "discrete_main",
                    "discrete_accent",
                    "discrete_rainbow_12_bit",
                    "diverging_OrTe"
                )
            )

        # Palette
        covalence_pals <- list(
            `discrete_complete` = covalence_colors(),
            `discrete_main` = covalence_colors("cov_lightblue",
                                               "cov_darkblue",
                                               "cov_teal",
                                               "cov_gray"),
            `discrete_accent` = covalence_colors("cov_red",
                                                 "cov_green",
                                                 "cov_orange",
                                                 "cov_purple"),
            `discrete_rainbow_12_bit` = c(
                "#881177",
                "#aa3355",
                "#cc6666",
                "#ee9944",
                "#eedd00",
                "#99dd55",
                "#44dd88",
                "#22ccbb",
                "#00bbcc",
                "#0099cc",
                "#3366bb",
                "#663399"
            ),
            `diverging_OrTe` = c(
                covalence_colors("cov_orange"),
                "#A18721",
                covalence_colors("cov_teal")
            )
        )

        covalence_pals[[pal]]
    }

#' Function factory to generate palette
#'
#' @description
#' This function generates a second function, for use in the 'ggplot' scales.
#'
#' @details
#' This function is slightly modified from a 2022 [blog post](https://www.jumpingrivers.com/blog/custom-colour-palettes-for-ggplot2/) by Nicola Rennie.
#' @param n Number of colors wanted from palette.
#' @param type Type of palette. Either 'discrete' or 'continuous'.
#' @param reverse Should the palette be reversed? Either 'TRUE' or 'FALSE'.
#' @inheritParams covalence_palette
#'
#' @return A palette-generating function.
generate_pal <- function(palette,
                         n,
                         type = "discrete",
                         reverse = FALSE) {
    # Check arguments
    arg_pal  <- rlang::arg_match(
        palette,
        values = c(
            "discrete_complete",
            "discrete_main",
            "discrete_accent",
            "discrete_rainbow_12_bit",
            "diverging_OrTe"
        )
    )
    arg_type  <- rlang::arg_match(type,
                                  values = c("discrete", "continuous"))
    if (!is.logical(reverse)) {
        cli::cli_abort("{.var reverse} must be TRUE or FALSE.")
    }

    pal <- covalence_palette(palette = arg_pal)
    if (rlang::is_missing(n))
        n <- length(pal)

    out <- switch(
        arg_type,
        discrete = unname(pal)[1:n],
        continuous = grDevices::colorRampPalette(pal)(n)
    )

    structure(out, name = arg_pal, class = "palette")
}

#' Covalence color and fill scales for ggplot2
#'
#' @description
#' The `scale_*_covalence_*` functions provide discrete (`_d`) and continuous
#' (`_c`) scales of Covalence colors for use in [ggplot2] plots.
#'
#' @details
#' These functions are inspired by a 2022 [blog post](https://www.jumpingrivers.com/blog/custom-colour-palettes-for-ggplot2/) by Nicola Rennie.
#'
#' @inheritParams generate_pal
#'
#' @return Discrete/continuous color/fill scales for ggplot2.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(data = diamonds, aes(x = carat, y = price)) +
#'   geom_point(aes(color = cut), alpha = 0.4) +
#'   scale_color_covalence_d(palette = "discrete_complete") +
#'   theme_covalence()
#'
#' ggplot(data = diamonds, aes(x = carat, y = price)) +
#'   geom_point(aes(color = x), alpha = 0.4) +
#'   scale_colour_covalence_c(palette = "discrete_complete") +
#'   theme_covalence()
#'
#' ggplot(data = diamonds, aes(x = price)) +
#'   geom_histogram(aes(fill = cut)) +
#'   scale_fill_covalence_d(
#'     palette = "discrete_rainbow_12_bit",
#'     reverse = TRUE
#'   ) +
#'   theme_covalence()
#'
scale_colour_covalence_d <- function(palette = "discrete_complete",
                                     reverse = FALSE) {
    ggplot2::scale_colour_manual(values = generate_pal(
        palette = palette,
        reverse = reverse,
        type = "discrete"
    ))
}

#' @rdname scale_colour_covalence_d
#' @export
scale_color_covalence_d <- scale_colour_covalence_d

#' @rdname scale_colour_covalence_d
#' @export
scale_fill_covalence_d <- function(palette = "discrete_complete",
                                   reverse = FALSE) {
    ggplot2::scale_fill_manual(values = generate_pal(
        palette = palette,
        reverse = reverse,
        type = "discrete"
    ))
}

#' @rdname scale_colour_covalence_d
#' @export
scale_colour_covalence_c <- function(palette = "discrete_complete",
                                     reverse = FALSE) {
    ggplot2::scale_colour_gradientn(colors = generate_pal(
        palette = palette,
        reverse = reverse,
        type = "continuous"
    ))
}

#' @rdname scale_colour_covalence_d
#' @export
scale_color_covalence_c <- scale_colour_covalence_c

#' @rdname scale_colour_covalence_d
#' @export
scale_fill_covalence_c <- function(palette = "discrete_complete",
                                   reverse = FALSE) {
    ggplot2::scale_fill_gradientn(colors = generate_pal(
        palette = palette,
        reverse = reverse,
        type = "continuous"
    ))
}

scale_color_covalence_diverging <- function(palette = "diverging_OrTe",
                                      reverse = FALSE,
                                      midpoint = 0) {
    if (reverse) {
        pal <- rev(covalence_palette(palette = palette))
    } else {
        pal <- covalence_palette(palette = palette)
    }
    pal_colors <- covalence_palette(palette = palette)

    ggplot2::scale_color_gradient2(
        low = pal_colors[1],
        mid = pal_colors[2],
        high = pal_colors[3],
        midpoint = midpoint,
        space = "Lab",
        na.value = "#cccccc",
        #grey80
        guide = "colourbar",
        aesthetics = "colour"
    )
}
