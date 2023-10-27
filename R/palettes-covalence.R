#' Covalence colours
#'
#' @description
#' The Covalence brand colours, including the primary colours
#' ('lightblue', 'darkblue', 'teal', and 'grey') and the accent colours
#' ('red', 'green', 'purple', and 'orange'). Each colour name is prefixed with
#' "cov_" to avoid confusion with the default R colour names.
#'
#' @details
#' This function is taken from a 2022 [blog post](https://meghan.rbind.io/blog/2022-10-11-creating-custom-color-palettes-with-ggplot2/#defining-custom-colors-and-palettes) by Meghan Hall.
#'
#' @param ... Access specific colours
#'
#' @return A named vector of brand colours
#' @export
#'
#' @examples
#' covalence_colours("darkblue")
covalence_colours <- function(...) {
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
#'   * The _complete_ palette that contains all brand colours.
#'   * The _main_ palette that contains the main, mostly blue-ish, brand colours.
#'   * The _accent_ palette that contains additional, more prominent colours.
#'
#' @details
#' This function is slightly modified from a 2022 [blog post](https://meghan.rbind.io/blog/2022-10-11-creating-custom-color-palettes-with-ggplot2/#defining-custom-colors-and-palettes) by Meghan Hall.
#' The [12-bit rainbow palette](https://iamkate.com/data/12-bit-rainbow/) was developed by Kate Morley and released under the terms of the Creative Commons CC0 1.0 Universal Legal Code.
#'
#' @param ... Arguments to be passed on.
#' @param palette Palette to use. One of 'complete', 'main', 'accent', or 'rainbow_12_bit'.
#'
#' @return A named vector of brand colours.
#' @export
#'
#' @examples
#' require(scales)
#' show_col(covalence_palette(palette = "main"))
covalence_palette <-
    function(...,
             palette = c(
                 "complete",
                 "main",
                 "accent",
                 "rainbow_12_bit",
                 "orange_teal",
                 "orange_purple",
                 "orange_darkblue"
             )) {
        # Check arguments
        pal <-
            rlang::arg_match(
                palette,
                values = c(
                    "complete",
                    "main",
                    "accent",
                    "rainbow_12_bit",
                    "orange_teal",
                    "orange_purple",
                    "orange_darkblue"
                )
            )

        # Palette
        covalence_pals <- list(
            `complete` = covalence_colours(),
            `main` = covalence_colours("cov_lightblue",
                                                "cov_darkblue",
                                                "cov_teal",
                                                "cov_gray"),
            `accent` = covalence_colours("cov_red",
                                                  "cov_green",
                                                  "cov_orange",
                                                  "cov_purple"),
            `rainbow_12_bit` = c(
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
            `orange_teal` = c(
                covalence_colours("cov_orange"),
                "#A18721",
                covalence_colours("cov_teal")
            ),
            `orange_purple` = c(
                covalence_colours("cov_orange"),
                "#AF4F3D",
                covalence_colours("cov_purple")
            ),
            `orange_darkblue` = c(
                covalence_colours("cov_orange"),
                "#AD9540",
                covalence_colours("cov_darkblue")
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
#' @param n Number of colours wanted from palette.
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
            "complete",
            "main",
            "accent",
            "rainbow_12_bit"
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

#' Covalence discrete colour and fill scales for ggplot2
#'
#' @description
#' The `scale_*_covalence_discrete` functions provide discrete scales of
#' Covalence colours for use in [ggplot2] plots.
#'
#' @details
#' These functions are inspired by a 2022 [blog post](https://www.jumpingrivers.com/blog/custom-colour-palettes-for-ggplot2/) by Nicola Rennie.
#'
#' @inheritParams generate_pal
#'
#' @return Discrete colour/fill scales for ggplot2.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(data = diamonds, aes(x = carat, y = price)) +
#'   geom_point(aes(colour = cut), alpha = 0.4) +
#'   scale_colour_covalence_discrete(palette = "complete") +
#'   theme_covalence()
#'
#' ggplot(data = diamonds, aes(x = carat, y = price)) +
#'   geom_point(aes(colour = x), alpha = 0.4) +
#'   scale_colour_covalence_c(palette = "complete") +
#'   theme_covalence()
#'
#' ggplot(data = diamonds, aes(x = price)) +
#'   geom_histogram(aes(fill = cut)) +
#'   scale_fill_covalence_discrete(
#'     palette = "rainbow_12_bit",
#'     reverse = TRUE
#'   ) +
#'   theme_covalence()
scale_colour_covalence_discrete <-
    function(palette = "complete",
             reverse = FALSE) {
        ggplot2::scale_colour_manual(values = generate_pal(
            palette = palette,
            reverse = reverse,
            type = "discrete"
        ))
    }

#' @rdname scale_colour_covalence_discrete
#' @export
scale_color_covalence_discrete <- scale_colour_covalence_discrete

#' @rdname scale_colour_covalence_discrete
#' @export
scale_fill_covalence_discrete <-
    function(palette = "complete",
             reverse = FALSE) {
        ggplot2::scale_fill_manual(values = generate_pal(
            palette = palette,
            reverse = reverse,
            type = "discrete"
        ))
    }

#' @rdname scale_colour_covalence_discrete
#' @export
scale_colour_covalence_c <- function(palette = "complete",
                                     reverse = FALSE) {
    ggplot2::scale_colour_gradientn(colours = generate_pal(
        palette = palette,
        reverse = reverse,
        type = "continuous"
    ))
}

#' @rdname scale_colour_covalence_discrete
#' @export
scale_color_covalence_c <- scale_colour_covalence_c

#' @rdname scale_colour_covalence_discrete
#' @export
scale_fill_covalence_c <- function(palette = "complete",
                                   reverse = FALSE) {
    ggplot2::scale_fill_gradientn(colours = generate_pal(
        palette = palette,
        reverse = reverse,
        type = "continuous"
    ))
}


#' Covalence diverging colour and fill scales for ggplot2
#'
#' @description
#' The `scale_*_covalence_diverging` functions provide diverging
#' scales of Covalence colours for use in [ggplot2] plots.
#'
#' @details
#' These functions are inspired by a 2022 [blog post](https://rfortherestofus.com/2022/02/data-viz-org-branding/) by Cara Thompson.
#'
#' @param palette One of the diverging colour palettes (`orange_teal`,
#'   `orange_purple`, or `orange_darkblue`). Default is `orange_teal`).
#' @param na_colour Colour for missing values. Default is "#cccccc" (grey80).
#' @param midpoint Numeric value for midpoint. Default is 0.
#' @inheritParams generate_pal
#'
#' @return Diverging colour/fill scales for ggplot2.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
# ggplot(data = diamonds, aes(x = carat, y = depth)) +
#   geom_point(aes(colour = log(price)), alpha = 0.1) +
#   scale_colour_covalence_diverging(palette = "orange_darkblue",
#                                   midpoint = 8) +
#   theme_covalence()
scale_colour_covalence_diverging <-
    function(palette = "orange_teal",
             reverse = FALSE,
             midpoint = 0,
             na_colour = "#cccccc") {
        if (!is.logical(reverse)) {
            cli::cli_abort("{.var reverse} must be TRUE or FALSE.")
        }

        if (!is.numeric(midpoint)) {
            cli::cli_abort("{.var midpoint} must be numeric.")
        }

        palette <-
            rlang::arg_match(palette,
                             values = c("orange_teal",
                                        "orange_purple",
                                        "orange_darkblue"))


        if (reverse) {
            pal_colours <- rev(covalence_palette(palette = palette))
        } else {
            pal_colours <- covalence_palette(palette = palette)
        }

        ggplot2::scale_colour_gradient2(
            low = pal_colours[1],
            mid = pal_colours[2],
            high = pal_colours[3],
            midpoint = midpoint,
            space = "Lab",
            na.value = na_colour,
            #grey80
            guide = "colourbar",
            aesthetics = "colour"
        )
    }

#' @rdname scale_colour_covalence_diverging
#' @export
scale_color_covalence_diverging <- scale_colour_covalence_diverging

#' @rdname scale_colour_covalence_diverging
#' @export
scale_fill_covalence_diverging <-
    function(palette = "orange_teal",
             reverse = FALSE,
             midpoint = 0,
             na_colour = "#cccccc") {
        if (!is.logical(reverse)) {
            cli::cli_abort("{.var reverse} must be TRUE or FALSE.")
        }

        if (!is.numeric(midpoint)) {
            cli::cli_abort("{.var midpoint} must be numeric.")
        }

        palette <-
            rlang::arg_match(palette,
                             values = c("orange_teal",
                                        "orange_purple",
                                        "orange_darkblue"))


        if (reverse) {
            pal_colours <- rev(covalence_palette(palette = palette))
        } else {
            pal_colours <- covalence_palette(palette = palette)
        }

        ggplot2::scale_fill_gradient2(
            low = pal_colours[1],
            mid = pal_colours[2],
            high = pal_colours[3],
            midpoint = midpoint,
            space = "Lab",
            na.value = na_colour,
            guide = "colourbar",
            aesthetics = "fill"
        )
    }
