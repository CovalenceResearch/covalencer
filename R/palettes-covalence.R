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
        `lightblue` = "#0099cc",
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
#' The [12-bit rainbow palette](https://iamkate.com/data/12-bit-rainbow/) was developed by Kate Morley and released under the terms of the Creative Commons CC0 1.0 Universal Legal Code.
#'
#' @param ... Arguments to be passed on.
#' @param palette Palette to use. One of 'complete', 'main', or 'accent'.
#'
#' @return A named vector of brand colors.
#' @export
#'
#' @examples
#' require(scales)
#' show_col(covalence_palette(palette = "main"))
covalence_palette <-
    function(...,
             palette = c("complete", "main", "accent")) {
        # Check arguments
        pal <-
            rlang::arg_match(palette, values = c("complete", "main", "accent"))

        # Palette
        covalence_pals <- list(
            `complete` = covalence_colors(),
            `main` = covalence_colors("lightblue", "darkblue", "teal", "gray"),
            `accent` = covalence_colors("red", "green", "orange", "purple"),
            `rainbow_12_bit` = c("#817",
                                 "#a35",
                                 "#c66",
                                 "#e94",
                                 "#ed0",
                                 "#9d5",
                                 "#4d8",
                                 "#2cb",
                                 "#0bc",
                                 "#09c",
                                 "#36b",
                                 "#639")
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
    arg_pal  <- rlang::arg_match(palette,
                                  values = c("complete", "main", "accent"))
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
#'   scale_color_covalence_d(palette = "complete") +
#'   theme_covalence()
#'
#' ggplot(data = diamonds, aes(x = carat, y = price)) +
#'   geom_point(aes(color = x), alpha = 0.4) +
#'   scale_colour_covalence_c(palette = "complete") +
#'   theme_covalence()
#'
#' ggplot(data = diamonds, aes(x = price)) +
#'   geom_histogram(aes(fill = cut)) +
#'   scale_fill_covalence_d(palette = "complete", reverse = TRUE) +
#'   theme_covalence()
#'
scale_colour_covalence_d <- function(palette = "complete",
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
scale_fill_covalence_d <- function(palette = "complete",
                                   reverse = FALSE) {
    ggplot2::scale_fill_manual(values = generate_pal(
        palette = palette,
        reverse = reverse,
        type = "discrete"
    ))
}

#' @rdname scale_colour_covalence_d
#' @export
scale_colour_covalence_c <- function(palette = "complete",
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
scale_fill_covalence_c <- function(palette = "complete",
                                   reverse = FALSE) {
    ggplot2::scale_fill_gradientn(colors = generate_pal(
        palette = palette,
        reverse = reverse,
        type = "continuous"
    ))
}
