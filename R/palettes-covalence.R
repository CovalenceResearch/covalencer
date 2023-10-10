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
#' covalence_palette("main")
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
