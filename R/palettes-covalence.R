#' Covalence colors
#'
#' @description
#' The Covalence brand colors, including the primary colors
#' ('lightblue', 'darkblue', 'teal', and 'grey') and the accent colors
#' ('red', 'green', 'purple', and 'orange').
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
    { return(covalence_cols) }

    covalence_cols[dots]
}
