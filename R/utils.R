#' Check if a string is a hex color
#'
#' @param color Color string to check
#'
#' @return A Boolean value.
#' @noRd
#'
#' @examples
#' is_hex("#000000") # TRUE
#' is_hex("#0000") # FALSE
#' is_hex("#09c") # TRUE
is_hex <- function(color) {
  xor(
    grepl("^\\#[0-9a-fA-F]{6}$", color),
    grepl("^\\#[0-9a-fA-F]{3}$", color)
  )
}

#' Check if a string is a color
#'
#' @param color Color string to check
#'
#' @return A Boolean value.
#' @noRd
#'
#' @examples
#' check_color("red") # TRUE
#' check_color("#249bc9") # TRUE
#' check_color("#red") # FALSE
check_color <- function(color) {
  is_hex(color) | color %in% grDevices::colors()
}
