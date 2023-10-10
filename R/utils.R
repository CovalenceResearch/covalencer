#' Check if a string is a hex color
#'
#' @param color Color string to check
#'
#' @return A Boolean value.
#' @noRd
#'
#' @examples
#' is_hex("#000000") #TRUE
#' is_hex("#0000")   #FALSE
is_hex <- function(color) {grepl("^\\#[0-6a-fA-F]{6}$", color)}
