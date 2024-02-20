#' Calculate qx column from a `survfit` object
#'
#' This function takes an object resulting from a call to `survival::survfit()`
#' or `survival::survfit0()` object and calculates a single-year lifetable for
#' each stratum.
#'
#' @param obj_survfit An object returned from a call to `survival::survfit()` or
#' `survival::survfit0()`.
#' @returns
#' A `tibble` with five columns - `stratum` first, which reports the stratum
#' from the `survfit` object to which the estimate belongs. The second, third,
#' and fourth columns return standard life table estimates, including
#' person-years (`py`), mortality rates (`mx`), and the probability of death
#' (`qx`).
#' @export
calc_qx_from_surv <- function(obj_survfit) {
  # Argument checks -----------------------------------------------------------
  rlang::check_required(obj_survfit)

  class_obj <- base::class(obj_survfit)
  if (class_obj != "survfit") {
    cli::cli_abort(
      message = "Please supply a {.code survfit},
      not {.code {class_obj}} object."
    )
  }

  # Calculate life table for each stratum -------------------------------------
  names(obj_survfit$strata) |>
    purrr::set_names() |>
    purrr::map(\(i) make_lt(convert_survfit2tibble(obj_survfit[i]))) |>
    dplyr::bind_rows(.id = "stratum")
}
