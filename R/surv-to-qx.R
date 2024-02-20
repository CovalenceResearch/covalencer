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

#' Convert estimates from a `survfit` object to a tibble (dataframe)
#'
#' @description
#' This function takes a `survfit` object and extracts time estimates, together
#' with the number of events and number of risk. Based on the user specification
#' of time units - either `days` or `years` - the function then creates a full
#' sequence of time and event/risk number estimates from 0 to a user-specified
#' maximum age.
#'
#' @details
#' Between time points at which an event takes place, the number at
#' risk is retained from the preceding time point with an event, while the
#' number of events is zero. Beyond the last observed time, the number at risk
#' from the last observed time is retained, with no more events taking place.
#'
#' @param obj_survfit A `survfit` object generated using `survival::survfit()`
#' or `survival::survfit0()`.
#' @param time_in Unit in which time is reported. Either `days` or `years`.
#' @param age_max_years Maximum age, in years, to which data should be
#' extrapolated. Defaults to 120.
#' @returns
#' A `tibble` with `age_max_years + 1` rows and three columns. The first column,
#' `year`, gives the year, the second, `n_events` the number events (deaths) in
#' this year, and the third, `n_start`, the number at risk at the start of the
#' year.
#' @export
convert_survfit2tibble <- function(obj_survfit,
                                   time_in = c("days", "years"),
                                   age_max_years = 120L) {
  # Argument checks -----------------------------------------------------------
  ## Time unit
  time_unit <- rlang::arg_match(time_in, values = c("days", "years"))

  ## Maximum age
  rlang::check_required(age_max_years)

  if (!base::is.numeric(age_max_years)) {
    cli::cli_abort(
      message = "Please supply a numeric value for {.code age_max_years}."
    )
  }

  # Projection time -----------------------------------------------------------
  max_time <- dplyr::if_else(
    time_unit == "days",
    as.integer(base::ceiling(365.25 * age_max_years)),
    age_max_years
  )

  # Convert to tibble ---------------------------------------------------------
  tibble::tibble(
    time = obj_survfit$time,
    n_risk = obj_survfit$n.risk,
    n_events = obj_survfit$n.event
  ) |>
    tidyr::complete(
      time = tidyr::full_seq(c(time, max_time), period = 1),
      fill = list(n_risk = NA_integer_, n_events = 0)
    ) |>
    tidyr::fill(n_risk, .direction = "updown") |>
    dplyr::distinct() |>
    dplyr::mutate(
      time_unit = .env$time_unit,
      year_precise = dplyr::if_else(
        time_unit == "days", time / 365.25, time
      ),
      year = base::floor(year_precise)
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      n_events = base::sum(n_events),
      n_start = base::max(n_risk)
    )
}
