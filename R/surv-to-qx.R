#' Calculate qx column from a `survfit` object
#'
#' This function takes an object resulting from a call to `survival::survfit()`
#' or `survival::survfit0()` object and calculates a single-year lifetable for
#' each stratum.
#'
#' @param obj_survfit An object returned from a call to `survival::survfit()` or
#' `survival::survfit0()`.
#' @param ... Additional arguments passed to `convert_survfit2tibble()`.
#'
#' @returns
#' A `tibble` with five columns - `stratum` first, which reports the stratum
#' from the `survfit` object to which the estimate belongs. The second, third,
#' and fourth columns return standard life table estimates, including
#' person-years (`py`), mortality rates (`mx`), and the probability of death
#' (`qx`).
#'
#' @export
#'
#' @examples
#' library(survival)
#'
#' obj_surv <- Surv(time = cancer$time, event = cancer$status)
#' obj_survfit <- survfit(obj_surv ~ sex, data = cancer)
#' calc_qx_from_surv(obj_survfit, time_in = "days", age_max_years = 90L)
calc_qx_from_surv <- function(obj_survfit, ...) {
  # Argument checks -----------------------------------------------------------
  rlang::check_required(obj_survfit)

  class_obj <- base::class(obj_survfit)
  if (class_obj != "survfit") {
    cli::cli_abort(
      message = "Please supply a {.code survfit},
      not {.code {class_obj}} object."
    )
  }

  # Check if formula contains a stratum
  names_strata <- names(obj_survfit$strata)

  if (base::is.null(names_strata)) {
    make_lt(convert_survfit2tibble(obj_survfit, ...)) |>
      dplyr::mutate(stratum = "all")
  } else {
    # Calculate life table for each stratum -----------------------------------
    names_strata |>
      purrr::set_names() |>
      purrr::map(\(i) make_lt(convert_survfit2tibble(obj_survfit[i], ...))) |>
      dplyr::bind_rows(.id = "stratum")
  }
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
#'
#' @returns
#' A `tibble` with `age_max_years + 1` rows and three columns. The first column,
#' `year`, gives the year, the second, `n_events` the number events (deaths) in
#' this year, and the third, `n_start`, the number at risk at the start of the
#' year.
#' @importFrom rlang .data
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

  # Ensure an inclusion of 0
  obj_survfit0 <- survival::survfit0(obj_survfit, start.time = 0)

  # Projection time -----------------------------------------------------------
  max_time <- dplyr::if_else(
    time_unit == "days",
    as.integer(base::ceiling(365.25 * age_max_years)),
    age_max_years
  )

  # Convert to tibble ---------------------------------------------------------
  tibble::tibble(
    time = obj_survfit0$time,
    n_risk = obj_survfit0$n.risk,
    n_events = obj_survfit0$n.event
  ) |>
    tidyr::complete(
      time = tidyr::full_seq(c(.data$time, max_time), period = 1),
      fill = list(.n_risk = NA_integer_, n_events = 0)
    ) |>
    tidyr::fill(.data$n_risk, .direction = "updown") |>
    dplyr::distinct() |>
    dplyr::mutate(
      time_unit = time_unit,
      year_precise = dplyr::if_else(
        .data$time_unit == "days", .data$time / 365.25, .data$time
      ),
      year = base::floor(.data$year_precise)
    ) |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(
      n_events = base::sum(.data$n_events),
      n_start = base::max(.data$n_risk)
    ) |>
    dplyr::filter(.data$year <= age_max_years)
}

#' Make a single-year life table from numbers of events and at risk
#'
#' @description description
#' Based on the output from `convert_survfit2tibble()`, make a tibble containing
#' `qx` (probabilities of dying before reaching age `x+1`) by year.
#'
#' @details
#' The person-years (`py`) lived in each year are calculated as the mean of the
#' cohort at age `x` and age `x+1`, i.e., deaths are assumed to occur,
#' on average, mid-way through the single-year interval.
#' The mortality rate, `mx`, is calculated as the number of
#' events (deaths) per person-year.
#' The probability of death between ages `x` and `x+1` is calculated as
#' \eqn{q_{x} = 1 - \exp{-n*m_{x}}}, where `n=1` as intervals are single-year.
#' This implies the assumption of constant mortality in the 1-year interval.
#'
#' @param data A tibble produced by `convert_survfit2tibble()`.
#'
#' @returns
#' A `tibble` with four columns, the first being `year`. The second column,
#' `py`, gives the person-years spent in year `x`.The third column, `mx`, gives
#' the mortality rate in year `x`; the last, `qx`, the probability of dying
#' between years `x` and `x+1`.
#'
#' @importFrom rlang .data
make_lt <- function(data) {
  # Argument checks -----------------------------------------------------------
  rlang::check_required(data)

  if (!tibble::is_tibble(data)) {
    cli::cli_abort(message = "Please supply a tibble.")
  }

  # Create lifetable ----------------------------------------------------------
  data |>
    dplyr::mutate(
      py = (dplyr::lead(.data$n_start) + .data$n_start) * 0.5,
      mx = .data$n_events / .data$py,
      qx = 1 - exp(-1 * .data$mx)
    ) |>
    dplyr::mutate(qx = dplyr::if_else(
      .data$year == base::max(.data$year), 1, .data$qx
    )) |>
    dplyr::distinct(.data$year, data$py, .data$mx, .data$qx)
}
