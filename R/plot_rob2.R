#' Plot summary of RoB2 assessments
#'
#' @description
#' The [RoB2 tool](https://www.riskofbias.info/welcome/rob-2-0-tool),
#' developed by the Cochrane Collaboration, allows assessing the
#' risk of bias in randomized trials. In addition to a study-level traffic light
#' plot, a typical output of RoB2 assessments is a summary of the proportion
#' of studies with a specific bias risk, per domain. This latter type of plot is
#' created by `plot_rob2_summary`, which requires summarized data (i.e., no
#' data synthesis or calculations are performed by the function).
#'
#' @param data A data.frame.
#' @param domain Unquoted column name for the bias domains.
#' @param judgement Unquoted column name for the risk of bias judgement.
#' @param study_share Unquoted column name for proportion \[0, 1\] of studies with
#'  a specific bias risk per domain. Values in this column must all be numeric.
#' @param domain_check_words A character vector of check words the presence of
#'  which is checked by the function, and a warning displayed if they're missing.
#'  This is useful to see if perhaps a domain had been missed.
#'
#' @return A ggplot2 object
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # These data are certainly not reflective of best practice!
#' data_rob2 <- data.frame(
#'  domain = rep(c("Overall bias",
#'    "Bias due to deviations from intended interventions", "Missing data"),
#'    each = 3),
#'  judgement = rep(c("Low", "Some concerns", "High risk of bias"), times = 3),
#'  study_share = c(0.4, 0.4, 0.2, 0.1, 0.85, 0.05, 0.2, 0.5, 0.3))
plot_rob2_summary <- function(data,
                              domain = domain,
                              judgement = judgement,
                              study_share = study_share,
                              domain_check_words = c("overall",
                                                     "random",
                                                     "deviation",
                                                     "missing",
                                                     "measure",
                                                     "select")) {
    col_domain      <- rlang::as_string(rlang::ensym(domain))
    col_judgement   <- rlang::as_string(rlang::ensym(judgement))
    col_study_share <- rlang::as_string(rlang::ensym(study_share))

    col_names   <- c(col_domain, col_judgement, col_study_share)
    col_missing <- setdiff(col_names, names(data))

    # Argument checks
    ## Data
    rlang::check_required(domain)
    rlang::check_required(judgement)
    rlang::check_required(study_share)

    if (length(col_missing) > 0) {
        cli::cli_abort(
            c(
                "{.var {rlang::caller_arg(col_missing)}} is not a column in {.var {rlang::caller_arg(data)}}."
            ),
            call = rlang::caller_env()
        )
    }

    ## Input types
    if (!is.numeric(data[[deparse(substitute(study_share))]])) {
        cli::cli_abort("{.var study_share} must be numeric.")
    }

    ## Domain checks
    domain_values <-
        tolower(unique(data[,
                            deparse(substitute(domain)),
                            drop = TRUE]))
    domain_missing <-
        domain_check_words[which(!(tolower(domain_check_words) %in%
                                       domain_values))]

    if (length(domain_missing) > 0) {
        cli::cli_alert_warning("No domain labels contain: {domain_missing}.")
    }

    # Use consistent terminology for domains
    data <- data |>
        dplyr::mutate(
            domain = stringr::str_to_lower(domain),
            judgement = stringr::str_to_lower(judgement)
        ) |>
        dplyr::mutate(
            plot_domain = dplyr::case_when(
                stringr::str_detect(.data$domain, "overall") ~
                    "Overall risk of bias",
                stringr::str_detect(.data$domain, "allocation|random") ~
                    "Bias arising from randomisation process",
                stringr::str_detect(.data$domain, "deviation|intended") ~ "
            Bias due to deviations from intended interventions",
            stringr::str_detect(.data$domain, "measure") ~
                "Bias in outcome measurement",
            stringr::str_detect(.data$domain, "miss") ~
                "Bias due to missing outcome data",
            stringr::str_detect(.data$domain, "report|select") ~
                "Bias in selection of reported results"
            )
        ) |>
        dplyr::mutate(
            plot_judgement = dplyr::case_when(
                stringr::str_detect(.data$judgement, stringr::coll("high")) ~
                    "High risk of bias",
                stringr::str_detect(.data$judgement, stringr::coll("low")) ~
                    "Low risk of bias",
                stringr::str_detect(.data$judgement, stringr::coll("some")) ~
                    "Some concerns"
            )
        )

    # Plot
    p <- ggplot2::ggplot(
        data = data,
        mapping = ggplot2::aes(
            x = .data$study_share,
            y = .data$plot_domain,
            fill = .data$plot_judgement
        )
    ) +
        ggplot2::geom_bar(position = "stack",
                          stat = "identity",
                          color = "#FFFFFF") +
        theme_covalence()

    p
}
