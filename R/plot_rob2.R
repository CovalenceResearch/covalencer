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
        mapping = ggplot2::aes(x = study_share,
                               y = plot_domain,
                               fill = plot_judgement)
    ) +
        ggplot2::geom_bar(position = "stack",
                          stat = "identity",
                          color = "#FFFFFF") +
        theme_covalence()

    p
}
