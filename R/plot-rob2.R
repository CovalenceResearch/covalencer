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
#'
#' @return A ggplot2 object
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' data_rob2 <- data.frame(
#'    domain = rep(
#'        c(
#'            "Overall bias",
#'            "Bias due to deviations from intended interventions",
#'            "Missing data"
#'        ),
#'        each = 3
#'    ),
#'    judgement = rep(c("Low", "Some concerns", "High risk of bias"), times = 3),
#'    study_share = c(0.4, 0.4, 0.2, 0.1, 0.85, 0.05, 0.2, 0.5, 0.3)
#')
#'
#'plot_rob2_summary(
#'    data_rob2,
#'    domain = domain,
#'    judgement = judgement,
#'    study_share = study_share
#')
plot_rob2_summary <- function(data,
                              domain,
                              judgement,
                              study_share) {
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

    ## Study shares sum to 1
    check_sum <- data |>
        dplyr::group_by({
            {
                domain
            }
        }) |>
        dplyr::summarize(total = sum({
            {
                study_share
            }
        }, na.rm = FALSE)) |>
        dplyr::ungroup()

    wrong_sum <-
        sort(check_sum$domain[which(!(check_sum$total == 1 |
                                          check_sum$total == 100))])

    if (length(wrong_sum) > 0) {
        cli::cli_alert_warning(
            cli::pluralize(
                "Study shares for the {wrong_sum} domain{?s} don't sum to a sensible value."
            )
        )
    }

    if (any(check_sum$total > 1, na.rm = TRUE)) {
        plotting_scale <- 1
    } else {
        plotting_scale <- 100
    }

    ## Input types
    if (!is.numeric(data[[deparse(substitute(study_share))]])) {
        cli::cli_abort("{.var study_share} must be numeric.")
    }

    # Use consistent terminology for domains
    data <- data |>
        dplyr::mutate(
            plot_domain = dplyr::case_when(
                stringr::str_detect(stringr::str_to_lower({
                    {
                        domain
                    }
                }), "overall") ~
                    "Overall risk of bias",
                stringr::str_detect(stringr::str_to_lower({
                    {
                        domain
                    }
                }), "allocation|random") ~
                    "Bias arising from randomisation process",
                stringr::str_detect({
                    {
                        domain
                    }
                }, "deviation|intended") ~ "
            Bias due to deviations from intended interventions",
            stringr::str_detect(stringr::str_to_lower({
                {
                    domain
                }
            }), "measure") ~
                "Bias in outcome measurement",
            stringr::str_detect(stringr::str_to_lower({
                {
                    domain
                }
            }), "miss") ~
                "Bias due to missing outcome data",
            stringr::str_detect(stringr::str_to_lower({
                {
                    domain
                }
            }), "report|select") ~
                "Bias in selection of reported results"
            )
        ) |>
        dplyr::mutate(
            plot_judgement = dplyr::case_when(
                stringr::str_detect(stringr::str_to_lower({
                    {
                        judgement
                    }
                }), stringr::coll("high")) ~
                    "High risk of bias",
                stringr::str_detect(stringr::str_to_lower({
                    {
                        judgement
                    }
                }), stringr::coll("low")) ~
                    "Low risk of bias",
                stringr::str_detect(stringr::str_to_lower({
                    {
                        judgement
                    }
                }), stringr::coll("some")) ~
                    "Some concerns"
            )
        ) |>
        dplyr::mutate(
            plot_judgement = forcats::fct_relevel(
                .data$plot_judgement,
                "High risk of bias",
                "Some concerns",
                "Low risk of bias"
            )
        )

    # Plot
    ## Fill palette
    pal_fill <-
        c(covalence_colours("cov_red", "cov_orange", "cov_green"))
    names(pal_fill) <- c("High risk of bias",
                         "Some concerns",
                         "Low risk of bias")

    ## Create plot
    p <- ggplot2::ggplot(
        data = data,
        mapping = ggplot2::aes(
            x = {
                {
                    study_share
                }
            },
            y = .data$plot_domain,
            fill = .data$plot_judgement
        )
    ) +
        ggplot2::geom_bar(position = "stack",
                          stat = "identity",
                          color = "#FFFFFF") +
        ggplot2::scale_x_continuous(
            name = "Studies",
            labels = scales::label_percent(scale = plotting_scale),
            expand = ggplot2::expansion(add = c(0, 0.02))
        ) +
        ggplot2::scale_y_discrete(
            name = NULL,
            labels = scales::wrap_format(width = 25),
            expand = ggplot2::expansion(add = 0)
        ) +
        ggplot2::scale_fill_manual(
            NULL,
            breaks =
                c("Low risk of bias",
                  "Some concerns",
                  "High risk of bias"),
            values = pal_fill
        ) +
        ggplot2::coord_cartesian(clip = "off") +
        theme_covalence() +
        ggplot2::theme(
            axis.text = ggplot2::element_text(family = "bold"),
            axis.title.y = ggplot2::element_blank(),
            legend.title = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_blank()
        )

    p
}
