#' Plot incremental outcomes on the cost-effectiveness plane
#'
#' @description
#' `plot_psa_scatter` takes a data.frame of incremental quality-adjusted life-years (QALYs) and incremental costs, as might be obtained from a probabilistic sensitivity analysis, and plots these data on the incremental cost-effectiveness plane.
#'
#' @details
#' Fore more information on the display of incremental outcomes on the cost-effectiveness plane, see, for example, [Fenwick *et al.*, 2006](https://pubmed.ncbi.nlm.nih.gov/16623946/).
#'
#' @param data A data.frame.
#' @param delta_qalys Unquoted column name for incremental QALYs.
#' @param delta_costs Unquoted column name for incremental costs.
#' @param currency String for the currency symbol. Default is `$`.
#' @param point_alpha Numeric value for point opacity. Default is `0.4`.
#' @param point_color Point color. Default is `#249bc9`.
#' @param point_shape Integer value for point shape. Default is `16`. See `?pch` for available shapes.
#' @param point_size Numeric value for point size. Default is `2`.
#' @param point_jitter_height,point_jitter_width Numeric value for vertical and horizontal jittering. Default is `0` (no jittering). Be careful that some key-outcome pairs may not be plotted if there is too much jitter.
#' @param show_wtp Boolean indicating if the willingness-to-pay (WTP) threshold should be displayed (as a line). Default is `TRUE`.
#' @param wtp_value Numeric value for WTP threshold. Default is `1`, a deliberately unrealistic value to remind you to set the value to your specific example.
#' @param wtp_alpha Numeric value for WTP threshold line opacity. Default is `1`.
#' @param wtp_color Color for WTP threshold line. Default is `#154754`.
#' @param wtp_linetype A valid specification of the WTP threshold line type. Default is `dashed` (`2`). See the [ggplot2](https://ggplot2.tidyverse.org/reference/aes_linetype_size_shape.html) documentation for valid options.
#' @param wtp_linewidth Numeric value for the WTP threshold line width. Default is `0.5`.
#' @param show_mean Boolean indicating if the mean of incremental QALYs and costs should be displayed (as a point). Default is `TRUE`.
#' @param mean_alpha Numeric value for mean point opacity. Default is `1`.
#' @param mean_color Color for the mean point. Default is `#7a0d66`.
#' @param mean_shape Integer value for mean point shape. Default is `18`. See `?pch` for available shapes.
#' @param mean_size Numeric value for point size. Default is `3`.
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' library(ggplot2)
#' df <- data.frame(d_qalys = runif(1000, -1, 2),
#'                  d_costs = runif(1000, -1000, 10000),
#'                  country = rep(c("Country A", "Country B"), each = 500))
#'
#' plot_psa_scatter(df, d_qalys, d_costs, wtp_value = 20000)
#'
#' # Facet plot by adding a call to facet_plot/facet_grid
#' plot_psa_scatter(df, d_qalys, d_costs, show_wtp = FALSE) +
#'   facet_wrap(vars(country))
#'
#' # Don't forget that column names should be unquoted
#' \dontrun{
#'   plot_psa_scatter(df, "d_qalys", "d_costs")
#' }
plot_psa_scatter <- function(data,
                             delta_qalys ,
                             delta_costs,
                             currency = "$",
                             point_alpha = 0.4,
                             point_color = "#249bc9",
                             point_shape = 16,
                             point_size = 2,
                             point_jitter_height = 0,
                             point_jitter_width = 0,
                             show_wtp = TRUE,
                             wtp_value = 1,
                             wtp_alpha = 1,
                             wtp_color = "#154754",
                             wtp_linetype = "dashed",
                             wtp_linewidth = 0.5,
                             show_mean = TRUE,
                             mean_alpha = 1,
                             mean_color = "#7a0d66",
                             mean_shape = 18,
                             mean_size = 3) {
    col_qalys <- rlang::as_string(rlang::ensym(delta_qalys))
    col_costs <- rlang::as_string(rlang::ensym(delta_costs))

    # Argument checks

    ## Data
    rlang::check_required(delta_qalys)
    rlang::check_required(delta_costs)

    if (!(col_qalys %in% names(data))) {
        cli::cli_abort(
            c(
                "{.var {rlang::caller_arg(delta_qalys)}} is not a column in {.var {rlang::caller_arg(data)}}."
            ),
            call = rlang::caller_env()
        )
    }
    if (!(col_costs %in% names(data))) {
        cli::cli_abort(
            c(
                "{.var {rlang::caller_arg(delta_costs)}} is not a column in {.var {rlang::caller_arg(data)}}."
            ),
            call = rlang::caller_env()
        )
    }

    ## Input: WTP
    if (!is.numeric(wtp_value)) {
        cli::cli_abort("{.var wtp_value} must be numeric.")
    }

    if (wtp_value < 0) {
        cli::cli_alert_warning("The WTP threshold is set to a negative value!")
    }

    if (wtp_value == 1) {
        cli::cli_alert_warning("The WTP threshold is (still) set at 1.")
    }

    ## Inputs: Booleans
    if (!is.logical(show_wtp)) {
        cli::cli_abort("{.var show_twp} must be TRUE or FALSE.")
    }

    if (!is.logical(show_mean)) {
        cli::cli_abort("{.var show_mean} must be TRUE or FALSE.")
    }

    ## Inputs: color
    if (!check_color(point_color)) {
        cli::cli_abort("{.var point_color} must be a hex color or in the R colors.")
    }

    if (!check_color(wtp_color)) {
        cli::cli_abort("{.var wtp_color} must be a hex color or in the R colors.")
    }

    if (!check_color(mean_color)) {
        cli::cli_abort("{.var mean_color} must be a hex color or in the R colors.")
    }


    # Calculate symmetrical plot limits
    max_qalys <- max(abs(data[[col_qalys]]))
    max_costs <- max(abs(data[[col_costs]]))

    limits_qalys <- c(-max_qalys, max_qalys)
    limits_costs <- c(-max_costs, max_costs)

    # Create scatter plot
    p <- ggplot2::ggplot(data = data,
                         mapping = ggplot2::aes(x = {
                             {
                                 delta_qalys
                             }
                         },
                         y = {
                             {
                                 delta_costs
                             }
                         })) +
        ggplot2::geom_hline(
            yintercept = 0,
            color = "#000000",
            linewidth = 0.5
        ) +
        ggplot2::geom_vline(
            xintercept = 0,
            color = "#000000",
            linewidth = 0.5
        ) +
        ggplot2::geom_jitter(
            alpha = point_alpha,
            color = point_color,
            shape = point_shape,
            size = point_size,
            width = point_jitter_width,
            height = point_jitter_height
        ) +
        ggplot2::scale_x_continuous(
            "Incremental quality-adjusted life-years",
            labels = scales::number_format(big.mark = ","),
            limits = limits_qalys
        ) +
        ggplot2::scale_y_continuous(
            "Incremental costs",
            labels = scales::label_dollar(prefix = currency, big.mark = ","),
            limits = limits_costs
        ) +
        theme_covalence() +
        ggplot2::theme(panel.grid.major = ggplot2::element_blank())

    # Add willingness-to-pay (WTP) threshold as line
    if (show_wtp) {
        p <- p +
            ggplot2::geom_abline(
                intercept = 0,
                slope = wtp_value,
                alpha = wtp_alpha,
                color = wtp_color,
                linetype = wtp_linetype,
                linewidth = wtp_linewidth
            )
    } else {
        p
    }

    # Add mean on plot
    if (show_mean) {
        p <- p +
            stat_doublemean(
                alpha = mean_alpha,
                color = mean_color,
                shape = mean_shape,
                size  = mean_size
            )
    } else {
        p
    }

    p
}


#' Calculate the mean of two variables
#'
#' @description
#' Calculate the arithmetic mean of two variables, for subsequent use in a call to [`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html).
#'
#' @details
#' The code for the `stat` and `layer` are heavily based on the [extending `ggplot2`](https://ggplot2.tidyverse.org/articles/extending-ggplot2.html) vignette.
#'
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#'
#' @return A 'ggplot2' stat.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(diamonds, aes(x = carat, y = price)) +
#'   geom_point() +
#'   stat_doublemean(color = "#249bc9", shape = 18, size = 3)
stat_doublemean <- function(mapping = NULL,
                            data = NULL,
                            geom = "point",
                            position = "identity",
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE,
                            ...) {
    ggplot2::layer(
        stat = StatDoubleMean,
        data = data,
        mapping = mapping,
        geom = geom,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = rlang::list2(na.rm = na.rm, ...)
    )
}

#' @rdname stat_doublemean
#' @format NULL
#' @usage NULL
#' @export
StatDoubleMean <- ggplot2::ggproto(
    "StatDoubleMean",
    ggplot2::Stat,
    compute_group = function(data, scales) {
        data.frame(x = mean(data$x, na.rm = TRUE),
                   y = mean(data$y, na.rm = TRUE))
    },
    required_aes = c("x", "y")
)
