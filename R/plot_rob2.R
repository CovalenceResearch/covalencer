plot_rob2_summary <- function(data,
                              domain = domain,
                              judgement = judgement,
                              study_share = study_share) {
    col_domain      <- rlang::as_string(rlang::ensym(domain))
    col_judgement   <- rlang::as_string(rlang::ensym(judgement))
    col_study_share <- rlang::as_string(rlang::ensym(study_share))

    col_names <- c(col_domain, col_judgement, col_study_share)
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
    if (!is.numeric(data[, deparse(substitute(study_share))])) {
        cli::cli_abort("{.var study_share} must be numeric.")
    }
}
