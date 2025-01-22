
parse_simple_slice_args <- function(data, index_by, target, regressors, L, H, start, step) {

    L <- parse_lag_times(L, length(regressors))
    H <- parse_lead_times(H)
    start <- parse_start_time(start, L)
    check_zero_lead_lag(L, H)
    check_column_names(data, index_by, target, regressors)

    parsed <- list(L, H, start)

    return(parsed)
}

slice_simple <- function(data, index_by, target, regressors, L, H, start, step) {

    parsed <- match.call()
    parsed[[1]] <- parse_simple_slice_args
    parsed <- eval(parsed)

    L <- parsed[[1]]
    H <- parsed[[2]]
    start <- parsed[[3]]

    roll_indexes <- seq(start, nrow(data) - max(H), by = step)

    out <- lapply(roll_indexes, function(i) {
        do_single_slice(data, index_by, target, regressors, L, H, i)
    })

    return(out)
}