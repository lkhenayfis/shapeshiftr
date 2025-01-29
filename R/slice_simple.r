
parse_simple_slice_args <- function(data, index_by, variables, L, start, step, names) {

    check_index_column(data, index_by)
    variables <- parse_variables(data, index_by, NULL, variables)
    sf    <- guess_sample_freq(data, index_by)
    L     <- parse_laglead_times(L, variables, sf)
    #start <- parse_start_time(data, L, start)

    parsed <- list(L, start, variables)

    return(parsed)
}

slice_simple <- function(data, index_by, variables, L, start, step, names) {

    parsed <- match.call()
    parsed[[1]] <- parse_simple_slice_args
    parsed <- eval(parsed)

    L <- parsed[[1]]
    #start <- parsed[[2]]
    variables <- parsed[[3]]

    roll_indexes <- seq(start, nrow(data) - max(H), by = step)

    out <- lapply(roll_indexes, function(i) {
        do_single_slice(data, index_by, target, regressors, L, H, i)
    })

    return(out)
}