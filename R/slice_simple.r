
parse_simple_slice_args <- function(data, index_by, variables, L, start, step, names) {

    check_index_column(data, index_by)
    variables <- parse_variables(data, index_by, NULL, variables)
    sf    <- guess_sample_freq(data, index_by)
    L     <- parse_laglead_times(L, variables, sf)
    slice_times <- parse_slice_times(data, index_by, start, step, sf)

    parsed <- list(L, variables, slice_times)

    return(parsed)
}

slice_simple <- function(data, index_by, variables, L, start, step, names) {

    parsed <- match.call()
    parsed[[1]] <- parse_simple_slice_args
    parsed <- eval(parsed)

    L <- parsed[[1]]
    variables <- parsed[[2]]
    slice_times <- parsed[[3]]

    out <- lapply(slice_times, function(i) {
        do_single_slice(data, i, index_by, variables, L, names)
    })
    out <- do.call(c, out)

    return(out)
}