
parse_simple_slice_args <- function(data, walk_on, variables, L, start, step, names) {

    check_index_column(data, walk_on)
    variables <- parse_variables(data, walk_on, NULL, variables)
    sf    <- guess_sample_freq(data, walk_on)
    L     <- parse_laglead_times(L, variables, sf)
    slice_times <- parse_slice_times(data, walk_on, start, step, sf)

    parsed <- list(L, variables, slice_times)

    return(parsed)
}

slice_simple <- function(data, walk_on, variables, L, start, step, names) {

    parsed <- match.call()
    parsed[[1]] <- parse_simple_slice_args
    parsed <- eval(parsed, parent.frame(), parent.frame())

    L <- parsed[[1]]
    variables <- parsed[[2]]
    slice_times <- parsed[[3]]

    out <- lapply(slice_times, function(i) {
        do_single_slice(data, i, walk_on, variables, L, names)
    })
    out <- do.call(c, out)

    return(out)
}