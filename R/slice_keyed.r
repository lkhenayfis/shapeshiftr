
parse_keyed_slice_args <- function(data, walk_on, slice_on, variables, L, start, step, names) {

    check_index_column(data, walk_on)
    check_index_column(data, slice_on)
    variables <- parse_variables(data, walk_on, slice_on, variables)
    sf_w  <- guess_sample_freq(data, walk_on)
    sf_s  <- guess_sample_freq(data, slice_on)
    L     <- parse_laglead_times(L, variables, sf_s)
    slice_times <- parse_slice_times(data, walk_on, start, step, sf_w)

    parsed <- list(L, variables, slice_times)

    return(parsed)
}

slice_keyed <- function(data, walk_on, slice_on, variables, L, start, step, names) {

    parsed <- match.call()
    parsed[[1]] <- parse_keyed_slice_args
    parsed <- eval(parsed, parent.frame(), parent.frame())

    L <- parsed[[1]]
    variables <- parsed[[2]]
    slice_times <- parsed[[3]]

    out  <- lapply(slice_times, function(i) {
        do_single_slice(data[data[[walk_on]] == i], i, slice_on, variables, L, names)
    })
    out <- do.call(c, out)

    return(out)
}