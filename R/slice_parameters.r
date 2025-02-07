
# SLICE PARAMETERS ---------------------------------------------------------------------------------

parse_slice_args <- function(data, variables, walk_on, slice_on, L, start, step, names, threads) {

    cl <- parse_threads(threads)
    check_index_column(data, walk_on)
    check_index_column(data, walk_on)
    variables <- parse_variables(data, walk_on, NULL, variables)
    L <- parse_laglead_times(data, slice_on, L, variables)
    slice_times <- parse_slice_times(data, walk_on, start, step)

    params <- new_slice_parameters(variables, walk_on, slice_on, L, slice_times, names, cl)

    return(params)
}

new_slice_parameters <- function(variables, walk_on, slice_on, L, slice_times, names, cl) {
    new <- list(
        variables = variables,
        walk_on = walk_on,
        slice_on = slice_on,
        L = L,
        slice_times = slice_times,
        names = names,
        cl = cl
    )

    new_class <- ifelse(walk_on == slice_on, "simple", "keyed")
    class(new) <- c(new_class, "list")

    return(new)
}
