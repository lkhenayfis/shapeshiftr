
#' Generate Temporal Slices From Data
#' 
#' Operates on a time-indexed data.frame-like to generate appropriate regressors and target variable
#' 
#' \code{walk_on} should be a time type column on which the reference times for slicing are searched
#' for. There is also \code{slice_by}, which indicates on which column the lags/lead times are
#' searched for. This may be confusing at first, but is intended to allow for simpler use of slicing
#' when \code{data} is keyed, for example in the case of forecasting data in which there is a column
#' of when the forecast was executed and which is the target time of the forecast.
#' 
#' For example, the included data.table \code{keyed_dt_date}
#' 
#' |      date  | target_date | X1  |  X2  | X3  |   Y |
#' |      ---   | ---         | --- |  --- | --- | --- |
#' | 2025-01-02 | 2025-01-03  | 16  | -18  | 66  | 132 |
#' | 2025-01-02 | 2025-01-04  |  5  | -17  | 55  | 122 |
#' | 2025-01-02 | 2025-01-05  | 12  | -16  | 68  | 138 |
#' | 2025-01-02 | 2025-01-06  | 15  | -19  | 69  | 137 |
#' | 2025-01-02 | 2025-01-07  |  9  |  -7  | 70  | 125 |
#' 
#' If \code{walk_on = "date"} and \code{slice_on = "target_date"}, slices will be generated for
#' values in \code{"date"}, but the actual subset will be based on \code{target_date}.
#' 
#' Argument \code{variables} defines which columns in \code{data} are to be sliced. If it is empty,
#' all variables except \code{walk_on} and \code{key_by} are used. This argument also accepts
#' duplicated values. This is intended to facilitate different forms of slicing a single variable
#' in the data. One example would be if one is extracting both target future values and explanatory
#' past values for fitting an autoregressive model.
#' 
#' Lag and lead times, passed through argument \code{L}, are differentiated by their sign. Negative
#' values are interpreted as lags and positive ones as leads. The canonical way to pass this
#' argument is as list indicating lags/leads for each variable, in the same order as
#' \code{variables}. If unamed, it is assumed \code{L} is in the same order as \code{variables}.
#' If \code{L} is a single integer or vector of integers, it is assumed all variables should be
#' sliced equally.
#' 
#' @param data the data.frame-like object on which to operate
#' @param walk_on name of the column to use as index for centering each slice. See Details
#' @param slice_on name of the column to use as reference for lead/lag slice search. See Details
#' @param variables names of variables for extraction in slicing
#' @param L lag/lead times of the variables that must be extracted. See Details
#' @param start where the slicing will start. Can be an integer, in which case it is interpreted as
#'     a line number, or a date-like object, in which case it is interpreted literally
#' @param step step size to walk through \code{walk_on}. Can be an integer, in which case it is
#'     interpreted as number of intervals in the temporal resolution of \code{walk_on}, or a string
#'     of the form "2 hours". See \code{\link{difftime}} for which time units are available
#' @param names naming for each sliced variable; by default this is the same as \code{variables} or,
#'     if there are duplicates, appends \code{_X} where X is an increasing integer 
#' @param threads number of threads for parallel execution; if 1, runs single threaded. Parallel
#'     execution is handled with \code{\link[parallel]{parLapply}} and is only supported in Linux
#'     based systems
#' 
#' @seealso \code{\link{new_slice_artifact}} for in-depth details of the returned object
#' 
#' @return An \code{slice_artifact} object with the results, see \code{\link{new_slice_artifact}}
#'     for more details
#' 
#' @export

slice <- function(data, walk_on, slice_on, variables,
    L = -1, start = 2, step = 1, names = auto_name(variables), threads = 1) {

    if (missing("slice_on")) {
        slice_simple(data, walk_on, variables, L, start, step, names, threads)
    } else {
        slice_keyed(data, walk_on, slice_on, variables, L, start, step, names, threads)
    }
}

auto_name <- function(x) {
    ord <- split(seq_along(x), x)
    x <- split(x, x)
    x <- lapply(x, function(xi) if (length(xi) > 1) paste0(xi, "_", seq_len(length(xi))) else xi)
    x <- unname(unlist(x))
    x <- x[order(unlist(ord))]
    return(x)
}

inner_run <- function(cl, X, fun) UseMethod("inner_run")

inner_run.default <- function(cl, X, fun) lapply(X, fun)

inner_run.cluster <- function(cl, X, fun) parallel::parLapply(cl, X, fun)

run_post_hook <- function(cl) {
    hook <- attr(cl, "post_hook")
    hook(cl)
}

# SLICE SIMPLE DATA --------------------------------------------------------------------------------

parse_simple_slice_args <- function(data, walk_on, variables, L, start, step, names, threads) {

    cl <- parse_threads(threads)
    check_index_column(data, walk_on)
    variables <- parse_variables(data, walk_on, NULL, variables)
    L     <- parse_laglead_times(data, walk_on, L, variables)
    slice_times <- parse_slice_times(data, walk_on, start, step)

    parsed <- list(L, variables, slice_times, cl)

    return(parsed)
}

slice_simple <- function(data, walk_on, variables, L, start, step, names, threads) {

    parsed <- match.call()
    parsed[[1]] <- parse_simple_slice_args
    parsed <- eval(parsed, parent.frame(), parent.frame())

    L <- parsed[[1]]
    variables <- parsed[[2]]
    slice_times <- parsed[[3]]
    cl <- parsed[[4]]

    inner_fun <- function(i) do_single_slice(data, i, walk_on, variables, L, names)

    out <- inner_run(cl, slice_times, inner_fun)
    out <- do.call(c, out)

    run_post_hook(cl)

    return(out)
}

# SLICE KEYED DATA ---------------------------------------------------------------------------------

parse_keyed_slice_args <- function(data, walk_on, slice_on, variables, L, start, step, names, threads) {

    cl <- parse_threads(threads)
    check_index_column(data, walk_on)
    check_index_column(data, slice_on)
    variables <- parse_variables(data, walk_on, slice_on, variables)
    L     <- parse_laglead_times(data, slice_on, L, variables)
    slice_times <- parse_slice_times(data, walk_on, start, step)

    parsed <- list(L, variables, slice_times, cl)

    return(parsed)
}

slice_keyed <- function(data, walk_on, slice_on, variables, L, start, step, names, threads) {

    parsed <- match.call()
    parsed[[1]] <- parse_keyed_slice_args
    parsed <- eval(parsed, parent.frame(), parent.frame())

    L <- parsed[[1]]
    variables <- parsed[[2]]
    slice_times <- parsed[[3]]
    cl <- parsed[[4]]

    inner_fun <- function(i) {
        do_single_slice(data[data[[walk_on]] == i], i, slice_on, variables, L, names)
    }

    out <- inner_run(cl, slice_times, inner_fun)
    out <- do.call(c, out)

    run_post_hook(cl)

    return(out)
}
