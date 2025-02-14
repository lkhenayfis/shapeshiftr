
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
#' @param variables names of variables for extraction in slicing
#' @param walk_on name of the column to use as index for centering each slice. See Details
#' @param slice_on name of the column to use as reference for lead/lag slice search. See Details
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

slice <- function(data, variables, walk_on, slice_on = walk_on,
    L = -1, start = 2, step = 1, names = auto_name(variables), threads = 1) {

    params <- parse_slice_args(data, variables, walk_on, slice_on, L, start, step, names, threads)
    slice_times <- params$slice_times
    cluster     <- params$cl

    out <- inner_run(cluster, slice_times, function(i) do_single_slice(data, i, params))
    out <- Reduce(c, out)

    run_post_hook(cluster)

    return(out)
}

# SLICING INTERNALS --------------------------------------------------------------------------------

do_single_slice <- function(data, index, params) UseMethod("do_single_slice", params)

do_single_slice.simple <- function(data, index, params) {
    lst <- extract_lagleads(data, index, params$slice_on, params$variables, params$L)
    names(lst) <- params$names

    new_slice_artifact(lst, index, params$L)
}

do_single_slice.keyed <- function(data, index, params) {
    data_at_index <- data[data[[params$walk_on]] == index]
    do_single_slice.simple(data_at_index, index, params)
}

extract_lagleads <- function(data, index, slice_on, variables, L) {

    time_indexes <- lapply(L, function(l) index + l)
    extracted <- mapply(variables, time_indexes, FUN = function(v, t) {
        rows <- match(t, data[[slice_on]])
        list(data[rows][[v]])
    }, SIMPLIFY = FALSE)

    return(extracted)
}

# HELPERS ------------------------------------------------------------------------------------------

auto_name <- function(x) {
    ord <- split(seq_along(x), x)
    x <- split(x, x)
    x <- lapply(x, function(xi) if (length(xi) > 1) paste0(xi, "_", seq_len(length(xi))) else xi)
    x <- unname(unlist(x))
    x <- x[order(unlist(ord))]
    return(x)
}
