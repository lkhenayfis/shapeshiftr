
#' Generate Temporal Slices From Data
#' 
#' Operates on a time-indexed data.frame-like to generate appropriate regressors and target variable
#' 
#' Argument \code{variables} defines which columns in \code{data} are to be sliced. If it is empty,
#' all variables except \code{index_by} and \code{key_by} are used. This argument also accepts
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
#' @param index_by name of the column to use as index; this must be strictly increasing and 
#'     \code{data} will be ordered on this column
#' @param key_by similar to \code{index_by}, but serves as a secondary index. This is useful for
#'     forecast data, in which there is a from and to time-stamp; same restrictions apply
#' @param variables names of variables for extraction in slicing
#' @param L lag/lead times of the variables that must be extracted. See Details
#' @param start integer indicating at which line slicing will start
#' @param step integer indicating step size to walk through \code{index_by}
#' @param names naming for each sliced variable; by default this is the same as \code{variables} or,
#'     if there are duplicates, appends \code{_X} where X is an increasing integer 
#' 
#' @examples
#' 
#' @seealso \code{\link{slice_artifact}} for in-depth details of the returned object
#' 
#' @return An \code{\link{slice_artifact}} object with the results, see it's documentation for more
#'     details

slice <- function(data, index_by, key_by, variables,
    L = -1, start = 2, step = 1, names = auto_name(variables)) {

    mc <- match.call()
    if (missing("key_by")) {
        mc[[1]] <- slice_simple
    } else {
        mc[[1]] <- slice_keyed
    }

    eval(mc, parent.frame(), parent.frame())
}

auto_name <- function(x) {
    ord <- split(seq_along(x), x)
    x <- split(x, x)
    x <- lapply(x, function(xi) if (length(xi) > 1) paste0(xi, "_", seq_len(length(xi))) else xi)
    x <- unname(unlist(x))
    x <- x[order(unlist(ord))]
    return(x)
}