
#' Generate Temporal Slices From Data
#' 
#' Operates on a time-indexed data.frame-like to generate appropriate regressors and target variable
#' 
#' Lag times, indicated by argument \code{L}, can be defined individually for each explanatory or 
#' target variable. In order to do so, this argument must be a list of vectors of integers.
#' The first position is assumed corresponding to the target variable; the following ones are 
#' matched to \code{regressors} in the order this argument was provided. It is possible to pass
#' \code{L} as a single integer, or vector of integers, in which case it will be assumed the same
#' lags for every variable.
#' 
#' The behavior of lead times \code{H} is similar, since it is only applicable to the target
#' variable, it is a vector of integers at most, not a list.
#' 
#' @param data the data.frame-like object on which to operate
#' @param index_by name of the column to use as index; this must be strictly increasing and 
#'     \code{data} will be ordered on this column
#' @param key_by similar to \code{index_by}, but serves as a secondary index. This is useful for
#'     forecast data, in which there is a from and to time-stamp; same restrictions apply
#' @param target name of the target variable
#' @param regressors optional, names of explanatory variables
#' @param L lag times of the target and explanatory variables that must be extracted. See Details
#' @param H lead times of the target variable that must be extracted. See Details
#' @param start integer indicating at which line slicing will start
#' @param step integer indicating step size to walk through \code{index_by}
#' 
#' @examples
#' 
#' @seealso \code{\link{slice_artifact}} for in-depth details of the returned object
#' 
#' @return An \code{\link{slice_artifact}} object with the results, see it's documentation for more
#'     details

slice <- function(data, index_by, key_by, target, regressors, L = 1, H = 1, start = 2, step = 1) {

    mc <- match.call()
    if (missing("key_by")) {
        mc[[1]] <- slice_simple
    } else {
        mc[[1]] <- slice_keyed
    }

    eval(mc)
}
