
#' General Methods For `slice_artifact` Class
#' 
#' Methods of [base], [utils] and [stats] generics for `slice_artifact` objects
#' 
#' @param x a `slice_artifact` object
#' @param variables variables on which to compute function
#' @param ... remaining arguments passed on to generic
#' 
#' @return `slice_artifact` with computed function on all slices of specified `variables`
#' 
#' @name general-methods
NULL

# BASE METHODS -------------------------------------------------------------------------------------

#' @export
#' 
#' @rdname general-methods

mean.slice_artifact <- function(x, variables, ...) {
    INNER_METHOD_RUN(x, variables, mean, ...)
}

#' @export
#' 
#' @rdname general-methods

min.slice_artifact <- function(x, variables, ...) {
    INNER_METHOD_RUN(x, variables, min, ...)
}

#' @export
#' 
#' @rdname general-methods

max.slice_artifact <- function(x, variables, ...) {
    INNER_METHOD_RUN(x, variables, max, ...)
}

# UTILS METHODS ------------------------------------------------------------------------------------

#' @export
#' 
#' @rdname general-methods

head.slice_artifact <- function(x, variables, ...) {
    INNER_METHOD_RUN(x, variables, head, ...)
}

#' @export
#' 
#' @rdname general-methods

tail.slice_artifact <- function(x, variables, ...) {
    INNER_METHOD_RUN(x, variables, tail, ...)
}

# STATS METHODS ------------------------------------------------------------------------------------

#' @param na.rm boolean indicating if `NA`s should be ignored
#' 
#' @export
#' 
#' @rdname general-methods

median.slice_artifact <- function(x, na.rm = FALSE, variables, ...) {
    INNER_METHOD_RUN(x, variables, median, ...)
}

#' @param object a `slice_artifact` object
#' 
#' @export
#' 
#' @rdname general-methods

summary.slice_artifact <- function(object, variables, ...) {
    INNER_METHOD_RUN(object, variables, summary, ...)
}

#' Mock Generic For Standard Deviation
#' 
#' Reexports the [stats::sd()] function as a generic so new methods can be created
#' 
#' @param x a `slice_artifact` object
#' @param ... remaining arguments of [stats::sd()]
#' 
#' @importFrom stats sd
#' 
#' @seealso [general-methods]
#' 
#' @export

sd <- function(x, ...) UseMethod("sd")

#' @rdname sd
#' 
#' @export

sd.default <- function(x, ...) stats::sd(x, ...)

#' @export
#' 
#' @rdname general-methods

sd.slice_artifact <- function(x, variables, ...) {
    INNER_METHOD_RUN(x, variables, sd, ...)
}

#' Mock Generic For Variance
#' 
#' Reexports the [stats::var()] function as a generic so new methods can be created
#' 
#' @param x a `slice_artifact` object
#' @param ... remaining arguments of [stats::var()]
#' 
#' @importFrom stats var
#' 
#' @seealso [general-methods]
#' 
#' @export

var <- function(x, ...) UseMethod("var")

#' @rdname var
#' 
#' @export

var.default <- function(x, ...) stats::var(x, ...)

#' @export
#' 
#' @rdname general-methods

var.slice_artifact <- function(x, variables, ...) {
    INNER_METHOD_RUN(x, variables, var, ...)
}

#' @export
#' 
#' @rdname general-methods

quantile.slice_artifact <- function(x, variables, ...) {
    INNER_METHOD_RUN(x, variables, quantile, ...)
}

# HELPERS ------------------------------------------------------------------------------------------

INNER_METHOD_RUN <- function(x, variables, fun, ...) {
    if (missing("variables")) variables <- names(x)

    x <- na.exclude(x)
    out <- lapply(x[variables], function(l) lapply(l, fun, ...))
    out <- lapply(out, function(l) lapply(l, unname))

    new_slice_artifact(out, attr(x, "index"), NA)
}