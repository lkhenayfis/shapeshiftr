
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

#' @export
#' 
#' @rdname general-methods

median.slice_artifact <- function(x, variables, ...) {
    INNER_METHOD_RUN(x, variables, median, ...)
}

#' @export
#' 
#' @rdname general-methods

summary.slice_artifact <- function(x, variables, ...) {
    INNER_METHOD_RUN(x, variables, summary, ...)
}

#' @export

sd <- function(x, na.rm = FALSE) UseMethod("sd")

#' @export

sd.default <- function(x, na.rm = FALSE) stats::sd(x, na.rm)

#' @export
#' 
#' @rdname general-methods

sd.slice_artifact <- function(x, variables, ...) {
    INNER_METHOD_RUN(x, variables, sd, ...)
}

#' @export

var <- function(x, y = NULL, na.rm = FALSE, use) UseMethod("var")

#' @export

var.default <- function(x, y = NULL, na.rm = FALSE, use) stats::var(x, y, na.rm, use)

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