# SHAPESHIFTR CLOSURE CLASS ------------------------------------------------------------------------

#' Internal `shapeshiftr_closure` Constructor
#'
#' S3 wrapper for paired forward/backward closures enabling reversible transformations
#'
#' If \code{funs} is a single function, it becomes the forward transformation
#' and \code{function(x) x} is used as the backward transformation (identity).
#'
#' @param funs Either single function or list of two functions. If single function, it is assumed
#'     to be the forward transformation and backward is set to identity. If list, assumed to be 
#'     forward and backward in this order
#'
#' @return S3 object of class \code{shapeshiftr_closure} with elements:
#'   \item{forward}{Forward transformation function}
#'   \item{backward}{Backward transformation function}
#'
#' @keywords internal

new_shapeshiftr_closure <- function(funs) {
    if (!is.list(funs)) {
        forward_fn <- funs
        backward_fn <- function(x) x
        environment(backward_fn) <- environment(forward_fn)
        funs <- list(forward_fn, backward_fn)
    }

    names(funs) <- c("forward", "backward")
    class(funs) <- "shapeshiftr_closure"

    validate_shapeshiftr_closure(funs)
}

#' Validator for shapeshiftr_closure class
#'
#' Enforces structural correctness and environment consistency for `shapeshiftr_closure` objects.
#'
#' Validation checks (in order):
#' \enumerate{
#'   \item Is a list
#'   \item Has length 2
#'   \item Has names \code{c("forward", "backward")}
#'   \item Both elements are functions
#'   \item Both functions have exactly one argument
#'   \item Argument is named "x"
#'   \item Environments are identical (strict check with \code{identical()})
#' }
#'
#' Environment identity is critical: both closures must share the same
#' environment to ensure parameter consistency (e.g., same \code{train_means}).
#'
#' **Environment Validation:**
#' 
#' Uses \code{identical()} to ensure forward and backward closures share the exact
#' same environment (same memory address). This is stricter than \code{all.equal()}
#' and guarantees parameter consistency between transformations.
#'
#' @param sc Object to validate
#'
#' @return Invisibly returns \code{sc} if valid; otherwise stops with error
#'
#' @keywords internal

validate_shapeshiftr_closure <- function(sc) {
    if (!is.list(sc)) {
        stop("shapeshiftr_closure must be a list", call. = FALSE)
    }

    if (length(sc) != 2) {
        stop("shapeshiftr_closure must have length 2, got length ", length(sc),
            call. = FALSE)
    }

    expected_names <- c("forward", "backward")
    if (!all(names(sc) == expected_names)) {
        stop("shapeshiftr_closure must have names c('forward', 'backward'), got: ",
            paste(names(sc), collapse = ", "), call. = FALSE)
    }

    if (!is.function(sc$forward)) {
        stop("forward element must be a function", call. = FALSE)
    }

    if (!is.function(sc$backward)) {
        stop("backward element must be a function", call. = FALSE)
    }

    validate_closure_signature(sc$forward, "forward")
    validate_closure_signature(sc$backward, "backward")

    env_forward <- environment(sc$forward)
    env_backward <- environment(sc$backward)

    if (!identical(env_forward, env_backward)) {
        stop("forward and backward closures must share the same environment.\n",
            "  Forward environment: ", format(env_forward), "\n",
            "  Backward environment: ", format(env_backward), call. = FALSE)
    }

    invisible(sc)
}

#' Validate closure signature
#'
#' Helper function to validate a single closure has correct signature
#'
#' @param fn Function to validate
#' @param name Name of the function (for error messages)
#'
#' @return Invisibly returns fn if valid; otherwise stops with error
#'
#' @keywords internal

validate_closure_signature <- function(fn, name) {
    fn_formals <- formals(fn)

    if (length(fn_formals) != 1) {
        stop(name, " function must have exactly 1 argument, got ",
            length(fn_formals), call. = FALSE)
    }

    if (names(fn_formals)[1] != "x") {
        stop(name, " function argument must be named 'x', got: ",
            names(fn_formals)[1], call. = FALSE)
    }

    invisible(fn)
}
