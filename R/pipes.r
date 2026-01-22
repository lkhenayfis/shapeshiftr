# PARSE PIPES --------------------------------------------------------------------------------------

#' Parse a Single Pipe
#' 
#' Interprets the definition of a single pipe, generating the defined closures
#' 
#' Both `env` and `enclos` function just as in `eval`, allowing `env` to correspond to a named
#' list or data.frame, and `enclos` to be an environment where helper functions can be defined
#' 
#' `raw_pipe` should be a named list with two arguments: `"on"` and `"transforms"`. `"on"` is
#' simply a vector of at most two elements indicating the names of the variables on which
#' the transformation(s) operate(s). `"transforms"` should be a list of transformation specifications.
#' Each transformation is in turn a list with at least one named element, `"fun"`, a string indicating
#' the name of the closure generator to be called. In addition to `"fun"`, other elements may exist
#' indicating parameterization of the closure generator. See `Examples` for an illustration of a raw pipe.
#' 
#' Note that functions in transformation definitions must always be closure generators and must
#' necessarily include the variadic argument `...`. This is necessary because, at a later stage,
#' these generators will be interpreted to then produce the parsed pipe in which `transforms` are,
#' in fact, functions to be called on the data.
#' 
#' @param raw_pipe a list defining a singular pipe, that is, with element `"on"` and `"transforms"`
#' @param env environment where the raw pipes will be evaluated
#' @param enclos enclosing environment for evaluation of the closures
#' 
#' @examples 
#' 
#' # generator of a closure with single argument `x` that filters this argument based on
#' # other specified parameters
#' gen_closure_filter <- function(by, value, ...) function(x) x[x[[by]] == value, ]
#' 
#' # generator of a closure with single argument `x` that calls `summary` on this argument
#' gen_closure_summary <- function(...) function(x) summary(x)
#' 
#' raw_pipe <- list(
#'     on = "mtcars",
#'     transforms = list(
#'         list(
#'             fun = "gen_closure_filter",
#'             by = "cyl", value = 6
#'         ),
#'         list(
#'             fun = "gen_closure_summary"
#'         )
#'     )
#' )
#' 
#' parsed_pipe <- shapeshiftr:::parse_single_pipe(raw_pipe)
#' 
#' @return list `raw_pipe` with element `"transforms"` evaluated to the defined closures
#' 
#' @export

parse_single_pipe <- function(raw_pipe, env = parent.frame(), enclos = parent.frame()) {
    args <- lapply(raw_pipe$on, str2lang)
    names(args) <- c("x", "y")[seq_along(args)]

    l_t <- raw_pipe$transforms

    cc <- c(l_t[[1]], args)
    cc[[1]] <- str2lang(cc[[1]])
    raw_pipe$transforms[[1]] <- eval(as.call(cc), env, enclos)

    if (length(l_t) >= 2) {
        x <- eval(as.call(c(list(raw_pipe$transforms[[1]]), args)), env, enclos)
        for (i in seq_along(l_t)[-1]) {
            cc <- c(l_t[[i]], list(x = x))
            cc[[1]] <- str2lang(cc[[1]])
            raw_pipe$transforms[[i]] <- eval(as.call(cc), env, enclos)
            x <- eval(as.call(c(list(raw_pipe$transforms[[i]]), list(x = x))), env, enclos)
        }
    }

    return(raw_pipe)
}

#' Parse List of Pipes
#' 
#' Simple wrapper for looping `parse_single_pipe` over multiple pipes
#' 
#' @param raw_pipes list defining various pipes
#' @param env environment where the pipe will be evaluated
#' @param enclos enclosing environment for evaluation of the closures
#' 
#' @return list `raw_pipes` with elements `"transforms"` of each pipe evaluated
#' 
#' @export

parse_pipes <- function(raw_pipes, env = parent.frame(), enclos = parent.frame()) {
    lapply(raw_pipes, parse_single_pipe, env = env, enclos = enclos)
}

# EVALUATE PIPES -----------------------------------------------------------------------------------

#' Evaluate a Single Pipe
#' 
#' Applies the closures in a `pipe` already parsed to the data defined in `"on"`
#' 
#' Both `env` and `enclos` function just as in `eval`, allowing `env` to correspond to a named
#' list or data.frame, and `enclos` to be an environment where helper functions can be defined
#' 
#' @param pipe a pipe parsed by `parse_single_pipe`
#' @param env environment where the pipe will be evaluated
#' @param enclos enclosing environment for evaluation of the closures
#' 
#' @return result of applying the closures in `transforms` to the data in `"on"`
#' 
#' @export

eval_single_pipe <- function(pipe, env = parent.frame(), enclos = parent.frame()) {

    args <- lapply(pipe$on, str2lang)
    names(args) <- c("x", "y")[seq_along(args)]

    l_t <- pipe$transforms

    cc <- c(list(l_t[[1]]), args)
    x <- eval(as.call(cc), env, enclos)
    l_t[[1]] <- NULL

    if (length(l_t) >= 1) {
        for (f in l_t) {
            cc <- list(f, x)
            x <- eval(as.call(cc), env, enclos)
        }
    }

    return(x)
}

#' Evaluate List of Pipes
#' 
#' Simple wrapper for looping `eval_single_pipe` over multiple pipes
#' 
#' @param pipes list of pipes already parsed by `parse_pipes`
#' @param env environment where the pipe will be evaluated
#' @param enclos enclosing environment for evaluation of the closures
#' 
#' @return single data.table combining the results of applying all pipes in `pipes`
#' 
#' @export

eval_pipes <- function(pipes, env = parent.frame(), enclos = parent.frame()) {
    lapply(pipes, eval_single_pipe, env = env, enclos = enclos)
}

#' Combination of Pipe Results
#' 
#' Combines outputs from pipes resulting from a call to `eval_pipes`
#' 
#' Essentially this function is a wrapper of `Reduce` applied to `evals` with the function
#' `combine_fun`. Because of this, `combine_fun` should be a function with at least two arguments
#' `x` and `y`, as well as the variadic argument `...` for consistency between calls. Other
#' optional arguments are permitted and will be passed to `combine_fun`.
#' 
#' `default_combine` is merely a call to `merge` using `by = 1`, that is, merging all
#' data in `evals` by the first column of each one.
#' 
#' @param evals list of elements to be combined, typically the result of `eval_pipes`
#' @param combine_fun combination function to be applied, by default `default_combine`. See
#'     Details
#' @param ... additional arguments to be passed to `combine_fun`
#' 
#' @return result of the combination
#' 
#' @export

combine_pipes <- function(evals, combine_fun = default_combine, ...) {
    Reduce(function(x, y) combine_fun(x, y, ...), evals)
}

default_combine <- function(x, y, ...) {
    merge(x, y, by = 1)
}