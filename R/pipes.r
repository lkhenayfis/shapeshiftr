# PARSE PIPES --------------------------------------------------------------------------------------

#' Parse a Single Pipe
#' 
#' Interprets the definition of a single pipe, generating the defined closures
#' 
#' Both `env` and `enclos` function just as in `eval`, allowing `env` to correspond to a named
#' list or data.frame, and `enclos` to be an environment where helper functions can be defined. The
#' function mainly works by evaluating a function specified in `raw_pipe` in the context of `env` 
#' and `enclos`. The following subsections provide in-depth details.
#' 
#' ## Closure Generator Specification
#' 
#' A closure generator is a function that returns a function. The returned function
#' (the closure) "closes over" variables from its enclosing environment, capturing
#' them for later use.
#' 
#' The generator can accept any number of parameters but must always include a variadic argument
#' `...`. This is necessary to ensure a consistent interface among generators.
#' 
#' The closure itself should only ever have a single argument `x` and rely on
#' the generator to capture any necessary parameters, either passed as arguments to the generator or
#' computed from data passed to the generator. The closure's only goal is to apply a transformation
#' on `x` using the parameters captured from the generator's environment.
#' 
#' An illustrative example is shown below with hypothetical function `transform`. Notice that
#' `params` is computed from `data` once, by the generator, and then encapsulated in the closure for
#' later use. This "later use" will be explained in the subsection **Parsing Process**.
#' 
#' \preformatted{
#' my_generator <- function(data, ...) {
#'   params <- compute_params(data)
#'   
#'   # Return closure that captures params
#'   function(x) {
#'     transform(x, params)
#'   }
#' }
#' }
#' 
#' So, basically, generators are a means of creating wrappers of other functions for
#' pre-parametrized use later.
#' 
#' ## Raw Pipe Structure
#' 
#' `raw_pipe` should be a named list with two arguments: `"on"` and `"transforms"`.
#' 
#' `"on"` is simply a vector of at most two elements indicating the names of the variables on which
#' the transformation(s) operate(s).
#' 
#' `"transforms"` should be a list of transformation specifications. Each transformation is in turn
#' a list with at least one named element, `"fun"`, a string indicating the name of a function which
#' **must be** a closure generator. In addition to `"fun"`, other elements may exist corresponding
#' to parameters of the generator. See the Examples for illustrations.
#' 
#' \preformatted{
#' my_raw_pipe <- list(
#'     on = c("some_variable"),
#'     transforms = list(
#'         list(fun = "my_generator", data = my_data),
#'     )
#' )
#' }
#' 
#' The transform used in this illustrative example is the one from the previous subsection. Note
#' the element `data` in the transform definition, which will be passed to the generator.
#' `some_variable` and `my_data` are placeholders for actual data.
#'
#' ## Parsing Process
#' 
#' The process of parsing a pipe is straightforward, based on building calls to the generator
#' functions with the parameters defined in the transform specifications, and evaluating them in
#' the context of `env` and `enclos`.
#' 
#' There is one key step that happens "invisibly": when the call to the generator is built, the
#' data defined in `"on"` is also passed as an argument `x` to the generator (which is why `...` is
#' necessary). This serves two purposes:
#' 
#' 1. allows for "trainable" transformations, which will be discussed in the following subsection
#' 2. allows chaining of transforms
#' 
#' Using the previous illustrative example, the parsing process would build a call like
#' 
#' \preformatted{
#' my_generator(data = my_data, x = some_variable)
#' }
#' 
#' If there is only one transformation, this is evaluated and the resulting closure is stored. If
#' there are more transformations, the resulting closure is immediately applied to `some_variable`,
#' and the resulting data is passed as `x` to the next generator in the list, and so on. This
#' ensures that each transformation is applied in sequence, with each one receiving the output of 
#' the previous one as input.
#' 
#' ### Closure Wrapping for Reversibility
#' 
#' Generator results are automatically wrapped in `shapeshiftr_closure` objects to enable
#' reversible transformations. This wrapping is transparent to existing code:
#' 
#' - **Single-closure generators**: The returned closure becomes the `forward` transformation,
#'   and an identity function `function(x) x` is used as the `backward` transformation
#' - **Paired-closure generators**: Generators can return a list of two closures
#'   `list(forward, backward)` for explicitly reversible transformations
#' - **Chaining**: During parse, only the `forward` closure is used for sequential chaining
#' - **Backward compatibility**: The wrapping is handled by the constructor and maintains
#'   compatibility with all existing single-closure generators
#' 
#' ## Stateless vs Trainable Transformations
#' 
#' For the sake of clarity, transformations can be classified into two categories: stateless and 
#' trainable. This is entirely jargon and has no real bearing on the implementation. Stateless are
#' the ones where the generator does not need to compute any parameters from data, either `x` or
#' dedicated parameters, while trainable are the ones there such computations exist.
#' 
#' ## Performance Considerations
#' 
#' The characterisct of closures to keep their binding environment allows for their use as intended
#' here but some caution must be exercised. Closures can capture large objects in their environment
#' and, if these are not needed later, this can lead to unnecessary memory consumption. Thus, it is
#' advisable to ensure that only necessary data is captured in the closure environment, deleting 
#' anything else. See Examples for illustrations.
#' 
#' @param raw_pipe a list defining a singular pipe, that is, with element `"on"` and `"transforms"`.
#'     See Details
#' @param env environment where the raw pipes will be evaluated
#' @param enclos enclosing environment for evaluation of the closures
#' 
#' @return list `raw_pipe` with element `"transforms"` evaluated to the defined closures
#' 
#' @seealso
#' * \code{\link{eval_single_pipe}} for applying parsed closures to data
#' * \code{\link{parse_pipes}} for parsing multiple pipes
#' * \code{\link{combine_pipes}} for combining multiple parsed pipes
#' 
#' @examples 
#' 
#' # Example 1: Simple closure generator without learned parameters --------------------------------
#' 
#' library(data.table)
#' data(simple_dt_date)
#' 
#' # Define closure generator: get head of data
#' gen_get_head <- function(n, ...) {
#'     # Return closure
#'     function(x) {
#'         head(x, n)
#'     }
#' }
#' 
#' # Define raw pipe structure
#' raw_pipe <- list(
#'     on = "simple_dt_date",
#'     transforms = list(
#'         list(fun = "gen_get_head", n = 5)
#'     )
#' )
#' 
#' # Parse: creates closure (no training needed)
#' parsed <- parse_single_pipe(raw_pipe)
#' print(parsed$transforms[[1]]$forward(simple_dt_date))
#' 
#' # Example 2: Stacking transformations -----------------------------------------------------------
#' 
#' # Define another closure generator: summary
#' gen_summary <- function(...) {
#'    function(x) {
#'       summary(x)
#'   }
#' }
#' 
#' #' # Define raw pipe structure
#' raw_pipe <- list(
#'     on = "simple_dt_date",
#'     transforms = list(
#'         list(fun = "gen_get_head", n = 5),
#'         list(fun = "gen_summary")
#'     )
#' )
#' 
#' # Parse: creates closure (no training needed)
#' parsed <- parse_single_pipe(raw_pipe)
#' 
#' # Application of the stacked transformations is shown in `eval_single_pipe` examples
#' 
#' # Example 3: Trainable transformation with parameter learning -----------------------------------
#' 
#' # Define a trainable closure generator: standardization of selected columns
#' # note the explicit argument `x`: it is not strictly necessary but clarifies intent
#' gen_standardize <- function(cols, x, ...) {
#'     train_means <- colMeans(x[, ..cols], na.rm = TRUE)
#'     train_sds <- apply(x[, ..cols], 2, sd, na.rm = TRUE)
#' 
#'     # train_means, train_sds and cols are captured in the closure
#'     function(x) {
#'         x_copy <- copy(x)  # avoid modifying original data
#'         for (col in cols) {
#'             x_copy[[col]] <- (x_copy[[col]] - train_means[col]) / train_sds[col]
#'         }
#'         return(x_copy)
#'     }
#' }
#'
#' raw_pipe <- list(
#'    on = "simple_dt_date",
#'   transforms = list(
#'       list(fun = "gen_standardize", cols = c("X1", "X2", "Y"))
#'   )
#' )
#' 
#' parsed <- parse_single_pipe(raw_pipe)
#' print(parsed$transforms[[1]]$forward(simple_dt_date))
#' 
#' # Example 4: Using environments to control scope ------------------------------------------------
#' 
#' # so far all data and functions are available in the global environment, but this is not
#' # necessarily going to happen in practice. The following example illustrates how to use
#' # environments to control scope.
#' 
#' # Create new environment functions
#' env <- new.env()
#' assign("gen_standardize_2", gen_standardize, envir = env)
#' 
#' # create a named list of data
#' data_list <- list(dt = simple_dt_date)
#' 
#' raw_pipe <- list(
#'    on = "dt",
#'   transforms = list(
#'       list(fun = "gen_standardize", cols = c("X1", "X2", "Y"))
#'   )
#' )
#'
#' # This is a preview of how the pipe framework is used to maintain consistency between training
#' # and inference
#' # By defining "on" as "dt", the data used in parsing and evaluation can be swapped easily
#' # without changing the pipe structure.
#' parsed <- parse_single_pipe(raw_pipe, env = data_list, enclos = env)
#' 
#' # exactly the same as previous example
#' print(parsed$transforms[[1]]$forward(data_list$dt))
#' 
#' @export

parse_single_pipe <- function(raw_pipe, env = parent.frame(), enclos = parent.frame()) {
    args <- lapply(raw_pipe$on, str2lang)
    names(args) <- c("x", "y")[seq_along(args)]

    l_t <- raw_pipe$transforms

    cc <- c(l_t[[1]], args)
    cc[[1]] <- str2lang(cc[[1]])
    result <- eval(as.call(cc), env, enclos)
    raw_pipe$transforms[[1]] <- new_shapeshiftr_closure(result)

    if (length(l_t) >= 2) {
        x <- eval(as.call(c(list(raw_pipe$transforms[[1]]$forward), args)), env, enclos)
        for (i in seq_along(l_t)[-1]) {
            cc <- c(l_t[[i]], list(x = x))
            cc[[1]] <- str2lang(cc[[1]])
            result <- eval(as.call(cc), env, enclos)
            raw_pipe$transforms[[i]] <- new_shapeshiftr_closure(result)
            x <- eval(as.call(c(list(raw_pipe$transforms[[i]]$forward), list(x = x))), env, enclos)
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
#' @seealso \code{\link{parse_single_pipe}} for details on closure generators and pipe structure
#' 
#' @export

parse_pipes <- function(raw_pipes, env = parent.frame(), enclos = parent.frame()) {
    lapply(raw_pipes, parse_single_pipe, env = env, enclos = enclos)
}

# EVALUATE PIPES -----------------------------------------------------------------------------------

#' Internal Helper for Evaluating Transformation Closures
#' 
#' Internal function that applies a sequence of transformation closures, not intended for direct use
#' 
#' This function expects \code{pipe$transforms} to contain raw function 
#' closures (not shapeshiftr_closure objects). It is called internally by
#' \code{forward_single_pipe()} and \code{backward_single_pipe()} after
#' they extract the appropriate closures.
#' 
#' @param pipe Pipe with raw function closures in \code{transforms}
#' @param env Environment for evaluation
#' @param enclos Enclosing environment for closures
#' 
#' @return Transformed data
#' 
#' @seealso 
#' * \code{\link{forward_single_pipe}} for applying forward transformations
#' * \code{\link{backward_single_pipe}} for applying backward transformations
#' 
#' @keywords internal

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

#' Internal Helper for Evaluating Multiple Pipes
#' 
#' Internal wrapper that loops `eval_single_pipe()` to multiple pipes, not intended for direct use
#' 
#' @param pipes List of pipes with raw function closures
#' @param env Environment for evaluation
#' @param enclos Enclosing environment for closures
#' 
#' @return List of transformed data
#' 
#' @seealso 
#' * \code{\link{forward_pipes}} for applying forward transformations
#' * \code{\link{backward_pipes}} for applying backward transformations
#' 
#' @keywords internal

eval_pipes <- function(pipes, env = parent.frame(), enclos = parent.frame()) {
    lapply(pipes, eval_single_pipe, env = env, enclos = enclos)
}

#' Combination of Pipe Results
#' 
#' Combines outputs from pipes resulting from a call to `eval_pipes`
#' 
#' @details
#' ## Default Merge Behavior
#' 
#' By default, \code{combine_pipes} applies transformations sequentially,
#' merging results by the first column. This creates a composite transformation
#' equivalent to function composition: \code{f3(f2(f1(x)))}.
#' 
#' ## Custom Combination
#' 
#' Provide \code{combine_fun} parameter to control how pipe outputs are merged:
#' 
#' * **Sequential**: Default behavior (function composition)
#' * **Parallel**: Apply pipes independently and cbind/rbind results
#' * **Custom logic**: Any merging strategy via custom function
#' 
#' The \code{combine_fun} should accept two arguments (outputs of consecutive pipes)
#' and return a combined result.
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
#' @seealso
#' * \code{\link{eval_pipes}} for creating evaluation results to combine
#' * \code{\link{parse_pipes}} for creating multiple pipes
#' 
#' @examples
#' # Example 1: Default merge for sequential transformations ----
#' # Chain multiple transformations using built-in merge
#' 
#' library(data.table)
#' data(simple_dt_date)
#' 
#' # Define transformation generators
#' gen_log_transform <- function(...) {
#'     function(x) log(abs(x) + 1)
#' }
#' 
#' gen_standardize <- function(...) {
#'     train_data <- list(...)[[1]]
#'     train_mean <- mean(train_data, na.rm = TRUE)
#'     train_sd <- sd(train_data, na.rm = TRUE)
#'     function(x) (x - train_mean) / train_sd
#' }
#' 
#' # Create raw pipes
#' raw_pipes <- list(
#'     list(
#'         on = "simple_dt_date$X1",
#'         transforms = list(list(fun = "gen_log_transform"))
#'     ),
#'     list(
#'         on = "simple_dt_date$X2",
#'         transforms = list(list(fun = "gen_standardize"))
#'     )
#' )
#' 
#' # Parse pipes
#' parsed <- parse_pipes(raw_pipes)
#' 
#' # Evaluate pipes
#' evals <- eval_pipes(parsed)
#' 
#' # Combine with default merge
#' combined <- combine_pipes(evals)
#' 
#' print(head(combined))
#' 
#' # Default merge creates data.table with results merged by index
#' 
#' # Example 2: Custom combine function for flexible composition ----
#' # Provide custom function to control how pipes are combined
#' 
#' # Custom combine: cbind results with custom names
#' custom_combine <- function(x, y, ...) {
#'     if (is.numeric(x) && is.numeric(y)) {
#'         data.table(transformed_1 = x, transformed_2 = y)
#'     } else {
#'         cbind(x, y)
#'     }
#' }
#' 
#' combined_custom <- combine_pipes(evals, combine_fun = custom_combine)
#' 
#' print(head(combined_custom))
#' 
#' # Custom combine enables:
#' # - Parallel transformations
#' # - Feature concatenation
#' # - Complex composition patterns
#' 
#' @export

combine_pipes <- function(evals, combine_fun = default_combine, ...) {
    Reduce(function(x, y) combine_fun(x, y, ...), evals)
}

default_combine <- function(x, y, ...) {
    merge(x, y, by = 1)
}

# FORWARD/BACKWARD EVALUATION ----------------------------------------------------------------------

#' Apply Forward Transformations to Single Pipe
#'
#' Extracts forward closures from a parsed pipe and applies them sequentially
#' using \code{eval_single_pipe()}. This is the recommended way to apply
#' forward transformations.
#'
#' @details
#' This function provides a clearer interface than the deprecated
#' \code{eval_single_pipe()} by making the directionality of transformations
#' explicit. It extracts the \code{$forward} component from each
#' \code{shapeshiftr_closure} object in the pipe's transforms, then delegates
#' to \code{eval_single_pipe()} for execution.
#'
#' For reversible transformations, use \code{backward_single_pipe()} to apply
#' the inverse transformations in reverse order.
#'
#' @param pipe A pipe parsed by \code{parse_single_pipe()}
#' @param env Environment where the pipe will be evaluated
#' @param enclos Enclosing environment for evaluation of the closures
#'
#' @return Result of applying the forward closures in \code{transforms} to the
#'     data in \code{"on"}
#'
#' @seealso
#' * \code{\link{backward_single_pipe}} for applying inverse transformations
#' * \code{\link{forward_pipes}} for evaluating multiple pipes
#' * \code{\link{parse_single_pipe}} for creating parsed pipes
#' * \code{\link{eval_single_pipe}} (deprecated) for legacy evaluation
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' data(simple_dt_date)
#'
#' gen_standardize <- function(cols, x, ...) {
#'     train_means <- colMeans(x[, ..cols], na.rm = TRUE)
#'     train_sds <- apply(x[, ..cols], 2, sd, na.rm = TRUE)
#'     function(x) {
#'         x_copy <- copy(x)
#'         for (col in cols) {
#'             x_copy[[col]] <- (x_copy[[col]] - train_means[col]) / train_sds[col]
#'         }
#'         return(x_copy)
#'     }
#' }
#'
#' data_list <- list(data = simple_dt_date)
#' raw_pipe <- list(
#'     on = "data",
#'     transforms = list(list(fun = "gen_standardize", cols = c("X1", "X2")))
#' )
#'
#' parsed <- parse_single_pipe(raw_pipe, env = data_list)
#' result <- forward_single_pipe(parsed, env = data_list)
#' }
#'
#' @export

forward_single_pipe <- function(pipe, env = parent.frame(), enclos = parent.frame()) {
    temp_pipe <- pipe
    temp_pipe$transforms <- lapply(pipe$transforms, function(x) x$forward)
    eval_single_pipe(temp_pipe, env = env, enclos = enclos)
}

#' Apply Backward Transformations to Single Pipe
#'
#' Extracts backward closures from a parsed pipe, reverses their order, and
#' applies them sequentially using \code{eval_single_pipe()}. This enables
#' inverse transformations for reversible pipelines.
#'
#' @details
#' This function applies transformations in reverse by:
#' 1. Extracting the \code{$backward} component from each
#'    \code{shapeshiftr_closure} object
#' 2. Reversing the order of closures with \code{rev()}
#' 3. Delegating to \code{eval_single_pipe()} for execution
#'
#' The reversal is critical for correct inverse transformations. If the forward
#' pipe applies transformations \code{f1}, then \code{f2}, then \code{f3}, the
#' backward pipe applies \code{f3_inv}, then \code{f2_inv}, then \code{f1_inv}.
#'
#' For generators that don't provide explicit backward closures, the identity
#' function \code{function(x) x} is used by default.
#'
#' @param pipe A pipe parsed by \code{parse_single_pipe()}
#' @param env Environment where the pipe will be evaluated
#' @param enclos Enclosing environment for evaluation of the closures
#'
#' @return Result of applying the backward closures (in reverse order) to the
#'     data in \code{"on"}
#'
#' @seealso
#' * \code{\link{forward_single_pipe}} for applying forward transformations
#' * \code{\link{backward_pipes}} for evaluating multiple pipes
#' * \code{\link{parse_single_pipe}} for creating parsed pipes
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' data(simple_dt_date)
#'
#' gen_log_exp <- function(...) {
#'     list(
#'         forward = function(x) log(x),
#'         backward = function(x) exp(x)
#'     )
#' }
#'
#' data_list <- list(data = abs(simple_dt_date$X1) + 1)
#' raw_pipe <- list(
#'     on = "data",
#'     transforms = list(list(fun = "gen_log_exp"))
#' )
#'
#' parsed <- parse_single_pipe(raw_pipe, env = data_list)
#' forward_result <- forward_single_pipe(parsed, env = data_list)
#' backward_result <- backward_single_pipe(
#'     parsed,
#'     env = list(data = forward_result)
#' )
#' }
#'
#' @export

backward_single_pipe <- function(pipe, env = parent.frame(), enclos = parent.frame()) {
    temp_pipe <- pipe
    backwards <- lapply(pipe$transforms, function(x) x$backward)
    temp_pipe$transforms <- rev(backwards)
    eval_single_pipe(temp_pipe, env = env, enclos = enclos)
}

#' Apply Forward Transformations to Multiple Pipes
#'
#' Simple wrapper for applying \code{forward_single_pipe()} over multiple
#' pipes. This is the recommended way to apply forward transformations to
#' multiple pipes.
#'
#' @param pipes List of parsed pipes from \code{parse_pipes()}
#' @param env Environment where the pipes will be evaluated
#' @param enclos Enclosing environment for evaluation of the closures
#'
#' @return List of results from applying forward transformations to each pipe
#'
#' @seealso
#' * \code{\link{forward_single_pipe}} for single pipe evaluation
#' * \code{\link{backward_pipes}} for applying inverse transformations
#' * \code{\link{parse_pipes}} for creating multiple parsed pipes
#' * \code{\link{combine_pipes}} for combining evaluation results
#' * \code{\link{eval_pipes}} (deprecated) for legacy evaluation
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' data(simple_dt_date)
#'
#' gen_standardize <- function(cols, x, ...) {
#'     train_means <- colMeans(x[, ..cols], na.rm = TRUE)
#'     train_sds <- apply(x[, ..cols], 2, sd, na.rm = TRUE)
#'     function(x) {
#'         x_copy <- copy(x)
#'         for (col in cols) {
#'             x_copy[[col]] <- (x_copy[[col]] - train_means[col]) / train_sds[col]
#'         }
#'         return(x_copy)
#'     }
#' }
#'
#' data_list <- list(data = simple_dt_date)
#' raw_pipes <- list(
#'     list(on = "data", transforms = list(list(fun = "gen_standardize", cols = "X1"))),
#'     list(on = "data", transforms = list(list(fun = "gen_standardize", cols = "X2")))
#' )
#'
#' parsed <- parse_pipes(raw_pipes, env = data_list)
#' results <- forward_pipes(parsed, env = data_list)
#' }
#'
#' @export

forward_pipes <- function(pipes, env = parent.frame(), enclos = parent.frame()) {
    lapply(pipes, forward_single_pipe, env = env, enclos = enclos)
}

#' Apply Backward Transformations to Multiple Pipes
#'
#' Simple wrapper for applying \code{backward_single_pipe()} over multiple
#' pipes. This enables inverse transformations for multiple reversible
#' pipelines.
#'
#' @param pipes List of parsed pipes from \code{parse_pipes()}
#' @param env Environment where the pipes will be evaluated
#' @param enclos Enclosing environment for evaluation of the closures
#'
#' @return List of results from applying backward transformations to each pipe
#'
#' @seealso
#' * \code{\link{backward_single_pipe}} for single pipe evaluation
#' * \code{\link{forward_pipes}} for applying forward transformations
#' * \code{\link{parse_pipes}} for creating multiple parsed pipes
#' * \code{\link{combine_pipes}} for combining evaluation results
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' data(simple_dt_date)
#'
#' gen_log_exp <- function(...) {
#'     list(
#'         forward = function(x) log(abs(x) + 1),
#'         backward = function(x) exp(x) - 1
#'     )
#' }
#'
#' data_list <- list(data = simple_dt_date$X1)
#' raw_pipes <- list(
#'     list(on = "data", transforms = list(list(fun = "gen_log_exp")))
#' )
#'
#' parsed <- parse_pipes(raw_pipes, env = data_list)
#' forward_results <- forward_pipes(parsed, env = data_list)
#' backward_results <- backward_pipes(
#'     parsed,
#'     env = list(data = forward_results[[1]])
#' )
#' }
#'
#' @export

backward_pipes <- function(pipes, env = parent.frame(), enclos = parent.frame()) {
    lapply(pipes, backward_single_pipe, env = env, enclos = enclos)
}