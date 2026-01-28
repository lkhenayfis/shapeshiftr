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
#' ## Reversible Transformations
#' 
#' All generator results are automatically wrapped in `shapeshiftr_closure` objects,
#' which store both forward and backward transformation closures:
#' 
#' - **Single closure generators**: Return a single function. This is wrapped with an
#'   identity backward function: `list(forward = result, backward = function(x) x)`
#' - **Paired closure generators**: Return a list with named elements `forward` and
#'   `backward`. This is wrapped as-is, preserving both closures for true reversibility.
#' 
#' The wrapping is transparent - parsed pipes can be used with `forward_single_pipe()`
#' to apply forward transformations or `backward_single_pipe()` to apply inverse
#' transformations.
#' 
#' ## Environment Sharing
#' 
#' For paired closure generators, the forward and backward closures must share the
#' same environment (verified with `identical()`). This ensures both closures access
#' the same captured parameters (e.g., training means and SDs).
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
#' * \code{\link{forward_single_pipe}}, \code{\link{backward_single_pipe}} for applying
#'   forward/backward transformations
#' * \code{\link{eval_single_pipe}} for applying parsed closures to data
#' * \code{\link{parse_pipes}} for parsing multiple pipes
#' * \code{\link{combine_pipes}} for combining multiple parsed pipes
#' * \code{\link{new_shapeshiftr_closure}} (internal constructor)
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
#' # Example 5: Reversible generator with forward and backward closures ---------------------------
#' 
#' # Define a reversible generator that returns paired closures
#' gen_scale <- function(x, ...) {
#'     train_mean <- mean(x, na.rm = TRUE)
#'     train_sd <- sd(x, na.rm = TRUE)
#'     
#'     forward <- function(x) {
#'         (x - train_mean) / train_sd
#'     }
#'     
#'     backward <- function(x) {
#'         (x * train_sd) + train_mean
#'     }
#'     
#'     # Return paired closures for reversibility
#'     list(forward = forward, backward = backward)
#' }
#' 
#' # Parse with reversible generator
#' train_data <- c(10, 20, 30, 40, 50)
#' data_list <- list(data = train_data)
#' raw_pipe <- list(
#'     on = "data",
#'     transforms = list(list(fun = "gen_scale"))
#' )
#' 
#' parsed <- parse_single_pipe(raw_pipe, env = data_list)
#' 
#' # Apply forward transformation
#' standardized <- forward_single_pipe(parsed, env = data_list)
#' print(standardized)  # Mean approximately 0, SD approximately 1
#' 
#' # Apply backward transformation (roundtrip)
#' reconstructed <- backward_single_pipe(parsed, env = list(data = standardized))
#' all.equal(train_data, reconstructed)  # TRUE (within numerical tolerance)
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
#' Combines outputs from pipes resulting from a call to `forward_pipes`
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
#' @param evals list of elements to be combined, typically the result of `forward_pipes`
#' @param combine_fun combination function to be applied, by default `default_combine`. See
#'     Details
#' @param ... additional arguments to be passed to `combine_fun`
#' 
#' @return result of the combination
#' 
#' @seealso
#' * \code{\link{forward_pipes}} for creating evaluation results to combine
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
#' evals <- forward_pipes(parsed)
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

#' Apply Forward or Backward Transformations
#'
#' Apply transformations from a parsed pipe in the forward (original) or backward (inverse) direction
#'
#' `forward_single_pipe()` extracts the forward closures from each
#' transformation and applies them sequentially. This is the standard way
#' to apply learned transformations to new data.
#'
#' `backward_single_pipe()` extracts the backward (inverse) closures,
#' reverses their order, and applies them to reconstruct the original data.
#' This enables perfect roundtrips: `backward(forward(x)) ≈ x`.
#'
#' The backward function applies transformations in reverse order. If forward
#' applies f1 → f2 → f3, backward applies f3_inv → f2_inv → f1_inv.
#'
#' ## Train-Test Consistency:
#' 
#' Both forward and backward transformations use parameters learned during
#' the parse phase (on training data). This ensures test data is transformed
#' using training statistics, preventing data leakage.
#'
#' Example: If a scaling transformation learns mean=5 and sd=2 from training
#' data, those same parameters are used when transforming test data, even if
#' the test data has different mean/sd.
#'
#' ## Roundtrip Validation:
#' 
#' For reversible transformations, applying backward after forward should
#' reconstruct the original data within numerical precision:
#' \preformatted{
#'   # Parse on training data
#'   parsed <- parse_single_pipe(raw_pipe, env = train_env)
#'   
#'   # Forward transformation
#'   transformed <- forward_single_pipe(parsed, env = train_env)
#'   
#'   # Backward transformation
#'   reconstructed <- backward_single_pipe(parsed, env = list(data = transformed))
#'   
#'   # Verify roundtrip
#'   all.equal(train_data, reconstructed)  # Should be TRUE (within tolerance)
#' }
#'
#' @param pipe Pipe parsed by `parse_single_pipe`
#' @param env Environment where data objects are found
#' @param enclos Enclosing environment for closure evaluation
#'
#' @return Transformed data (forward) or reconstructed data (backward)
#'
#' @seealso
#' * [`forward_pipes()`], [`backward_pipes()`] for multiple pipes
#' * [`parse_single_pipe()`] for creating parsed pipes
#' * [`eval_single_pipe()`] (internal helper)
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' data(simple_dt_date)
#'
#' # Example 1: Basic Forward/Backward Usage -----------------------------------------------------
#' # Simple single-transform pipeline with forward, backward, and roundtrip validation
#'
#' # Define a reversible log1p generator
#' gen_log1p <- function(x, ...) {
#'     forward <- function(x) {
#'         log1p(abs(x))
#'     }
#'     
#'     backward <- function(x) {
#'         expm1(x)
#'     }
#'     
#'     list(forward = forward, backward = backward)
#' }
#'
#' # Setup data with a single numeric vector
#' original_data <- c(0, 1, 10, 100, 1000)
#' data_env <- list(data = original_data)
#'
#' # Parse pipeline
#' raw_pipe <- list(
#'     on = "data",
#'     transforms = list(list(fun = "gen_log1p"))
#' )
#' parsed <- parse_single_pipe(raw_pipe, env = data_env)
#'
#' # Forward transformation
#' transformed <- forward_single_pipe(parsed, env = data_env)
#' cat("Original:", original_data, "\n")
#' cat("Transformed:", transformed, "\n")
#'
#' # Backward transformation
#' reconstructed <- backward_single_pipe(parsed, env = list(data = transformed))
#' cat("Reconstructed:", reconstructed, "\n")
#'
#' # Roundtrip validation
#' roundtrip_error <- max(abs(original_data - reconstructed))
#' cat("Roundtrip error:", roundtrip_error, "\n")
#' stopifnot(roundtrip_error < 1e-10)
#'
#'
#' # Example 2: Train-Test Workflow --------------------------------------------------------------
#' # Parse on training data, apply to both train and test with consistent parameters
#'
#' # Define standardization generator
#' gen_standardize <- function(cols, x, ...) {
#'     train_means <- colMeans(x[, ..cols], na.rm = TRUE)
#'     train_sds <- apply(x[, ..cols], 2, sd, na.rm = TRUE)
#'     
#'     forward <- function(x) {
#'         x_copy <- copy(x)
#'         for (col in cols) {
#'             x_copy[[col]] <- (x_copy[[col]] - train_means[col]) / train_sds[col]
#'         }
#'         return(x_copy)
#'     }
#'     
#'     backward <- function(x) {
#'         x_copy <- copy(x)
#'         for (col in cols) {
#'             x_copy[[col]] <- (x_copy[[col]] * train_sds[col]) + train_means[col]
#'         }
#'         return(x_copy)
#'     }
#'     
#'     list(forward = forward, backward = backward)
#' }
#'
#' # Create train and test datasets
#' train_data <- copy(simple_dt_date)
#' test_data <- copy(simple_dt_date)
#' test_data$X1 <- test_data$X1 + 10  # Different distribution
#' test_data$X2 <- test_data$X2 * 2
#'
#' # Parse on training data (learns parameters)
#' train_env <- list(data = train_data)
#' raw_pipe <- list(
#'     on = "data",
#'     transforms = list(list(fun = "gen_standardize", cols = c("X1", "X2")))
#' )
#' parsed <- parse_single_pipe(raw_pipe, env = train_env)
#'
#' # Apply forward to training data
#' train_standardized <- forward_single_pipe(parsed, env = train_env)
#' cat("Train means after standardization:", 
#'     colMeans(train_standardized[, .(X1, X2)]), "\n")
#' # Should be approximately [0, 0]
#'
#' # Apply forward to test data (uses TRAINING parameters)
#' test_env <- list(data = test_data)
#' test_standardized <- forward_single_pipe(parsed, env = test_env)
#' cat("Test means after standardization:", 
#'     colMeans(test_standardized[, .(X1, X2)]), "\n")
#' # Will NOT be [0, 0] because test data has different distribution
#'
#' # Verify parameters are consistent (both use train stats)
#' cat("This demonstrates train-test consistency: test uses training parameters\n")
#'
#'
#' # Example 3: Multi-Transform Chain -------------------------------------------------------------
#' # Chain 2-3 transformations, verify order matters, test backward reversal
#'
#' # Define scaling generator
#' gen_scale <- function(x, factor = 2, ...) {
#'     forward <- function(x) {
#'         x * factor
#'     }
#'     
#'     backward <- function(x) {
#'         x / factor
#'     }
#'     
#'     list(forward = forward, backward = backward)
#' }
#'
#' # Define shift generator
#' gen_shift <- function(x, offset = 10, ...) {
#'     forward <- function(x) {
#'         x + offset
#'     }
#'     
#'     backward <- function(x) {
#'         x - offset
#'     }
#'     
#'     list(forward = forward, backward = backward)
#' }
#'
#' # Chain: scale -> log1p -> shift
#' original_vec <- c(1, 2, 3, 4, 5)
#' data_env <- list(data = original_vec)
#'
#' raw_pipe <- list(
#'     on = "data",
#'     transforms = list(
#'         list(fun = "gen_scale", factor = 10),
#'         list(fun = "gen_log1p"),
#'         list(fun = "gen_shift", offset = 100)
#'     )
#' )
#' parsed <- parse_single_pipe(raw_pipe, env = data_env)
#'
#' # Forward chain: scale(10) -> log1p -> shift(100)
#' forward_result <- forward_single_pipe(parsed, env = data_env)
#' cat("Original:", original_vec, "\n")
#' cat("After scale->log1p->shift:", forward_result, "\n")
#'
#' # Backward chain (automatic reversal): shift_inv(100) -> log1p_inv -> scale_inv(10)
#' backward_result <- backward_single_pipe(parsed, env = list(data = forward_result))
#' cat("After backward chain:", backward_result, "\n")
#'
#' # Verify roundtrip
#' chain_error <- max(abs(original_vec - backward_result))
#' cat("Chain roundtrip error:", chain_error, "\n")
#' stopifnot(chain_error < 1e-10)
#'
#' # Verify order matters: compare with different order
#' raw_pipe_reversed <- list(
#'     on = "data",
#'     transforms = list(
#'         list(fun = "gen_shift", offset = 100),
#'         list(fun = "gen_log1p"),
#'         list(fun = "gen_scale", factor = 10)
#'     )
#' )
#' parsed_reversed <- parse_single_pipe(raw_pipe_reversed, env = data_env)
#' forward_reversed <- forward_single_pipe(parsed_reversed, env = data_env)
#' cat("Different order produces different results:", 
#'     !isTRUE(all.equal(forward_result, forward_reversed)), "\n")
#'
#'
#' # Example 4: Roundtrip Validation Pattern -----------------------------------------------------
#' # Helper function to validate roundtrips across multiple generators and edge cases
#'
#' # Roundtrip validation helper
#' validate_roundtrip <- function(generator_name, generator_fun, test_data, 
#'                                 tolerance = 1e-10, ...) {
#'     data_env <- list(data = test_data)
#'     
#'     raw_pipe <- list(
#'         on = "data",
#'         transforms = list(list(fun = generator_name, ...))
#'     )
#'     
#'     # Make generator available in current environment
#'     assign(generator_name, generator_fun, envir = environment())
#'     
#'     parsed <- parse_single_pipe(raw_pipe, env = data_env, enclos = environment())
#'     forward_result <- forward_single_pipe(parsed, env = data_env, enclos = environment())
#'     backward_result <- backward_single_pipe(parsed, 
#'         env = list(data = forward_result), enclos = environment())
#'     
#'     error <- max(abs(test_data - backward_result), na.rm = TRUE)
#'     
#'     list(
#'         passed = error < tolerance,
#'         error = error,
#'         tolerance = tolerance
#'     )
#' }
#'
#' # Test multiple generators
#' test_cases <- list(
#'     normal = c(1, 2, 3, 4, 5),
#'     with_zero = c(0, 1, 2, 3),
#'     negative = c(-5, -2, 0, 2, 5),
#'     large_values = c(1e6, 1e7, 1e8)
#' )
#'
#' # Test log1p generator
#' cat("Testing gen_log1p:\n")
#' for (case_name in names(test_cases)) {
#'     result <- validate_roundtrip("gen_log1p", gen_log1p, test_cases[[case_name]])
#'     cat(sprintf("  %s: %s (error: %.2e)\n", 
#'         case_name, 
#'         ifelse(result$passed, "PASS", "FAIL"),
#'         result$error))
#' }
#'
#' # Test scale generator
#' cat("\nTesting gen_scale:\n")
#' for (case_name in names(test_cases)) {
#'     result <- validate_roundtrip("gen_scale", gen_scale, 
#'         test_cases[[case_name]], factor = 3.14)
#'     cat(sprintf("  %s: %s (error: %.2e)\n", 
#'         case_name, 
#'         ifelse(result$passed, "PASS", "FAIL"),
#'         result$error))
#' }
#'
#'
#' # Example 5: Real-World Scenario --------------------------------------------------------------
#' # Data.frame with multiple columns, different transformations per column, train-test split
#'
#' # Create realistic dataset
#' set.seed(123)
#' n_train <- 100
#' n_test <- 30
#'
#' full_data <- data.table(
#'     id = 1:(n_train + n_test),
#'     price = exp(rnorm(n_train + n_test, mean = 5, sd = 1)),  # Log-normal prices
#'     quantity = rpois(n_train + n_test, lambda = 50),  # Count data
#'     temperature = rnorm(n_train + n_test, mean = 20, sd = 5)  # Normal temp
#' )
#'
#' train_df <- full_data[1:n_train]
#' test_df <- full_data[(n_train + 1):(n_train + n_test)]
#'
#' # Define column-specific generators
#' gen_log_scale <- function(cols, x, ...) {
#'     train_means <- colMeans(log1p(x[, ..cols]), na.rm = TRUE)
#'     train_sds <- apply(log1p(x[, ..cols]), 2, sd, na.rm = TRUE)
#'     
#'     forward <- function(x) {
#'         x_copy <- copy(x)
#'         for (col in cols) {
#'             log_val <- log1p(x_copy[[col]])
#'             x_copy[[col]] <- (log_val - train_means[col]) / train_sds[col]
#'         }
#'         return(x_copy)
#'     }
#'     
#'     backward <- function(x) {
#'         x_copy <- copy(x)
#'         for (col in cols) {
#'             scaled_back <- (x_copy[[col]] * train_sds[col]) + train_means[col]
#'             x_copy[[col]] <- expm1(scaled_back)
#'         }
#'         return(x_copy)
#'     }
#'     
#'     list(forward = forward, backward = backward)
#' }
#'
#' # Parse different transformations for different columns
#' raw_pipes <- list(
#'     # Price: log-transform then standardize
#'     list(
#'         on = "data",
#'         transforms = list(list(fun = "gen_log_scale", cols = "price"))
#'     ),
#'     # Quantity and temperature: just standardize
#'     list(
#'         on = "data",
#'         transforms = list(
#'             list(fun = "gen_standardize", cols = c("quantity", "temperature"))
#'         )
#'     )
#' )
#'
#' # Parse on training data
#' train_env <- list(data = train_df)
#' parsed_pipes <- parse_pipes(raw_pipes, env = train_env)
#'
#' # Forward transformation for model training
#' train_transformed_list <- forward_pipes(parsed_pipes, env = train_env)
#'
#' # Combine transformed columns
#' train_transformed <- train_df[, .(id)]
#' train_transformed[, price := train_transformed_list[[1]]$price]
#' train_transformed[, c("quantity", "temperature") := 
#'     train_transformed_list[[2]][, .(quantity, temperature)]]
#'
#' cat("Training data transformed:\n")
#' cat("  Price mean:", mean(train_transformed$price), 
#'     "sd:", sd(train_transformed$price), "\n")
#' cat("  Quantity mean:", mean(train_transformed$quantity), 
#'     "sd:", sd(train_transformed$quantity), "\n")
#'
#' # Apply to test data (uses training parameters)
#' test_env <- list(data = test_df)
#' test_transformed_list <- forward_pipes(parsed_pipes, env = test_env)
#'
#' test_transformed <- test_df[, .(id)]
#' test_transformed[, price := test_transformed_list[[1]]$price]
#' test_transformed[, c("quantity", "temperature") := 
#'     test_transformed_list[[2]][, .(quantity, temperature)]]
#'
#' cat("\nTest data transformed (using training params):\n")
#' cat("  Price mean:", mean(test_transformed$price), 
#'     "sd:", sd(test_transformed$price), "\n")
#'
#' # Backward transformation for model interpretation
#' # Suppose model predicted on transformed scale, convert back to original
#' predictions_transformed <- test_transformed[, .(id, price, quantity, temperature)]
#'
#' # Apply backward to reconstruct original scale
#' backward_list <- backward_pipes(parsed_pipes, 
#'     env = list(data = predictions_transformed))
#'
#' predictions_original <- predictions_transformed[, .(id)]
#' predictions_original[, price := backward_list[[1]]$price]
#' predictions_original[, c("quantity", "temperature") := 
#'     backward_list[[2]][, .(quantity, temperature)]]
#'
#' cat("\nPredictions back-transformed to original scale:\n")
#' cat("  Original test price range:", 
#'     range(test_df$price), "\n")
#' cat("  Reconstructed price range:", 
#'     range(predictions_original$price), "\n")
#'
#' # Verify roundtrip accuracy
#' price_error <- max(abs(test_df$price - predictions_original$price))
#' quantity_error <- max(abs(test_df$quantity - predictions_original$quantity))
#' cat("\nRoundtrip errors:\n")
#' cat("  Price:", price_error, "\n")
#' cat("  Quantity:", quantity_error, "\n")
#' cat("  All errors < 1e-9:", 
#'     price_error < 1e-9 && quantity_error < 1e-9, "\n")
#' }
#'
#' @rdname forward_backward_single_pipe
#' 
#' @export

forward_single_pipe <- function(pipe, env = parent.frame(), enclos = parent.frame()) {
    temp_pipe <- pipe
    temp_pipe$transforms <- lapply(pipe$transforms, function(x) x$forward)
    eval_single_pipe(temp_pipe, env = env, enclos = enclos)
}

#' @rdname forward_backward_single_pipe
#' 
#' @export

backward_single_pipe <- function(pipe, env = parent.frame(), enclos = parent.frame()) {
    temp_pipe <- pipe
    backwards <- lapply(pipe$transforms, function(x) x$backward)
    temp_pipe$transforms <- rev(backwards)
    eval_single_pipe(temp_pipe, env = env, enclos = enclos)
}

#' Apply Forward or Backward Transformations to Multiple Pipes
#'
#' @description
#' Apply transformations to multiple pipes in the forward (original) or
#' backward (inverse) direction. These are simple \code{lapply} wrappers
#' over \code{forward_single_pipe()} and \code{backward_single_pipe()}.
#'
#' @details
#' \code{forward_pipes()} applies forward transformations to each pipe in
#' the list, returning a list of transformed data.
#'
#' \code{backward_pipes()} applies backward transformations to each pipe,
#' returning a list of reconstructed data.
#'
#' These functions are useful when working with multiple related pipelines
#' (e.g., transforming different datasets with different configurations).
#'
#' @param pipes List of pipes parsed by \code{\link{parse_pipes}}
#' @param env Environment where data objects are found
#' @param enclos Enclosing environment for closure evaluation
#'
#' @return List of transformed (forward) or reconstructed (backward) data
#'
#' @seealso
#' * \code{\link{forward_single_pipe}}, \code{\link{backward_single_pipe}}
#' * \code{\link{parse_pipes}} for creating multiple parsed pipes
#' * \code{\link{eval_pipes}} (internal helper)
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' data(simple_dt_date)
#'
#' # Multiple pipes for different column sets
#' data_list <- list(data = simple_dt_date)
#' raw_pipes <- list(
#'     list(
#'         on = "data",
#'         transforms = list(list(fun = "gen_standardize", cols = "X1"))
#'     ),
#'     list(
#'         on = "data",
#'         transforms = list(list(fun = "gen_standardize", cols = "X2"))
#'     )
#' )
#'
#' # Parse all pipes
#' parsed <- parse_pipes(raw_pipes, env = data_list)
#'
#' # Forward transformations on all pipes
#' results <- forward_pipes(parsed, env = data_list)
#' cat("Number of transformed datasets:", length(results), "\n")
#'
#' # Backward transformations (roundtrip)
#' reconstructed <- backward_pipes(parsed, env = list(data = results[[1]]))
#' cat("Roundtrip successful:", all.equal(data_list$data, reconstructed[[1]]), "\n")
#' }
#'
#' @rdname forward_backward_pipes
#' @export

forward_pipes <- function(pipes, env = parent.frame(), enclos = parent.frame()) {
    lapply(pipes, forward_single_pipe, env = env, enclos = enclos)
}

#' @rdname forward_backward_pipes
#' @export

backward_pipes <- function(pipes, env = parent.frame(), enclos = parent.frame()) {
    lapply(pipes, backward_single_pipe, env = env, enclos = enclos)
}