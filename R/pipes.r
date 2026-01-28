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

#' Evaluate a Single Pipe
#' 
#' Applies the closures in a `pipe` already parsed to the data defined in `"on"`
#' 
#' @details
#' ## Parse-Eval Separation
#'
#' The pipes system separates transformation into two phases:
#'
#' * **Parse** (\code{parse_single_pipe}): Generate closures for transformations
#' * **Eval** (\code{eval_single_pipe}): Apply closures to data
#'
#' This separation ensures train-test consistency: parameters are learned once
#' from training data, then applied identically to train, test, and production data.
#'
#' **Recommended Workflow with Named Lists**:
#' \preformatted{
#' # 1. Create named list with training data
#' data_list_train <- list(data = train_data)
#'
#' # 2. Define raw pipe referencing list element
#' raw_pipe <- list(
#'     on = "data",
#'     transforms = list(list(fun = "gen_standardize", cols = c("X1", "X2")))
#' )
#'
#' # 3. Parse on train: learn parameters
#' parsed <- parse_single_pipe(raw_pipe, env = data_list_train)
#'
#' # 4. Eval on train: transform with learned params
#' train_result <- eval_single_pipe(parsed, env = data_list_train)
#'
#' # 5. Define a test set of data
#' data_list_test <- list(data = test_data)
#'
#' # 6. Eval on test: SAME params, different data
#' test_result <- eval_single_pipe(parsed, env = data_list_test)
#' }
#'
#' This pattern enables easy data swapping without modifying the pipe structure.
#' See Example 2 for a complete demonstration of train-test consistency.
#'
#' ## Environment Parameters: `env` and `enclos`
#'
#' Both `env` and `enclos` function identically to their counterparts in base R's `eval()`:
#'
#' * **`env`**: Where to look up data references (e.g., `"data"` in `on = "data"`)
#'   - Can be a named list, data.frame, or environment
#'   - Controls DATA scope
#'
#' * **`enclos`**: Where to look up function references (e.g., `"gen_standardize"`)
#'   - Must be an environment
#'   - Controls FUNCTION scope
#'
#' This separation enables:
#' - Clean data swapping (modify `env` without touching `enclos`)
#' - Isolated function namespaces (useful for package development)
#' - Flexible deployment patterns (see Example 5)
#'
#' ## When to Use Single vs Plural
#'
#' * \code{parse_single_pipe} / \code{eval_single_pipe}: One transformation
#' * \code{parse_pipes} / \code{eval_pipes}: Multiple transformations (returns lists)
#'
#' Plural functions are convenience wrappers for multi-pipe workflows.
#'
#' @param pipe a pipe parsed by `parse_single_pipe`
#' @param env environment where the pipe will be evaluated
#' @param enclos enclosing environment for evaluation of the closures
#' 
#' @return result of applying the closures in `transforms` to the data in `"on"`
#' 
#' @seealso
#' * \code{\link{parse_single_pipe}} for creating parsed pipes
#' * \code{\link{eval_pipes}} for evaluating multiple pipes
#' * \code{\link{combine_pipes}} for combining evaluation results
#' 
#' @examples
#'
#' # Example 1: Basic parse-eval workflow with env parameter ---------------------------------------
#'
#' library(data.table)
#' data(simple_dt_date)
#' 
#' # Define closure generator for standardization (same as parse_single_pipe Example 3)
#' gen_standardize <- function(cols, x, ...) {
#'     train_means <- colMeans(x[, ..cols], na.rm = TRUE)
#'     train_sds <- apply(x[, ..cols], 2, sd, na.rm = TRUE)
#' 
#'     function(x) {
#'         x_copy <- copy(x)
#'         for (col in cols) {
#'             x_copy[[col]] <- (x_copy[[col]] - train_means[col]) / train_sds[col]
#'         }
#'         return(x_copy)
#'     }
#' }
#' 
#' # Create named list containing data (recommended pattern)
#' data_list_train <- list(data = simple_dt_date)
#' 
#' # Define raw pipe using reference to list element
#' raw_pipe <- list(
#'     on = "data",
#'     transforms = list(
#'         list(fun = "gen_standardize", cols = c("X1", "X2", "Y"))
#'     )
#' )
#' 
#' # PARSE: Learn parameters from data
#' parsed <- parse_single_pipe(raw_pipe, env = data_list_train)
#' 
#' # EVAL: Apply learned transformation
#' result <- eval_single_pipe(parsed, env = data_list_train)
#' 
#' # Verify standardization
#' cat("Column X1 - Mean:", mean(result$X1), "SD:", sd(result$X1), "\n")
#' cat("Column X2 - Mean:", mean(result$X2), "SD:", sd(result$X2), "\n")
#' cat("Column Y - Mean:", mean(result$Y), "SD:", sd(result$Y), "\n")
#' 
#' # Example 2: Train-test consistency - CORE USE CASE AND PRIMARY VALUE PROPOSITION ---------------
#' 
#' # This example demonstrates the PRIMARY PURPOSE of the parse-eval separation:
#' # ensuring transformations use TRAINING parameters on both train and test data.
#' # This prevents data leakage and maintains statistical validity.
#' 
#' library(data.table)
#' data(simple_dt_date)
#' 
#' # Define closure generator (same as Example 1)
#' gen_standardize <- function(cols, x, ...) {
#'     train_means <- colMeans(x[, ..cols], na.rm = TRUE)
#'     train_sds <- apply(x[, ..cols], 2, sd, na.rm = TRUE)
#' 
#'     function(x) {
#'         x_copy <- copy(x)
#'         for (col in cols) {
#'             x_copy[[col]] <- (x_copy[[col]] - train_means[col]) / train_sds[col]
#'         }
#'         return(x_copy)
#'     }
#' }
#' 
#' # STEP 1: Split data into train and test sets
#' train_data <- simple_dt_date[1:15]
#' test_data <- simple_dt_date[16:20]
#' 
#' # Create named list with training data
#' data_list_train <- list(data = train_data)
#' 
#' # Define raw pipe
#' raw_pipe <- list(
#'     on = "data",
#'     transforms = list(
#'         list(fun = "gen_standardize", cols = c("X1", "X2"))
#'     )
#' )
#' 
#' # STEP 2: PARSE on training data - Learn parameters (mean, sd) from train set
#' parsed <- parse_single_pipe(raw_pipe, env = data_list_train)
#' 
#' # Extract and print captured training parameters for verification
#' closure <- parsed$transforms[[1]]$forward
#' closure_env <- environment(closure)
#' train_means <- get("train_means", closure_env)
#' train_sds <- get("train_sds", closure_env)
#' 
#' cat("\n=== CAPTURED TRAINING PARAMETERS ===\n")
#' cat("Training mean X1:", train_means["X1"], "\n")
#' cat("Training sd X1:", train_sds["X1"], "\n")
#' cat("Training mean X2:", train_means["X2"], "\n")
#' cat("Training sd X2:", train_sds["X2"], "\n")
#' 
#' # STEP 3: EVAL on training data - Apply transformation to train set
#' train_result <- eval_single_pipe(parsed, env = data_list_train)
#' 
#' cat("\n=== TRAINING SET TRANSFORMATION ===\n")
#' cat("Train X1 - Mean:", round(mean(train_result$X1), 10), "SD:", round(sd(train_result$X1), 6), "\n")
#' cat("Train X2 - Mean:", round(mean(train_result$X2), 10), "SD:", round(sd(train_result$X2), 6), "\n")
#' cat("(Should be mean ≈ 0, sd ≈ 1 because we used training params on training data)\n")
#' 
#' # STEP 4: EVAL on test data - Swap data in environment, apply SAME transformation
#' # This is THE KEY STEP: we use the SAME parsed pipe (with training parameters)
#' # but evaluate on different data
#' data_list_test <- list(data = test_data)
#' test_result <- eval_single_pipe(parsed, env = data_list_test)
#' 
#' cat("\n=== TEST SET TRANSFORMATION ===\n")
#' cat("Test X1 - Mean:", round(mean(test_result$X1), 6), "SD:", round(sd(test_result$X1), 6), "\n")
#' cat("Test X2 - Mean:", round(mean(test_result$X2), 6), "SD:", round(sd(test_result$X2), 6), "\n")
#' cat("(Mean ≠ 0, sd ≠ 1 because we used TRAINING params, not test params)\n")
#' 
#' # PROOF: Verify test transformation used TRAINING parameters, not test parameters
#' cat("\n=== PROOF OF TRAIN-TEST CONSISTENCY ===\n")
#' cat("Test data raw statistics:\n")
#' cat("  Test X1 mean (raw):", round(mean(test_data$X1), 6), "\n")
#' cat("  Test X1 sd (raw):", round(sd(test_data$X1), 6), "\n")
#' 
#' cat("\nManual verification (using TRAINING params):\n")
#' manual_transform <- (test_data$X1 - train_means["X1"]) / train_sds["X1"]
#' cat("  Manual transform mean:", round(mean(manual_transform), 6), "\n")
#' cat("  eval_single_pipe mean:", round(mean(test_result$X1), 6), "\n")
#' cat("  Match:", all.equal(manual_transform, test_result$X1), "\n")
#' 
#' cat("\nWhy this matters:\n")
#' cat("  - Prevents data leakage (test stats don't influence transformation)\n")
#' cat("  - Ensures reproducibility (same params in production)\n")
#' cat("  - Maintains statistical validity (test is truly held-out)\n")
#' 
#' # Example 3: Stacking multiple transformations --------------------------------------------------
#' 
#' # Demonstrates chaining transformations while maintaining train-test consistency
#' 
#' library(data.table)
#' data(simple_dt_date)
#' 
#' # modify data to ensure positive values for log transform
#' simple_dt_date[, X1 := X1 + abs(min(X1)) + 1]
#' simple_dt_date[, X2 := X2 + abs(min(X2)) + 1]
#' 
#' # Define stateless log transformation
#' gen_log_transform <- function(cols, offset = 1, ...) {
#'     function(x) {
#'         x_copy <- copy(x)
#'         for (col in cols) {
#'             x_copy[[col]] <- log(x_copy[[col]] + offset)
#'         }
#'         return(x_copy)
#'     }
#' }
#' 
#' # Define trainable standardization (same as previous examples)
#' gen_standardize <- function(cols, x, ...) {
#'     train_means <- colMeans(x[, ..cols], na.rm = TRUE)
#'     train_sds <- apply(x[, ..cols], 2, sd, na.rm = TRUE)
#' 
#'     function(x) {
#'         x_copy <- copy(x)
#'         for (col in cols) {
#'             x_copy[[col]] <- (x_copy[[col]] - train_means[col]) / train_sds[col]
#'         }
#'         return(x_copy)
#'     }
#' }
#' 
#' # Split data
#' train_data <- simple_dt_date[1:15]
#' test_data <- simple_dt_date[16:20]
#' data_list_train <- list(data = train_data)
#' 
#' # Define stacked transformations: first log, then standardize
#' raw_pipe <- list(
#'     on = "data",
#'     transforms = list(
#'         list(fun = "gen_log_transform", cols = c("X1", "X2"), offset = 1),
#'         list(fun = "gen_standardize", cols = c("X1", "X2"))
#'     )
#' )
#' 
#' # Parse on training data (learns standardization params from log-transformed train data)
#' parsed <- parse_single_pipe(raw_pipe, env = data_list_train)
#' 
#' # Apply to training data
#' train_result <- eval_single_pipe(parsed, env = data_list_train)
#' cat("Train after stacked transforms - X1 mean:", round(mean(train_result$X1), 10), "\n")
#' 
#' # Apply to test data with same parameters
#' data_list_test <- list(data = test_data)
#' test_result <- eval_single_pipe(parsed, env = data_list_test)
#' cat("Test after stacked transforms - X1 mean:", round(mean(test_result$X1), 6), "\n")
#' cat("(Both transformations maintain consistency: same log offset, same train mean/sd)\n")
#' 
#' # Example 4: Inspecting closure environments for debugging -------------------------------------
#' # Shows how to extract and verify captured parameters from closures
#' 
#' library(data.table)
#' data(simple_dt_date)
#' 
#' # Define trainable transformation
#' gen_standardize <- function(cols, x, ...) {
#'     train_means <- colMeans(x[, ..cols], na.rm = TRUE)
#'     train_sds <- apply(x[, ..cols], 2, sd, na.rm = TRUE)
#' 
#'     function(x) {
#'         x_copy <- copy(x)
#'         for (col in cols) {
#'             x_copy[[col]] <- (x_copy[[col]] - train_means[col]) / train_sds[col]
#'         }
#'         return(x_copy)
#'     }
#' }
#' 
#' # Setup and parse
#' data_list <- list(data = simple_dt_date)
#' raw_pipe <- list(
#'     on = "data",
#'     transforms = list(
#'         list(fun = "gen_standardize", cols = c("X1", "Y"))
#'     )
#' )
#' parsed <- parse_single_pipe(raw_pipe, env = data_list)
#' 
#' # Extract forward closure from parsed pipe
#' closure <- parsed$transforms[[1]]$forward
#' 
#' # Access closure's environment to see captured parameters
#' closure_env <- environment(closure)
#' 
#' cat("Variables captured in closure environment:\n")
#' print(ls(closure_env))
#' 
#' # Extract specific parameter values
#' captured_means <- get("train_means", closure_env)
#' captured_sds <- get("train_sds", closure_env)
#' captured_cols <- get("cols", closure_env)
#' 
#' cat("\nCaptured parameters:\n")
#' cat("  Columns:", captured_cols, "\n")
#' cat("  Mean of X1:", captured_means["X1"], "\n")
#' cat("  SD of X1:", captured_sds["X1"], "\n")
#' 
#' # Verify against manual computation
#' manual_mean <- mean(simple_dt_date$X1, na.rm = TRUE)
#' manual_sd <- sd(simple_dt_date$X1, na.rm = TRUE)
#' 
#' cat("\nVerification against manual computation:\n")
#' cat("  Manual mean:", manual_mean, "\n")
#' cat("  Captured mean:", captured_means["X1"], "\n")
#' cat("  Match:", all.equal(manual_mean, captured_means["X1"]), "\n")
#' 
#' cat("\nUse cases for environment inspection:\n")
#' cat("  - Debugging: verify parameters were computed correctly\n")
#' cat("  - Serialization: extract params for storage/transmission\n")
#' cat("  - Auditing: understand what the model learned\n")
#' cat("  - Testing: validate closure behavior\n")
#' 
#' # Example 5: Using custom enclosing environment ------------------------------------------------
#' # Demonstrates separation between data environment (env) and function environment (enclos)
#' 
#' library(data.table)
#' data(simple_dt_date)
#' 
#' # Define generator in isolated environment (simulates package namespace or module)
#' gen_standardize <- function(cols, x, ...) {
#'     train_means <- colMeans(x[, ..cols], na.rm = TRUE)
#'     train_sds <- apply(x[, ..cols], 2, sd, na.rm = TRUE)
#' 
#'     function(x) {
#'         x_copy <- copy(x)
#'         for (col in cols) {
#'             x_copy[[col]] <- (x_copy[[col]] - train_means[col]) / train_sds[col]
#'         }
#'         return(x_copy)
#'     }
#' }
#' 
#' # Create custom environment for functions
#' custom_env <- new.env()
#' assign("gen_standardize", gen_standardize, envir = custom_env)
#' 
#' # Create named list for data
#' data_list <- list(data = simple_dt_date)
#' 
#' # Define raw pipe
#' raw_pipe <- list(
#'     on = "data",
#'     transforms = list(
#'         list(fun = "gen_standardize", cols = c("X1", "X2"))
#'     )
#' )
#' 
#' # Parse with BOTH env (for data) and enclos (for functions)
#' # env = data_list: where to find "data"
#' # enclos = custom_env: where to find "gen_standardize"
#' parsed <- parse_single_pipe(raw_pipe, env = data_list, enclos = custom_env)
#' 
#' # Evaluate with same environments
#' result <- eval_single_pipe(parsed, env = data_list, enclos = custom_env)
#' 
#' cat("Result using custom environments:\n")
#' cat("  X1 mean:", round(mean(result$X1), 10), "\n")
#' cat("  X2 mean:", round(mean(result$X2), 10), "\n")
#' 
#' cat("\nSeparation of concerns:\n")
#' cat("  - env: controls DATA scope (where to find 'data')\n")
#' cat("  - enclos: controls FUNCTION scope (where to find 'gen_standardize')\n")
#' cat("  - This mirrors R's evaluation model (see ?eval)\n")
#' cat("  - Useful for: package development, isolated testing, deployment\n")
#'
#' @export

eval_single_pipe <- function(pipe, env = parent.frame(), enclos = parent.frame()) {

    args <- lapply(pipe$on, str2lang)
    names(args) <- c("x", "y")[seq_along(args)]

    l_t <- pipe$transforms

    first_fn <- if (inherits(l_t[[1]], "shapeshiftr_closure")) l_t[[1]]$forward else l_t[[1]]
    cc <- c(list(first_fn), args)
    x <- eval(as.call(cc), env, enclos)
    l_t[[1]] <- NULL

    if (length(l_t) >= 1) {
        for (f in l_t) {
            fn <- if (inherits(f, "shapeshiftr_closure")) f$forward else f
            cc <- list(fn, x)
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
#' @return list of results from applying each pipe to its defined data
#' 
#' @seealso
#' * \code{\link{eval_single_pipe}} for details on parse-eval separation
#' * \code{\link{parse_pipes}} for creating multiple parsed pipes
#' 
#' @export

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