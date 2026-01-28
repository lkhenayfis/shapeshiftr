gen_closure_filter <- function(by, value, ...) function(x) x[x[[by]] == value, ]
gen_closure_summary <- function(...) function(x) summary(x)
gen_closure_colMeans <- function(...) function(x) colMeans(x)

env <- environment(gen_closure_filter)

raw_pipes <- list(
    list(
        on = "mtcars",
        transforms = list(
            list(
                fun = "gen_closure_filter",
                by = "cyl", value = 6
            ),
            list(
                fun = "gen_closure_summary"
            )
        )
    ),
    list(
        on = "mtcars",
        transforms = list(
            list(
                fun = "gen_closure_filter",
                by = "cyl", value = 4
            ),
            list(
                fun = "gen_closure_colMeans"
            )
        )
    )
)

data_list <- list(mtcars = mtcars)

test_that("parse_single_pipe", {

    f <- parse_single_pipe
    expect_true(is.function(f))

    test_that("caso de uso padrao", {
        parsed_pipe <- f(raw_pipes[[1]])

        expect_true(inherits(parsed_pipe, "list"))
        expect_equal(length(parsed_pipe), 2)
        expect_equal(names(parsed_pipe), c("on", "transforms"))

        expect_equal(parsed_pipe$on, "mtcars")

        l_t <- parsed_pipe$transforms

        expect_equal(length(l_t), 2)
        expect_true(all(sapply(l_t, inherits, what = "shapeshiftr_closure")))

        expect_equal(formalArgs(l_t[[1]]$forward), "x")
        expect_equal(formalArgs(l_t[[2]]$forward), "x")

        expect_equal(deparse(body(l_t[[1]]$forward)), "x[x[[by]] == value, ]")
        expect_equal(deparse(body(l_t[[2]]$forward)), "summary(x)")
    })

    test_that("env e uma lista/data.frame", {

        parsed_pipe <- f(raw_pipes[[1]], env = data_list, enclos = env)

        expect_true(inherits(parsed_pipe, "list"))
        expect_equal(length(parsed_pipe), 2)
        expect_equal(names(parsed_pipe), c("on", "transforms"))

        expect_equal(parsed_pipe$on, "mtcars")

        l_t <- parsed_pipe$transforms

        expect_equal(length(l_t), 2)
        expect_true(all(sapply(l_t, inherits, what = "shapeshiftr_closure")))

        expect_equal(formalArgs(l_t[[1]]$forward), "x")
        expect_equal(formalArgs(l_t[[2]]$forward), "x")

        expect_equal(deparse(body(l_t[[1]]$forward)), "x[x[[by]] == value, ]")
        expect_equal(deparse(body(l_t[[2]]$forward)), "summary(x)")
    })
})

test_that("parse_pipes", {

    f <- parse_pipes
    expect_true(is.function(f))

    test_that("caso de uso padrao", {
        parsed_pipes <- f(raw_pipes)

        expect_true(inherits(parsed_pipes, "list"))
        expect_equal(length(parsed_pipes), 2)

        for (i in seq_along(parsed_pipes)) {
            parsed_pipe <- parsed_pipes[[i]]

            expect_true(inherits(parsed_pipe, "list"))
            expect_equal(length(parsed_pipe), 2)
            expect_equal(names(parsed_pipe), c("on", "transforms"))

            expect_equal(parsed_pipe$on, "mtcars")

            l_t <- parsed_pipe$transforms

            expect_equal(length(l_t), 2)
            expect_true(all(sapply(l_t, inherits, what = "shapeshiftr_closure")))
        }
    })

    test_that("env e uma lista/data.frame", {

        parsed_pipes <- f(raw_pipes, env = data_list, enclos = env)

        expect_true(inherits(parsed_pipes, "list"))
        expect_equal(length(parsed_pipes), 2)

        for (i in seq_along(parsed_pipes)) {
            parsed_pipe <- parsed_pipes[[i]]

            expect_true(inherits(parsed_pipe, "list"))
            expect_equal(length(parsed_pipe), 2)
            expect_equal(names(parsed_pipe), c("on", "transforms"))
            expect_equal(parsed_pipe$on, "mtcars")
        }
    })
})

test_that("eval_single_pipe", {

    f <- eval_single_pipe
    expect_true(is.function(f))

    test_that("caso de uso padrao", {
        parsed_pipe <- parse_single_pipe(raw_pipes[[1]])

        eval_pipe <- f(parsed_pipe)

        expect_true(inherits(eval_pipe, "table"))
        expect_equal(eval_pipe[1, 1], "Min.   :17.80  ")
        expect_equal(eval_pipe[1, 2], "Min.   :6  ")
    })

    test_that("env e uma lista/data.frame", {
        data_list <- list(mtcars = mtcars)

        parsed_pipe <- parse_single_pipe(raw_pipes[[1]], env = data_list, enclos = env)

        eval_pipe <- f(parsed_pipe, env = data_list, enclos = env)

        expect_true(inherits(eval_pipe, "table"))
        expect_equal(eval_pipe[1, 1], "Min.   :17.80  ")
        expect_equal(eval_pipe[1, 2], "Min.   :6  ")
    })
})

test_that("eval_pipes", {

    f <- eval_pipes
    expect_true(is.function(f))

    test_that("caso de uso padrao", {
        parsed_pipes <- parse_pipes(raw_pipes)

        eval_pipe <- f(parsed_pipes)

        expect_true(inherits(eval_pipe, "list"))
        expect_equal(length(eval_pipe), 2)
        expect_equal(eval_pipe[[1]][1, 2], "Min.   :6  ")
        expect_equal(round(eval_pipe[[2]][1]), c(mpg = 27))
    })

    test_that("env e uma lista/list", {
        data_list <- list(mtcars = mtcars)

        parsed_pipes <- parse_pipes(raw_pipes, env = data_list, enclos = env)
        eval_pipe <- f(parsed_pipes, env = data_list, enclos = env)

        expect_true(inherits(eval_pipe, "list"))
        expect_equal(length(eval_pipe), 2)
        expect_equal(eval_pipe[[1]][1, 2], "Min.   :6  ")
        expect_equal(round(eval_pipe[[2]][1]), c(mpg = 27))
    })
})

test_that("combine_pipes", {
    f <- combine_pipes
    expect_true(is.function(f))

    test_that("caso de uso padrao", {
        parsed_pipes <- parse_pipes(raw_pipes)
        eval_pipe    <- eval_pipes(parsed_pipes)

        cfun <- function(x, y, ...) rbind(x, y)
        combined <- combine_pipes(eval_pipe, cfun)

        expect_true(inherits(combined, "matrix"))
        expect_equal(dim(combined), c(7, 11))
    })
})

test_that("new_shapeshiftr_closure", {
    f <- new_shapeshiftr_closure
    expect_true(is.function(f))

    test_that("wraps single closure with identity backward", {
        forward_fn <- function(x) x * 2
        result <- f(forward_fn)

        expect_s3_class(result, "shapeshiftr_closure")
        expect_true(is.list(result))
        expect_equal(length(result), 2)
        expect_equal(names(result), c("forward", "backward"))

        expect_true(is.function(result$forward))
        expect_true(is.function(result$backward))

        expect_equal(result$forward(5), 10)
        expect_equal(result$backward(5), 5)
    })

    test_that("uses paired closures as-is", {
        forward_fn <- function(x) log(x)
        backward_fn <- function(x) exp(x)
        funs <- list(forward_fn, backward_fn)

        result <- f(funs)

        expect_s3_class(result, "shapeshiftr_closure")
        expect_equal(length(result), 2)
        expect_equal(names(result), c("forward", "backward"))

        expect_equal(result$forward(2.718282), log(2.718282), tolerance = 1e-5)
        expect_equal(result$backward(1), exp(1), tolerance = 1e-5)
    })

    test_that("sets correct class attribute", {
        forward_fn <- function(x) x + 1
        result <- f(forward_fn)

        expect_true(inherits(result, "shapeshiftr_closure"))
        expect_equal(class(result), "shapeshiftr_closure")
    })

    test_that("ensures correct names", {
        forward_fn <- function(x) x - 3
        result <- f(forward_fn)

        expect_equal(names(result), c("forward", "backward"))
        expect_true("forward" %in% names(result))
        expect_true("backward" %in% names(result))
    })

    test_that("shares environment between forward and backward", {
        forward_fn <- function(x) x / 2
        result <- f(forward_fn)

        env_forward <- environment(result$forward)
        env_backward <- environment(result$backward)

        expect_true(identical(env_forward, env_backward))
    })

    test_that("validation is called and catches invalid inputs", {
        expect_error(f(list(1, 2)), "forward element must be a function")
        expect_error(f(list(function(x) x, "not a function")),
                     "backward element must be a function")
    })
})

test_that("validate_shapeshiftr_closure", {
    f <- validate_shapeshiftr_closure
    expect_true(is.function(f))

    test_that("returns invisible for valid input", {
        forward_fn <- function(x) x * 2
        backward_fn <- function(x) x / 2
        environment(backward_fn) <- environment(forward_fn)
        sc <- list(forward = forward_fn, backward = backward_fn)
        class(sc) <- "shapeshiftr_closure"

        result <- withVisible(f(sc))

        expect_false(result$visible)
        expect_identical(result$value, sc)
    })

    test_that("errors when input is not a list", {
        expect_error(f("not a list"), "shapeshiftr_closure must be a list")
        expect_error(f(42), "shapeshiftr_closure must be a list")
        expect_error(f(NULL), "shapeshiftr_closure must be a list")
    })

    test_that("errors when length is wrong", {
        sc <- list(forward = function(x) x)
        class(sc) <- "shapeshiftr_closure"

        expect_error(f(sc), "shapeshiftr_closure must have length 2, got length 1")

        sc <- list(forward = function(x) x, backward = function(x) x, extra = function(x) x)
        class(sc) <- "shapeshiftr_closure"

        expect_error(f(sc), "shapeshiftr_closure must have length 2, got length 3")
    })

    test_that("errors when names are missing or wrong", {
        forward_fn <- function(x) x
        backward_fn <- function(x) x
        environment(backward_fn) <- environment(forward_fn)

        sc <- list(fwd = forward_fn, bwd = backward_fn)
        class(sc) <- "shapeshiftr_closure"

        expect_error(f(sc), "shapeshiftr_closure must have names c\\('forward', 'backward'\\)")

        sc <- list(forward_fn, backward_fn)
        class(sc) <- "shapeshiftr_closure"

        expect_error(f(sc), "shapeshiftr_closure must have names c\\('forward', 'backward'\\)|forward element must be a function")
    })

    test_that("errors when elements are not functions", {
        sc <- list(forward = 42, backward = function(x) x)
        class(sc) <- "shapeshiftr_closure"

        expect_error(f(sc), "forward element must be a function")

        sc <- list(forward = function(x) x, backward = "not a function")
        class(sc) <- "shapeshiftr_closure"

        expect_error(f(sc), "backward element must be a function")
    })

    test_that("errors when functions have wrong argument count", {
        sc <- list(forward = function(x, y) x + y, backward = function(x) x)
        class(sc) <- "shapeshiftr_closure"

        expect_error(f(sc), "forward function must have exactly 1 argument, got 2")

        sc <- list(forward = function(x) x, backward = function() 42)
        class(sc) <- "shapeshiftr_closure"

        expect_error(f(sc), "backward function must have exactly 1 argument, got 0")
    })

    test_that("errors when argument is not named x", {
        sc <- list(forward = function(y) y * 2, backward = function(x) x)
        class(sc) <- "shapeshiftr_closure"

        expect_error(f(sc), "forward function argument must be named 'x', got: y")

        forward_fn <- function(x) x
        sc <- list(forward = forward_fn, backward = function(z) z)
        class(sc) <- "shapeshiftr_closure"

        expect_error(f(sc), "backward function argument must be named 'x', got: z")
    })

    test_that("errors when environments are different", {
        env1 <- new.env()
        env2 <- new.env()

        forward_fn <- function(x) x * 2
        backward_fn <- function(x) x / 2
        environment(forward_fn) <- env1
        environment(backward_fn) <- env2

        sc <- list(forward = forward_fn, backward = backward_fn)
        class(sc) <- "shapeshiftr_closure"

        expect_error(f(sc), "forward and backward closures must share the same environment")
    })
})

test_that("parse_single_pipe wraps generators as shapeshiftr_closure", {
    gen_simple <- function(...) function(x) x * 2

    test_that("single closure generators wrapped correctly", {
        raw_pipe <- list(
            on = "mtcars",
            transforms = list(list(fun = "gen_simple"))
        )

        parsed <- parse_single_pipe(raw_pipe)

        expect_equal(length(parsed$transforms), 1)
        expect_s3_class(parsed$transforms[[1]], "shapeshiftr_closure")
        expect_true(is.function(parsed$transforms[[1]]$forward))
        expect_true(is.function(parsed$transforms[[1]]$backward))

        expect_equal(parsed$transforms[[1]]$forward(5), 10)
        expect_equal(parsed$transforms[[1]]$backward(5), 5)
    })

    test_that("paired closure generators wrapped correctly", {
        gen_paired <- function(...) {
            list(
                function(x) log(x),
                function(x) exp(x)
            )
        }

        raw_pipe <- list(
            on = "mtcars",
            transforms = list(list(fun = "gen_paired"))
        )

        parsed <- parse_single_pipe(raw_pipe)

        expect_equal(length(parsed$transforms), 1)
        expect_s3_class(parsed$transforms[[1]], "shapeshiftr_closure")

        expect_equal(parsed$transforms[[1]]$forward(2.718282),
                     log(2.718282), tolerance = 1e-5)
        expect_equal(parsed$transforms[[1]]$backward(1),
                     exp(1), tolerance = 1e-5)
    })

    test_that("multiple transforms all wrapped", {
        gen_add <- function(val, ...) function(x) x + val
        gen_mult <- function(val, ...) function(x) x * val

        raw_pipe <- list(
            on = "mtcars",
            transforms = list(
                list(fun = "gen_add", val = 10),
                list(fun = "gen_mult", val = 2)
            )
        )

        parsed <- parse_single_pipe(raw_pipe)

        expect_equal(length(parsed$transforms), 2)
        expect_true(all(sapply(parsed$transforms, inherits, "shapeshiftr_closure")))

        expect_true(is.function(parsed$transforms[[1]]$forward))
        expect_true(is.function(parsed$transforms[[1]]$backward))
        expect_true(is.function(parsed$transforms[[2]]$forward))
        expect_true(is.function(parsed$transforms[[2]]$backward))
    })

    test_that("chaining uses forward closures", {
        gen_add <- function(val, x, ...) {
            function(x) x + val
        }

        gen_capture <- function(x, ...) {
            captured_val <- x
            function(x) captured_val
        }

        raw_pipe <- list(
            on = "mtcars",
            transforms = list(
                list(fun = "gen_add", val = 100),
                list(fun = "gen_capture")
            )
        )

        data_env <- list(mtcars = 5)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        intermediate_value <- parsed$transforms[[2]]$forward(0)
        expect_equal(intermediate_value, 105)
    })

    test_that("forward closures share environment with backward", {
        raw_pipe <- list(
            on = "mtcars",
            transforms = list(list(fun = "gen_simple"))
        )

        parsed <- parse_single_pipe(raw_pipe)

        env_forward <- environment(parsed$transforms[[1]]$forward)
        env_backward <- environment(parsed$transforms[[1]]$backward)

        expect_true(identical(env_forward, env_backward))
    })
})

test_that("forward_single_pipe", {
    f <- forward_single_pipe
    expect_true(is.function(f))

    test_that("extracts and applies forward closures", {
        gen_mult <- function(val, ...) function(x) x * val

        raw_pipe <- list(
            on = "value",
            transforms = list(list(fun = "gen_mult", val = 3))
        )

        data_env <- list(value = 10)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)
        result <- f(parsed, env = data_env)

        expect_equal(result, 30)
    })

    test_that("forward closure is callable with single argument", {
        gen_square <- function(...) function(x) x^2

        raw_pipe <- list(
            on = "value",
            transforms = list(list(fun = "gen_square"))
        )

        data_env <- list(value = 5)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        forward_fn <- parsed$transforms[[1]]$forward

        expect_true(is.function(forward_fn))
        expect_equal(length(formals(forward_fn)), 1)
        expect_equal(names(formals(forward_fn)), "x")
        expect_equal(forward_fn(4), 16)
    })

    test_that("forward closures have correct environment", {
        gen_with_params <- function(offset, x, ...) {
            function(x) x + offset
        }

        raw_pipe <- list(
            on = "value",
            transforms = list(list(fun = "gen_with_params", offset = 100))
        )

        data_env <- list(value = 5)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        forward_fn <- parsed$transforms[[1]]$forward
        forward_env <- environment(forward_fn)

        expect_true(exists("offset", envir = forward_env))
        expect_equal(get("offset", envir = forward_env), 100)
    })
})

test_that("backward_single_pipe", {
    f <- backward_single_pipe
    expect_true(is.function(f))

    test_that("extracts and applies backward closures in reverse order", {
        gen_log_exp <- function(...) {
            list(
                function(x) log(x),
                function(x) exp(x)
            )
        }

        raw_pipe <- list(
            on = "value",
            transforms = list(list(fun = "gen_log_exp"))
        )

        data_env <- list(value = 2.718282)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        forward_result <- forward_single_pipe(parsed, env = data_env)
        backward_result <- f(parsed, env = list(value = forward_result))

        expect_equal(backward_result, 2.718282, tolerance = 1e-5)
    })

    test_that("backward closure is callable with single argument", {
        gen_paired <- function(...) {
            list(
                function(x) x * 2,
                function(x) x / 2
            )
        }

        raw_pipe <- list(
            on = "value",
            transforms = list(list(fun = "gen_paired"))
        )

        data_env <- list(value = 10)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        backward_fn <- parsed$transforms[[1]]$backward

        expect_true(is.function(backward_fn))
        expect_equal(length(formals(backward_fn)), 1)
        expect_equal(names(formals(backward_fn)), "x")
        expect_equal(backward_fn(20), 10)
    })

    test_that("backward closures share environment with forward", {
        gen_paired <- function(offset, ...) {
            list(
                function(x) x + offset,
                function(x) x - offset
            )
        }

        raw_pipe <- list(
            on = "value",
            transforms = list(list(fun = "gen_paired", offset = 50))
        )

        data_env <- list(value = 10)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        forward_env <- environment(parsed$transforms[[1]]$forward)
        backward_env <- environment(parsed$transforms[[1]]$backward)

        expect_true(identical(forward_env, backward_env))
        expect_true(exists("offset", envir = backward_env))
        expect_equal(get("offset", envir = backward_env), 50)
    })

    test_that("reverses order correctly for multiple transforms", {
        gen_add <- function(val, ...) {
            list(
                function(x) x + val,
                function(x) x - val
            )
        }

        gen_mult <- function(val, ...) {
            list(
                function(x) x * val,
                function(x) x / val
            )
        }

        raw_pipe <- list(
            on = "value",
            transforms = list(
                list(fun = "gen_add", val = 10),
                list(fun = "gen_mult", val = 2)
            )
        )

        data_env <- list(value = 5)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        forward_result <- forward_single_pipe(parsed, env = data_env)
        backward_result <- f(parsed, env = list(value = forward_result))

        expect_equal(backward_result, 5, tolerance = 1e-10)
    })
})

test_that("forward_single_pipe integration", {
    f <- forward_single_pipe
    expect_true(is.function(f))

    test_that("single transformation applied correctly", {
        gen_scale <- function(factor, ...) {
            list(
                function(x) x * factor,
                function(x) x / factor
            )
        }

        raw_pipe <- list(
            on = "value",
            transforms = list(list(fun = "gen_scale", factor = 2.5))
        )

        data_env <- list(value = c(1, 2, 3, 4, 5))
        parsed <- parse_single_pipe(raw_pipe, env = data_env)
        result <- f(parsed, env = data_env)

        expected <- c(2.5, 5.0, 7.5, 10.0, 12.5)
        expect_equal(result, expected, tolerance = 1e-10)
    })

    test_that("multi-transform chaining applied correctly", {
        gen_add <- function(val, ...) {
            list(
                function(x) x + val,
                function(x) x - val
            )
        }

        gen_mult <- function(val, ...) {
            list(
                function(x) x * val,
                function(x) x / val
            )
        }

        raw_pipe <- list(
            on = "value",
            transforms = list(
                list(fun = "gen_add", val = 10),
                list(fun = "gen_mult", val = 2)
            )
        )

        data_env <- list(value = c(5, 10, 15))
        parsed <- parse_single_pipe(raw_pipe, env = data_env)
        result <- f(parsed, env = data_env)

        expected <- (c(5, 10, 15) + 10) * 2
        expect_equal(result, expected, tolerance = 1e-10)
    })

    test_that("matches eval_single_pipe result", {
        gen_log <- function(...) {
            list(
                function(x) log(x),
                function(x) exp(x)
            )
        }

        raw_pipe <- list(
            on = "value",
            transforms = list(list(fun = "gen_log"))
        )

        data_env <- list(value = c(1, 2.718282, 7.389056))
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        suppressWarnings({
            result_forward <- f(parsed, env = data_env)
            result_eval <- eval_single_pipe(parsed, env = data_env)
        })

        expect_equal(result_forward, result_eval, tolerance = 1e-5)
    })

    test_that("environment handling works with custom env", {
        gen_normalize <- function(x, ...) {
            min_val <- min(x)
            max_val <- max(x)
            range_val <- max_val - min_val

            list(
                function(x) (x - min_val) / range_val,
                function(x) x * range_val + min_val
            )
        }

        data_list <- list(train = c(0, 10, 20, 30, 40))
        raw_pipe <- list(
            on = "train",
            transforms = list(list(fun = "gen_normalize"))
        )

        parsed <- parse_single_pipe(raw_pipe, env = data_list)
        result <- f(parsed, env = data_list)

        expected <- c(0, 0.25, 0.5, 0.75, 1.0)
        expect_equal(result, expected, tolerance = 1e-10)
    })

    test_that("handles data.frame transformations", {
        gen_select_cols <- function(cols, ...) {
            function(x) x[, cols, drop = FALSE]
        }

        df <- data.frame(a = 1:5, b = 6:10, c = 11:15)
        data_list <- list(df = df)

        raw_pipe <- list(
            on = "df",
            transforms = list(list(fun = "gen_select_cols", cols = c("a", "c")))
        )

        parsed <- parse_single_pipe(raw_pipe, env = data_list)
        result <- f(parsed, env = data_list)

        expect_equal(ncol(result), 2)
        expect_equal(colnames(result), c("a", "c"))
        expect_equal(result$a, 1:5)
        expect_equal(result$c, 11:15)
    })
})

test_that("backward_single_pipe integration", {
    f <- backward_single_pipe
    expect_true(is.function(f))

    test_that("single transformation inverted correctly", {
        gen_scale <- function(factor, ...) {
            list(
                function(x) x * factor,
                function(x) x / factor
            )
        }

        raw_pipe <- list(
            on = "value",
            transforms = list(list(fun = "gen_scale", factor = 3))
        )

        original <- c(2, 4, 6, 8, 10)
        data_env <- list(value = original)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        forward_result <- forward_single_pipe(parsed, env = data_env)
        backward_result <- f(parsed, env = list(value = forward_result))

        expect_equal(backward_result, original, tolerance = 1e-10)
    })

    test_that("reverses order correctly for multiple transforms", {
        gen_add <- function(val, ...) {
            list(
                function(x) x + val,
                function(x) x - val
            )
        }

        gen_mult <- function(val, ...) {
            list(
                function(x) x * val,
                function(x) x / val
            )
        }

        raw_pipe <- list(
            on = "value",
            transforms = list(
                list(fun = "gen_add", val = 5),
                list(fun = "gen_mult", val = 3)
            )
        )

        original <- c(1, 2, 3, 4, 5)
        data_env <- list(value = original)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        forward_result <- forward_single_pipe(parsed, env = data_env)
        backward_result <- f(parsed, env = list(value = forward_result))

        expect_equal(backward_result, original, tolerance = 1e-10)
    })

    test_that("three-transform chain reverses correctly", {
        gen_add <- function(val, ...) {
            list(
                function(x) x + val,
                function(x) x - val
            )
        }

        gen_mult <- function(val, ...) {
            list(
                function(x) x * val,
                function(x) x / val
            )
        }

        gen_square <- function(...) {
            list(
                function(x) x^2,
                function(x) sqrt(abs(x))
            )
        }

        raw_pipe <- list(
            on = "value",
            transforms = list(
                list(fun = "gen_add", val = 2),
                list(fun = "gen_mult", val = 3),
                list(fun = "gen_square")
            )
        )

        original <- c(1, 2, 3, 4, 5)
        data_env <- list(value = original)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        forward_result <- forward_single_pipe(parsed, env = data_env)
        backward_result <- f(parsed, env = list(value = forward_result))

        expect_equal(backward_result, original, tolerance = 1e-8)
    })

    test_that("log-exp transformation reverses correctly", {
        gen_log_exp <- function(...) {
            list(
                function(x) log(x),
                function(x) exp(x)
            )
        }

        raw_pipe <- list(
            on = "value",
            transforms = list(list(fun = "gen_log_exp"))
        )

        original <- c(1, 2.718282, 7.389056, 20.085537)
        data_env <- list(value = original)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        forward_result <- forward_single_pipe(parsed, env = data_env)
        backward_result <- f(parsed, env = list(value = forward_result))

        expect_equal(backward_result, original, tolerance = 1e-5)
    })

    test_that("environment handling works correctly", {
        gen_center_uncenter <- function(x, ...) {
            center_val <- mean(x)

            list(
                function(x) x - center_val,
                function(x) x + center_val
            )
        }

        original <- c(5, 10, 15, 20, 25)
        data_list <- list(train = original)

        raw_pipe <- list(
            on = "train",
            transforms = list(list(fun = "gen_center_uncenter"))
        )

        parsed <- parse_single_pipe(raw_pipe, env = data_list)
        forward_result <- forward_single_pipe(parsed, env = data_list)
        backward_result <- f(parsed, env = list(train = forward_result))

        expect_equal(backward_result, original, tolerance = 1e-10)
    })
})

test_that("multi-transform pipe integration", {

    test_that("forward chain applies in correct order", {
        gen_add <- function(val, ...) {
            list(
                function(x) x + val,
                function(x) x - val
            )
        }

        gen_mult <- function(val, ...) {
            list(
                function(x) x * val,
                function(x) x / val
            )
        }

        gen_power <- function(exp, ...) {
            list(
                function(x) x^exp,
                function(x) x^(1 / exp)
            )
        }

        raw_pipe <- list(
            on = "value",
            transforms = list(
                list(fun = "gen_add", val = 3),
                list(fun = "gen_mult", val = 2),
                list(fun = "gen_power", exp = 2)
            )
        )

        input <- c(1, 2, 3)
        data_env <- list(value = input)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)
        result <- forward_single_pipe(parsed, env = data_env)

        expected <- ((input + 3) * 2)^2
        expect_equal(result, expected, tolerance = 1e-10)
    })

    test_that("backward chain reverses correctly", {
        gen_log <- function(...) {
            list(
                function(x) log(x),
                function(x) exp(x)
            )
        }

        gen_scale <- function(factor, ...) {
            list(
                function(x) x * factor,
                function(x) x / factor
            )
        }

        gen_shift <- function(offset, ...) {
            list(
                function(x) x + offset,
                function(x) x - offset
            )
        }

        raw_pipe <- list(
            on = "value",
            transforms = list(
                list(fun = "gen_log"),
                list(fun = "gen_scale", factor = 5),
                list(fun = "gen_shift", offset = 10)
            )
        )

        original <- c(1, 2.718282, 7.389056)
        data_env <- list(value = original)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        forward_result <- forward_single_pipe(parsed, env = data_env)
        backward_result <- backward_single_pipe(
            parsed,
            env = list(value = forward_result)
        )

        expect_equal(backward_result, original, tolerance = 1e-5)
    })

    test_that("complex chaining with four transforms", {
        gen_add <- function(val, ...) {
            list(
                function(x) x + val,
                function(x) x - val
            )
        }

        gen_mult <- function(val, ...) {
            list(
                function(x) x * val,
                function(x) x / val
            )
        }

        raw_pipe <- list(
            on = "value",
            transforms = list(
                list(fun = "gen_add", val = 1),
                list(fun = "gen_mult", val = 2),
                list(fun = "gen_add", val = 3),
                list(fun = "gen_mult", val = 4)
            )
        )

        original <- c(5, 10, 15)
        data_env <- list(value = original)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        forward_result <- forward_single_pipe(parsed, env = data_env)
        backward_result <- backward_single_pipe(
            parsed,
            env = list(value = forward_result)
        )

        expect_equal(backward_result, original, tolerance = 1e-10)
    })

    test_that("mixed reversible and identity transforms", {
        gen_reversible <- function(val, ...) {
            list(
                function(x) x * val,
                function(x) x / val
            )
        }

        gen_identity <- function(...) {
            function(x) x
        }

        raw_pipe <- list(
            on = "value",
            transforms = list(
                list(fun = "gen_reversible", val = 2),
                list(fun = "gen_identity"),
                list(fun = "gen_reversible", val = 3)
            )
        )

        original <- c(2, 4, 6, 8)
        data_env <- list(value = original)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        forward_result <- forward_single_pipe(parsed, env = data_env)
        expected_forward <- original * 2 * 3
        expect_equal(forward_result, expected_forward, tolerance = 1e-10)

        backward_result <- backward_single_pipe(
            parsed,
            env = list(value = forward_result)
        )
        expect_equal(backward_result, original, tolerance = 1e-10)
    })

    test_that("parse handles multiple transforms correctly", {
        gen_t1 <- function(a, ...) function(x) x + a
        gen_t2 <- function(b, ...) function(x) x * b
        gen_t3 <- function(c, ...) function(x) x - c

        raw_pipe <- list(
            on = "value",
            transforms = list(
                list(fun = "gen_t1", a = 5),
                list(fun = "gen_t2", b = 2),
                list(fun = "gen_t3", c = 3)
            )
        )

        data_env <- list(value = 10)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        expect_equal(length(parsed$transforms), 3)
        expect_true(all(sapply(parsed$transforms, inherits, "shapeshiftr_closure")))
    })
})

test_that("train-test consistency integration", {

    test_that("parse on train captures training parameters", {
        gen_standardize <- function(x, ...) {
            train_mean <- mean(x)
            train_sd <- sd(x)

            list(
                function(x) (x - train_mean) / train_sd,
                function(x) x * train_sd + train_mean
            )
        }

        train_data <- c(10, 20, 30, 40, 50)
        data_list_train <- list(data = train_data)

        raw_pipe <- list(
            on = "data",
            transforms = list(list(fun = "gen_standardize"))
        )

        parsed <- parse_single_pipe(raw_pipe, env = data_list_train)

        closure_env <- environment(parsed$transforms[[1]]$forward)
        captured_mean <- get("train_mean", envir = closure_env)
        captured_sd <- get("train_sd", envir = closure_env)

        expect_equal(captured_mean, mean(train_data), tolerance = 1e-10)
        expect_equal(captured_sd, sd(train_data), tolerance = 1e-10)
    })

    test_that("forward on train uses training parameters", {
        gen_standardize <- function(x, ...) {
            train_mean <- mean(x)
            train_sd <- sd(x)

            list(
                function(x) (x - train_mean) / train_sd,
                function(x) x * train_sd + train_mean
            )
        }

        train_data <- c(5, 10, 15, 20, 25)
        data_list_train <- list(data = train_data)

        raw_pipe <- list(
            on = "data",
            transforms = list(list(fun = "gen_standardize"))
        )

        parsed <- parse_single_pipe(raw_pipe, env = data_list_train)
        result_train <- forward_single_pipe(parsed, env = data_list_train)

        expect_equal(mean(result_train), 0, tolerance = 1e-10)
        expect_equal(sd(result_train), 1, tolerance = 1e-10)
    })

    test_that("forward on test uses same training parameters", {
        gen_standardize <- function(x, ...) {
            train_mean <- mean(x)
            train_sd <- sd(x)

            list(
                function(x) (x - train_mean) / train_sd,
                function(x) x * train_sd + train_mean
            )
        }

        train_data <- c(0, 10, 20, 30, 40)
        test_data <- c(5, 15, 25)

        data_list_train <- list(data = train_data)

        raw_pipe <- list(
            on = "data",
            transforms = list(list(fun = "gen_standardize"))
        )

        parsed <- parse_single_pipe(raw_pipe, env = data_list_train)

        data_list_test <- list(data = test_data)
        result_test <- forward_single_pipe(parsed, env = data_list_test)

        train_mean <- mean(train_data)
        train_sd <- sd(train_data)
        expected_test <- (test_data - train_mean) / train_sd

        expect_equal(result_test, expected_test, tolerance = 1e-10)
        expect_false(isTRUE(all.equal(mean(result_test), 0, tolerance = 1e-5)))
        expect_false(isTRUE(all.equal(sd(result_test), 1, tolerance = 1e-5)))
    })

    test_that("backward uses same training parameters", {
        gen_normalize <- function(x, ...) {
            min_val <- min(x)
            max_val <- max(x)
            range_val <- max_val - min_val

            list(
                function(x) (x - min_val) / range_val,
                function(x) x * range_val + min_val
            )
        }

        train_data <- c(0, 25, 50, 75, 100)
        test_data <- c(10, 30, 60, 90)

        data_list_train <- list(data = train_data)

        raw_pipe <- list(
            on = "data",
            transforms = list(list(fun = "gen_normalize"))
        )

        parsed <- parse_single_pipe(raw_pipe, env = data_list_train)

        data_list_test <- list(data = test_data)
        forward_test <- forward_single_pipe(parsed, env = data_list_test)
        backward_test <- backward_single_pipe(
            parsed,
            env = list(data = forward_test)
        )

        expect_equal(backward_test, test_data, tolerance = 1e-10)
    })

    test_that("manual verification matches hand calculation", {
        gen_center_scale <- function(x, ...) {
            center <- mean(x)
            scale <- sd(x)

            list(
                function(x) (x - center) / scale,
                function(x) x * scale + center
            )
        }

        train_data <- c(2, 4, 6, 8, 10)
        test_data <- c(3, 5, 7)

        data_list_train <- list(data = train_data)

        raw_pipe <- list(
            on = "data",
            transforms = list(list(fun = "gen_center_scale"))
        )

        parsed <- parse_single_pipe(raw_pipe, env = data_list_train)

        train_mean <- mean(train_data)
        train_sd <- sd(train_data)

        data_list_test <- list(data = test_data)
        result_test <- forward_single_pipe(parsed, env = data_list_test)

        manual_transform <- (test_data - train_mean) / train_sd

        expect_equal(result_test, manual_transform, tolerance = 1e-10)
    })

    test_that("multiple transforms maintain train-test consistency", {
        gen_log <- function(x, ...) {
            offset <- min(x) - 1
            if (offset >= 0) offset <- 0

            list(
                function(x) log(x - offset),
                function(x) exp(x) + offset
            )
        }

        gen_standardize <- function(x, ...) {
            train_mean <- mean(x)
            train_sd <- sd(x)

            list(
                function(x) (x - train_mean) / train_sd,
                function(x) x * train_sd + train_mean
            )
        }

        train_data <- c(1, 2, 3, 4, 5)
        test_data <- c(1.5, 2.5, 3.5, 4.5)

        data_list_train <- list(data = train_data)

        raw_pipe <- list(
            on = "data",
            transforms = list(
                list(fun = "gen_log"),
                list(fun = "gen_standardize")
            )
        )

        parsed <- parse_single_pipe(raw_pipe, env = data_list_train)

        result_train <- forward_single_pipe(parsed, env = data_list_train)
        expect_equal(mean(result_train), 0, tolerance = 1e-10)
        expect_equal(sd(result_train), 1, tolerance = 1e-10)

        data_list_test <- list(data = test_data)
        result_test <- forward_single_pipe(parsed, env = data_list_test)

        expect_false(isTRUE(all.equal(mean(result_test), 0, tolerance = 1e-5)))

        backward_test <- backward_single_pipe(
            parsed,
            env = list(data = result_test)
        )
        expect_equal(backward_test, test_data, tolerance = 1e-8)
    })

    test_that("train-test with data.frame transformations", {
        gen_standardize_df <- function(cols, x, ...) {
            train_means <- colMeans(x[, cols, drop = FALSE])
            train_sds <- apply(x[, cols, drop = FALSE], 2, sd)

            list(
                function(x) {
                    x_copy <- x
                    for (col in cols) {
                        x_copy[[col]] <- (x_copy[[col]] - train_means[col]) / train_sds[col]
                    }
                    x_copy
                },
                function(x) {
                    x_copy <- x
                    for (col in cols) {
                        x_copy[[col]] <- x_copy[[col]] * train_sds[col] + train_means[col]
                    }
                    x_copy
                }
            )
        }

        train_df <- data.frame(
            a = c(10, 20, 30, 40, 50),
            b = c(100, 200, 300, 400, 500)
        )

        test_df <- data.frame(
            a = c(15, 25, 35),
            b = c(150, 250, 350)
        )

        data_list_train <- list(data = train_df)

        raw_pipe <- list(
            on = "data",
            transforms = list(list(fun = "gen_standardize_df", cols = c("a", "b")))
        )

        parsed <- parse_single_pipe(raw_pipe, env = data_list_train)

        result_train <- forward_single_pipe(parsed, env = data_list_train)
        expect_equal(mean(result_train$a), 0, tolerance = 1e-10)
        expect_equal(sd(result_train$a), 1, tolerance = 1e-10)

        data_list_test <- list(data = test_df)
        result_test <- forward_single_pipe(parsed, env = data_list_test)

        train_mean_a <- mean(train_df$a)
        train_sd_a <- sd(train_df$a)
        expected_a <- (test_df$a - train_mean_a) / train_sd_a
        expect_equal(result_test$a, expected_a, tolerance = 1e-10)

        backward_test <- backward_single_pipe(
            parsed,
            env = list(data = result_test)
        )
        expect_equal(backward_test$a, test_df$a, tolerance = 1e-10)
        expect_equal(backward_test$b, test_df$b, tolerance = 1e-10)
    })
})

test_that("expect_roundtrip", {
    f <- function(pipe, data_env, tolerance = 1e-10) {
        forward_result <- forward_single_pipe(pipe, env = data_env)
        backward_env <- list(data = forward_result)
        backward_result <- backward_single_pipe(pipe, env = backward_env)
        expect_equal(backward_result, data_env$data, tolerance = tolerance)
        invisible(backward_result)
    }

    expect_true(is.function(f))

    test_that("helper validates roundtrip for simple transform", {
        gen_scale <- function(factor, ...) {
            list(
                function(x) x * factor,
                function(x) x / factor
            )
        }

        raw_pipe <- list(
            on = "data",
            transforms = list(list(fun = "gen_scale", factor = 2))
        )

        original <- c(1, 2, 3, 4, 5)
        data_env <- list(data = original)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        result <- f(parsed, data_env)
        expect_equal(result, original, tolerance = 1e-10)
    })

    test_that("helper detects failed roundtrip", {
        gen_bad <- function(...) {
            list(
                function(x) x * 2,
                function(x) x / 3
            )
        }

        raw_pipe <- list(
            on = "data",
            transforms = list(list(fun = "gen_bad"))
        )

        original <- c(1, 2, 3)
        data_env <- list(data = original)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        expect_error(f(parsed, data_env))
    })

    test_that("helper respects custom tolerance", {
        gen_approx <- function(...) {
            list(
                function(x) x + 1e-11,
                function(x) x - 1e-11
            )
        }

        raw_pipe <- list(
            on = "data",
            transforms = list(list(fun = "gen_approx"))
        )

        original <- c(1, 2, 3)
        data_env <- list(data = original)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        result <- f(parsed, data_env, tolerance = 1e-10)
        expect_equal(result, original, tolerance = 1e-10)
    })
})

test_that("scaling transformations roundtrip", {
    expect_roundtrip <- function(pipe, data_env, tolerance = 1e-10) {
        forward_result <- forward_single_pipe(pipe, env = data_env)
        backward_env <- list(data = forward_result)
        backward_result <- backward_single_pipe(pipe, env = backward_env)
        expect_equal(backward_result, data_env$data, tolerance = tolerance)
        invisible(backward_result)
    }

    test_that("single column scaling roundtrips", {
        gen_scale <- function(factor, ...) {
            list(
                function(x) x * factor,
                function(x) x / factor
            )
        }

        raw_pipe <- list(
            on = "data",
            transforms = list(list(fun = "gen_scale", factor = 3.5))
        )

        original <- c(1.5, 2.5, 3.5, 4.5, 5.5)
        data_env <- list(data = original)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        expect_roundtrip(parsed, data_env)
    })

    test_that("standardization roundtrips", {
        gen_standardize <- function(x, ...) {
            train_mean <- mean(x)
            train_sd <- sd(x)

            list(
                function(x) (x - train_mean) / train_sd,
                function(x) x * train_sd + train_mean
            )
        }

        raw_pipe <- list(
            on = "data",
            transforms = list(list(fun = "gen_standardize"))
        )

        original <- c(10, 20, 30, 40, 50, 60, 70)
        data_env <- list(data = original)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        expect_roundtrip(parsed, data_env)
    })

    test_that("normalization roundtrips", {
        gen_normalize <- function(x, ...) {
            min_val <- min(x)
            max_val <- max(x)
            range_val <- max_val - min_val

            list(
                function(x) (x - min_val) / range_val,
                function(x) x * range_val + min_val
            )
        }

        raw_pipe <- list(
            on = "data",
            transforms = list(list(fun = "gen_normalize"))
        )

        original <- c(5, 15, 25, 35, 45, 55)
        data_env <- list(data = original)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        expect_roundtrip(parsed, data_env)
    })

    test_that("train-test scaling roundtrips with test data", {
        gen_standardize <- function(x, ...) {
            train_mean <- mean(x)
            train_sd <- sd(x)

            list(
                function(x) (x - train_mean) / train_sd,
                function(x) x * train_sd + train_mean
            )
        }

        raw_pipe <- list(
            on = "data",
            transforms = list(list(fun = "gen_standardize"))
        )

        train_data <- c(0, 10, 20, 30, 40)
        test_data <- c(5, 15, 25, 35)

        train_env <- list(data = train_data)
        parsed <- parse_single_pipe(raw_pipe, env = train_env)

        test_env <- list(data = test_data)
        expect_roundtrip(parsed, test_env)
    })

    test_that("multi-column data.frame scaling roundtrips", {
        gen_scale_df <- function(cols, factor, ...) {
            list(
                function(x) {
                    x_copy <- x
                    for (col in cols) {
                        x_copy[[col]] <- x_copy[[col]] * factor
                    }
                    x_copy
                },
                function(x) {
                    x_copy <- x
                    for (col in cols) {
                        x_copy[[col]] <- x_copy[[col]] / factor
                    }
                    x_copy
                }
            )
        }

        raw_pipe <- list(
            on = "data",
            transforms = list(list(fun = "gen_scale_df", cols = c("a", "b"), factor = 2.5))
        )

        original_df <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6), c = c(7, 8, 9))
        data_env <- list(data = original_df)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        expect_roundtrip(parsed, data_env)
    })

    test_that("subset column scaling roundtrips", {
        gen_standardize_df <- function(cols, x, ...) {
            train_means <- colMeans(x[, cols, drop = FALSE])
            train_sds <- apply(x[, cols, drop = FALSE], 2, sd)

            list(
                function(x) {
                    x_copy <- x
                    for (col in cols) {
                        x_copy[[col]] <- (x_copy[[col]] - train_means[col]) / train_sds[col]
                    }
                    x_copy
                },
                function(x) {
                    x_copy <- x
                    for (col in cols) {
                        x_copy[[col]] <- x_copy[[col]] * train_sds[col] + train_means[col]
                    }
                    x_copy
                }
            )
        }

        raw_pipe <- list(
            on = "data",
            transforms = list(list(fun = "gen_standardize_df", cols = c("x", "y")))
        )

        original_df <- data.frame(
            x = c(10, 20, 30),
            y = c(100, 200, 300),
            z = c(1, 2, 3)
        )
        data_env <- list(data = original_df)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        expect_roundtrip(parsed, data_env)
    })
})

test_that("log exp transformations roundtrip", {
    expect_roundtrip <- function(pipe, data_env, tolerance = 1e-10) {
        forward_result <- forward_single_pipe(pipe, env = data_env)
        backward_env <- list(data = forward_result)
        backward_result <- backward_single_pipe(pipe, env = backward_env)
        expect_equal(backward_result, data_env$data, tolerance = tolerance)
        invisible(backward_result)
    }

    test_that("simple log exp roundtrips", {
        gen_log_exp <- function(...) {
            list(
                function(x) log(x),
                function(x) exp(x)
            )
        }

        raw_pipe <- list(
            on = "data",
            transforms = list(list(fun = "gen_log_exp"))
        )

        original <- c(1, 2.718282, 7.389056, 20.085537)
        data_env <- list(data = original)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        expect_roundtrip(parsed, data_env, tolerance = 1e-5)
    })

    test_that("log with offset roundtrips", {
        gen_log_offset <- function(offset, ...) {
            list(
                function(x) log(x + offset),
                function(x) exp(x) - offset
            )
        }

        raw_pipe <- list(
            on = "data",
            transforms = list(list(fun = "gen_log_offset", offset = 1))
        )

        original <- c(0, 1, 2, 3, 4, 5)
        data_env <- list(data = original)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        expect_roundtrip(parsed, data_env, tolerance = 1e-10)
    })

    test_that("multiple columns log roundtrip", {
        gen_log_df <- function(cols, ...) {
            list(
                function(x) {
                    x_copy <- x
                    for (col in cols) {
                        x_copy[[col]] <- log(x_copy[[col]])
                    }
                    x_copy
                },
                function(x) {
                    x_copy <- x
                    for (col in cols) {
                        x_copy[[col]] <- exp(x_copy[[col]])
                    }
                    x_copy
                }
            )
        }

        raw_pipe <- list(
            on = "data",
            transforms = list(list(fun = "gen_log_df", cols = c("a", "b")))
        )

        original_df <- data.frame(
            a = c(1, 2, 3, 4),
            b = c(2.718282, 7.389056, 20.085537, 54.598150),
            c = c(100, 200, 300, 400)
        )
        data_env <- list(data = original_df)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        expect_roundtrip(parsed, data_env, tolerance = 1e-5)
    })

    test_that("log then scale roundtrips", {
        gen_log <- function(...) {
            list(
                function(x) log(x),
                function(x) exp(x)
            )
        }

        gen_scale <- function(factor, ...) {
            list(
                function(x) x * factor,
                function(x) x / factor
            )
        }

        raw_pipe <- list(
            on = "data",
            transforms = list(
                list(fun = "gen_log"),
                list(fun = "gen_scale", factor = 2.5)
            )
        )

        original <- c(1, 2.718282, 7.389056)
        data_env <- list(data = original)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        expect_roundtrip(parsed, data_env, tolerance = 1e-5)
    })

    test_that("log with adaptive offset roundtrips", {
        gen_log_adaptive <- function(x, ...) {
            offset <- min(x) - 1
            if (offset >= 0) offset <- 0

            list(
                function(x) log(x - offset),
                function(x) exp(x) + offset
            )
        }

        raw_pipe <- list(
            on = "data",
            transforms = list(list(fun = "gen_log_adaptive"))
        )

        original <- c(1, 2, 3, 4, 5)
        data_env <- list(data = original)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        expect_roundtrip(parsed, data_env, tolerance = 1e-10)
    })
})

test_that("compound transformation roundtrips", {
    expect_roundtrip <- function(pipe, data_env, tolerance = 1e-10) {
        forward_result <- forward_single_pipe(pipe, env = data_env)
        backward_env <- list(data = forward_result)
        backward_result <- backward_single_pipe(pipe, env = backward_env)
        expect_equal(backward_result, data_env$data, tolerance = tolerance)
        invisible(backward_result)
    }

    test_that("log then scale roundtrips", {
        gen_log <- function(...) {
            list(
                function(x) log(x),
                function(x) exp(x)
            )
        }

        gen_scale <- function(factor, ...) {
            list(
                function(x) x * factor,
                function(x) x / factor
            )
        }

        raw_pipe <- list(
            on = "data",
            transforms = list(
                list(fun = "gen_log"),
                list(fun = "gen_scale", factor = 3)
            )
        )

        original <- c(1, 2, 3, 4, 5)
        data_env <- list(data = original)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        expect_roundtrip(parsed, data_env, tolerance = 1e-10)
    })

    test_that("power then scale roundtrips", {
        gen_power <- function(exp, ...) {
            list(
                function(x) x^exp,
                function(x) x^(1 / exp)
            )
        }

        gen_scale <- function(factor, ...) {
            list(
                function(x) x * factor,
                function(x) x / factor
            )
        }

        raw_pipe <- list(
            on = "data",
            transforms = list(
                list(fun = "gen_power", exp = 2),
                list(fun = "gen_scale", factor = 5)
            )
        )

        original <- c(1, 2, 3, 4, 5)
        data_env <- list(data = original)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        expect_roundtrip(parsed, data_env, tolerance = 1e-10)
    })

    test_that("scale then log then power roundtrips", {
        gen_scale <- function(factor, ...) {
            list(
                function(x) x * factor,
                function(x) x / factor
            )
        }

        gen_log <- function(...) {
            list(
                function(x) log(x),
                function(x) exp(x)
            )
        }

        gen_power <- function(exp, ...) {
            list(
                function(x) x^exp,
                function(x) x^(1 / exp)
            )
        }

        raw_pipe <- list(
            on = "data",
            transforms = list(
                list(fun = "gen_scale", factor = 2),
                list(fun = "gen_log"),
                list(fun = "gen_power", exp = 3)
            )
        )

        original <- c(1, 2, 3, 4, 5)
        data_env <- list(data = original)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        expect_roundtrip(parsed, data_env, tolerance = 1e-8)
    })

    test_that("four transform chain roundtrips", {
        gen_add <- function(val, ...) {
            list(
                function(x) x + val,
                function(x) x - val
            )
        }

        gen_mult <- function(val, ...) {
            list(
                function(x) x * val,
                function(x) x / val
            )
        }

        gen_power <- function(exp, ...) {
            list(
                function(x) x^exp,
                function(x) x^(1 / exp)
            )
        }

        gen_log <- function(...) {
            list(
                function(x) log(x),
                function(x) exp(x)
            )
        }

        raw_pipe <- list(
            on = "data",
            transforms = list(
                list(fun = "gen_add", val = 5),
                list(fun = "gen_mult", val = 2),
                list(fun = "gen_log"),
                list(fun = "gen_power", exp = 2)
            )
        )

        original <- c(1, 2, 3, 4, 5)
        data_env <- list(data = original)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        expect_roundtrip(parsed, data_env, tolerance = 1e-8)
    })

    test_that("normalize then standardize roundtrips", {
        gen_normalize <- function(x, ...) {
            min_val <- min(x)
            max_val <- max(x)
            range_val <- max_val - min_val

            list(
                function(x) (x - min_val) / range_val,
                function(x) x * range_val + min_val
            )
        }

        gen_standardize <- function(x, ...) {
            train_mean <- mean(x)
            train_sd <- sd(x)

            list(
                function(x) (x - train_mean) / train_sd,
                function(x) x * train_sd + train_mean
            )
        }

        raw_pipe <- list(
            on = "data",
            transforms = list(
                list(fun = "gen_normalize"),
                list(fun = "gen_standardize")
            )
        )

        original <- c(10, 20, 30, 40, 50, 60)
        data_env <- list(data = original)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        expect_roundtrip(parsed, data_env, tolerance = 1e-10)
    })

    test_that("complex data.frame multi-transform roundtrips", {
        gen_log_df <- function(cols, ...) {
            list(
                function(x) {
                    x_copy <- x
                    for (col in cols) {
                        x_copy[[col]] <- log(x_copy[[col]])
                    }
                    x_copy
                },
                function(x) {
                    x_copy <- x
                    for (col in cols) {
                        x_copy[[col]] <- exp(x_copy[[col]])
                    }
                    x_copy
                }
            )
        }

        gen_standardize_df <- function(cols, x, ...) {
            train_means <- colMeans(x[, cols, drop = FALSE])
            train_sds <- apply(x[, cols, drop = FALSE], 2, sd)

            list(
                function(x) {
                    x_copy <- x
                    for (col in cols) {
                        x_copy[[col]] <- (x_copy[[col]] - train_means[col]) / train_sds[col]
                    }
                    x_copy
                },
                function(x) {
                    x_copy <- x
                    for (col in cols) {
                        x_copy[[col]] <- x_copy[[col]] * train_sds[col] + train_means[col]
                    }
                    x_copy
                }
            )
        }

        raw_pipe <- list(
            on = "data",
            transforms = list(
                list(fun = "gen_log_df", cols = c("a", "b")),
                list(fun = "gen_standardize_df", cols = c("a", "b"))
            )
        )

        original_df <- data.frame(
            a = c(1, 2, 3, 4),
            b = c(2, 4, 6, 8),
            c = c(10, 20, 30, 40)
        )
        data_env <- list(data = original_df)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        expect_roundtrip(parsed, data_env, tolerance = 1e-10)
    })
})

test_that("edge case roundtrips", {
    expect_roundtrip <- function(pipe, data_env, tolerance = 1e-10) {
        forward_result <- forward_single_pipe(pipe, env = data_env)
        backward_env <- list(data = forward_result)
        backward_result <- backward_single_pipe(pipe, env = backward_env)
        expect_equal(backward_result, data_env$data, tolerance = tolerance)
        invisible(backward_result)
    }

    test_that("NA values preserved in roundtrip", {
        gen_scale <- function(factor, ...) {
            list(
                function(x) x * factor,
                function(x) x / factor
            )
        }

        raw_pipe <- list(
            on = "data",
            transforms = list(list(fun = "gen_scale", factor = 2))
        )

        original <- c(1, NA, 3, NA, 5)
        data_env <- list(data = original)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        expect_roundtrip(parsed, data_env)
    })

    test_that("zero values preserved in roundtrip", {
        gen_add_mult <- function(add_val, mult_val, ...) {
            list(
                function(x) (x + add_val) * mult_val,
                function(x) x / mult_val - add_val
            )
        }

        raw_pipe <- list(
            on = "data",
            transforms = list(list(fun = "gen_add_mult", add_val = 10, mult_val = 2))
        )

        original <- c(0, 1, 0, 2, 0, 3)
        data_env <- list(data = original)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        expect_roundtrip(parsed, data_env)
    })

    test_that("negative values preserved in roundtrip", {
        gen_scale <- function(factor, ...) {
            list(
                function(x) x * factor,
                function(x) x / factor
            )
        }

        raw_pipe <- list(
            on = "data",
            transforms = list(list(fun = "gen_scale", factor = 3))
        )

        original <- c(-5, -3, -1, 0, 1, 3, 5)
        data_env <- list(data = original)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        expect_roundtrip(parsed, data_env)
    })

    test_that("extreme values roundtrip", {
        gen_scale <- function(factor, ...) {
            list(
                function(x) x * factor,
                function(x) x / factor
            )
        }

        raw_pipe <- list(
            on = "data",
            transforms = list(list(fun = "gen_scale", factor = 2))
        )

        original <- c(1e-10, 1e-5, 1, 1e5, 1e10)
        data_env <- list(data = original)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        expect_roundtrip(parsed, data_env)
    })

    test_that("infinite values handled in roundtrip", {
        gen_scale <- function(factor, ...) {
            list(
                function(x) x * factor,
                function(x) x / factor
            )
        }

        raw_pipe <- list(
            on = "data",
            transforms = list(list(fun = "gen_scale", factor = 2))
        )

        original <- c(-Inf, -100, 0, 100, Inf)
        data_env <- list(data = original)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        expect_roundtrip(parsed, data_env)
    })

    test_that("single element vector roundtrips", {
        gen_scale <- function(factor, ...) {
            list(
                function(x) x * factor,
                function(x) x / factor
            )
        }

        raw_pipe <- list(
            on = "data",
            transforms = list(list(fun = "gen_scale", factor = 5))
        )

        original <- c(42)
        data_env <- list(data = original)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        expect_roundtrip(parsed, data_env)
    })

    test_that("zero-row data.frame roundtrips", {
        gen_scale_df <- function(cols, factor, ...) {
            list(
                function(x) {
                    x_copy <- x
                    for (col in cols) {
                        x_copy[[col]] <- x_copy[[col]] * factor
                    }
                    x_copy
                },
                function(x) {
                    x_copy <- x
                    for (col in cols) {
                        x_copy[[col]] <- x_copy[[col]] / factor
                    }
                    x_copy
                }
            )
        }

        raw_pipe <- list(
            on = "data",
            transforms = list(list(fun = "gen_scale_df", cols = c("a", "b"), factor = 2))
        )

        original_df <- data.frame(a = numeric(0), b = numeric(0), c = numeric(0))
        data_env <- list(data = original_df)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        expect_roundtrip(parsed, data_env)
    })

    test_that("mixed NA and extreme values roundtrip", {
        gen_standardize <- function(x, ...) {
            train_mean <- mean(x, na.rm = TRUE)
            train_sd <- sd(x, na.rm = TRUE)

            list(
                function(x) (x - train_mean) / train_sd,
                function(x) x * train_sd + train_mean
            )
        }

        raw_pipe <- list(
            on = "data",
            transforms = list(list(fun = "gen_standardize"))
        )

        original <- c(NA, -1e10, 0, 1e10, NA)
        data_env <- list(data = original)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        expect_roundtrip(parsed, data_env)
    })
})

test_that("non-reversible transform behavior", {
    expect_roundtrip <- function(pipe, data_env, tolerance = 1e-10) {
        forward_result <- forward_single_pipe(pipe, env = data_env)
        backward_env <- list(data = forward_result)
        backward_result <- backward_single_pipe(pipe, env = backward_env)
        expect_equal(backward_result, data_env$data, tolerance = tolerance)
        invisible(backward_result)
    }

    test_that("single closure uses identity backward", {
        gen_single <- function(...) {
            function(x) x * 2
        }

        raw_pipe <- list(
            on = "data",
            transforms = list(list(fun = "gen_single"))
        )

        original <- c(1, 2, 3, 4, 5)
        data_env <- list(data = original)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        forward_result <- forward_single_pipe(parsed, env = data_env)
        backward_result <- backward_single_pipe(
            parsed,
            env = list(data = forward_result)
        )

        expect_equal(backward_result, forward_result, tolerance = 1e-10)
        expect_false(isTRUE(all.equal(backward_result, original, tolerance = 1e-10)))
    })

    test_that("identity roundtrip returns forward result", {
        gen_square <- function(...) {
            function(x) x^2
        }

        raw_pipe <- list(
            on = "data",
            transforms = list(list(fun = "gen_square"))
        )

        original <- c(1, 2, 3, 4, 5)
        data_env <- list(data = original)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        forward_result <- forward_single_pipe(parsed, env = data_env)
        expected_forward <- c(1, 4, 9, 16, 25)
        expect_equal(forward_result, expected_forward, tolerance = 1e-10)

        backward_result <- backward_single_pipe(
            parsed,
            env = list(data = forward_result)
        )

        expect_equal(backward_result, expected_forward, tolerance = 1e-10)
    })

    test_that("non-reversible does not crash", {
        gen_abs <- function(...) {
            function(x) abs(x)
        }

        raw_pipe <- list(
            on = "data",
            transforms = list(list(fun = "gen_abs"))
        )

        original <- c(-5, -3, -1, 1, 3, 5)
        data_env <- list(data = original)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        forward_result <- forward_single_pipe(parsed, env = data_env)
        backward_result <- backward_single_pipe(
            parsed,
            env = list(data = forward_result)
        )

        expect_equal(backward_result, forward_result, tolerance = 1e-10)
    })

    test_that("mixed reversible and non-reversible transforms", {
        gen_reversible <- function(val, ...) {
            list(
                function(x) x * val,
                function(x) x / val
            )
        }

        gen_non_reversible <- function(...) {
            function(x) abs(x)
        }

        raw_pipe <- list(
            on = "data",
            transforms = list(
                list(fun = "gen_reversible", val = 2),
                list(fun = "gen_non_reversible"),
                list(fun = "gen_reversible", val = 3)
            )
        )

        original <- c(-5, -3, 1, 3, 5)
        data_env <- list(data = original)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        forward_result <- forward_single_pipe(parsed, env = data_env)
        expected_forward <- abs(original * 2) * 3
        expect_equal(forward_result, expected_forward, tolerance = 1e-10)

        backward_result <- backward_single_pipe(
            parsed,
            env = list(data = forward_result)
        )

        expected_backward <- abs(original)
        expect_equal(backward_result, expected_backward, tolerance = 1e-10)
    })

    test_that("non-reversible behavior is documented via identity", {
        gen_clip <- function(min_val, max_val, ...) {
            function(x) pmin(pmax(x, min_val), max_val)
        }

        raw_pipe <- list(
            on = "data",
            transforms = list(list(fun = "gen_clip", min_val = 0, max_val = 10))
        )

        original <- c(-5, 0, 5, 10, 15)
        data_env <- list(data = original)
        parsed <- parse_single_pipe(raw_pipe, env = data_env)

        sc <- parsed$transforms[[1]]

        expect_true(is.function(sc$forward))
        expect_true(is.function(sc$backward))

        test_val <- c(-5, 0, 5, 10, 15)
        forward_result <- sc$forward(test_val)
        backward_result <- sc$backward(forward_result)

        expect_equal(backward_result, forward_result, tolerance = 1e-10)
    })
})
