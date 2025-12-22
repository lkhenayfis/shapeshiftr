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
        expect_true(all(sapply(l_t, inherits, what = "function")))

        expect_equal(formalArgs(l_t[[1]]), "x")
        expect_equal(formalArgs(l_t[[2]]), "x")

        expect_equal(deparse(body(l_t[[1]])), "x[x[[by]] == value, ]")
        expect_equal(deparse(body(l_t[[2]])), "summary(x)")
    })

    test_that("env e uma lista/data.frame", {

        parsed_pipe <- f(raw_pipes[[1]], env = data_list, enclos = env)

        expect_true(inherits(parsed_pipe, "list"))
        expect_equal(length(parsed_pipe), 2)
        expect_equal(names(parsed_pipe), c("on", "transforms"))

        expect_equal(parsed_pipe$on, "mtcars")

        l_t <- parsed_pipe$transforms

        expect_equal(length(l_t), 2)
        expect_true(all(sapply(l_t, inherits, what = "function")))

        expect_equal(formalArgs(l_t[[1]]), "x")
        expect_equal(formalArgs(l_t[[2]]), "x")

        expect_equal(deparse(body(l_t[[1]])), "x[x[[by]] == value, ]")
        expect_equal(deparse(body(l_t[[2]])), "summary(x)")
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
            expect_true(all(sapply(l_t, inherits, what = "function")))
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
