gen_closure_filter <- function(by, value, ...) function(x) x[x[[by]] == value, ]
gen_closure_summary <- function(...) function(x) summary(x)

raw_pipe <- list(
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
)

test_that("parse_single_pipe", {

    parsed_pipe <- parse_single_pipe(raw_pipe)

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

test_that("eval_single_pipe", {
    parsed_pipe <- parse_single_pipe(raw_pipe)

    eval_pipe <- eval_single_pipe(parsed_pipe)

    expect_true(inherits(eval_pipe, "table"))
    expect_equal(eval_pipe[1, 1], "Min.   :17.80  ")
    expect_equal(eval_pipe[1, 2], "Min.   :6  ")
})