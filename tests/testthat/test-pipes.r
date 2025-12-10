test_that("parse_single_pipe", {

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