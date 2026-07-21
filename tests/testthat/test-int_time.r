
build_ts <- function(n, start, frequency) {
    ts(seq_len(n), start = start, frequency = frequency)
}

expected_int_codes <- function(start, n, frequency) {
    start_year <- start[1]

    if (frequency == 1) return(start_year + seq_len(n) - 1)

    start_season <- start[2]
    extra <- ceiling(log10(frequency))
    offset <- start_season - 1 + seq_len(n) - 1
    season <- offset %% frequency + 1
    year <- start_year + offset %/% frequency

    year * 10^extra + season
}

generate_int_time <- function(type = c("annual", "quarterly", "weekly", "monthly", "weekly52")) {
    type <- match.arg(type)
    mc <- match.call()
    fun <- as.name(paste0("generate_int_time_", type))
    mc[[1]] <- fun
    eval(mc, parent.frame())
}

generate_int_time_annual <- function(...) {
    x <- build_ts(10, 2000, 1)
    list(x = x, it = int_time(x))
}

generate_int_time_quarterly <- function(...) {
    x <- build_ts(20, c(1987, 3), 4)
    list(x = x, it = int_time(x))
}

generate_int_time_weekly <- function(...) {
    x <- build_ts(38, c(2000, 1), 7)
    list(x = x, it = int_time(x))
}

generate_int_time_monthly <- function(...) {
    x <- build_ts(36, c(2000, 1), 12)
    list(x = x, it = int_time(x))
}

generate_int_time_weekly52 <- function(...) {
    x <- build_ts(114, c(1987, 3), 52)
    list(x = x, it = int_time(x))
}

test_that("int_time.ts", {

    test_that("annual", {
        fx <- generate_int_time("annual")
        expected <- expected_int_codes(start(fx$x), length(fx$x), frequency(fx$x))

        expect_equal(as.numeric(fx$it), expected)
        expect_equal(as.numeric(fx$it), 2000:2009)
        expect_equal(attr(fx$it, "frequency"), 1)
        expect_equal(attr(fx$it, "extra"), 0)
        expect_equal(class(fx$it), c("int_time", "numeric"))
        expect_equal(attr(fx$it, "inner_times"), as.numeric(time(fx$x)))
    })

    test_that("quarterly", {
        fx <- generate_int_time("quarterly")
        expected <- expected_int_codes(start(fx$x), length(fx$x), frequency(fx$x))

        expect_equal(as.numeric(fx$it), expected)
        expect_equal(attr(fx$it, "frequency"), 4)
        expect_equal(attr(fx$it, "extra"), 1)
        expect_equal(class(fx$it), c("int_time", "numeric"))
        expect_equal(attr(fx$it, "inner_times"), as.numeric(time(fx$x)))

        modulus <- 10^attr(fx$it, "extra")
        seasons <- unclass(fx$it) %% modulus
        expect_true(all(seasons >= 1))
        expect_true(all(seasons <= 4))

        expect_equal(as.numeric(fx$it[2]), 19874)
        expect_equal(as.numeric(fx$it[3]), 19881)
    })

    test_that("weekly", {
        fx <- generate_int_time("weekly")
        expected <- expected_int_codes(start(fx$x), length(fx$x), frequency(fx$x))

        expect_equal(as.numeric(fx$it), expected)
        expect_equal(attr(fx$it, "frequency"), 7)
        expect_equal(attr(fx$it, "extra"), 1)
        expect_equal(class(fx$it), c("int_time", "numeric"))
        expect_equal(attr(fx$it, "inner_times"), as.numeric(time(fx$x)))

        modulus <- 10^attr(fx$it, "extra")
        seasons <- unclass(fx$it) %% modulus
        expect_true(all(seasons >= 1))
        expect_true(all(seasons <= 7))
    })

    test_that("monthly", {
        fx <- generate_int_time("monthly")
        expected <- expected_int_codes(start(fx$x), length(fx$x), frequency(fx$x))

        expect_equal(as.numeric(fx$it), expected)
        expect_equal(attr(fx$it, "frequency"), 12)
        expect_equal(attr(fx$it, "extra"), 2)
        expect_equal(class(fx$it), c("int_time", "numeric"))
        expect_equal(attr(fx$it, "inner_times"), as.numeric(time(fx$x)))

        modulus <- 10^attr(fx$it, "extra")
        seasons <- unclass(fx$it) %% modulus
        expect_true(all(seasons >= 1))
        expect_true(all(seasons <= 12))

        expect_equal(as.numeric(fx$it[12]), 200012)
        expect_equal(as.numeric(fx$it[13]), 200101)
    })

    test_that("weekly52", {
        fx <- generate_int_time("weekly52")
        expected <- expected_int_codes(start(fx$x), length(fx$x), frequency(fx$x))

        expect_equal(as.numeric(fx$it), expected)
        expect_equal(attr(fx$it, "frequency"), 52)
        expect_equal(attr(fx$it, "extra"), 2)
        expect_equal(class(fx$it), c("int_time", "numeric"))
        expect_equal(attr(fx$it, "inner_times"), as.numeric(time(fx$x)))

        modulus <- 10^attr(fx$it, "extra")
        seasons <- unclass(fx$it) %% modulus
        expect_true(all(seasons >= 1))
        expect_true(all(seasons <= 52))
    })

    test_that("non-2000 start year", {
        x <- build_ts(24, c(1987, 5), 12)
        it <- int_time(x)
        expected <- expected_int_codes(start(x), length(x), frequency(x))

        expect_equal(as.numeric(it), expected)
        expect_equal(as.numeric(it)[1], 198705)
    })
})

test_that("int_time.numeric", {

    test_that("matches int_time.ts on the same series", {
        fx <- generate_int_time("monthly")
        from_numeric <- int_time.numeric(as.numeric(time(fx$x)), frequency(fx$x))

        expect_equal(as.numeric(from_numeric), as.numeric(fx$it))
        expect_equal(attr(from_numeric, "inner_times"), attr(fx$it, "inner_times"))
    })

    test_that("frequency 1 encodes to the integer year", {
        encoded <- int_time.numeric(2000:2009, 1)

        expect_equal(as.numeric(encoded), 2000:2009)
        expect_equal(attr(encoded, "extra"), 0)
        expect_equal(decode_int_time(encoded), 2000:2009, tolerance = 1e-6)
    })
})

test_that("decode_int_time", {

    test_that("annual", {
        fx <- generate_int_time("annual")
        expect_equal(decode_int_time(fx$it), as.numeric(time(fx$x)), tolerance = 1e-6)
    })

    test_that("quarterly", {
        fx <- generate_int_time("quarterly")
        expect_equal(decode_int_time(fx$it), as.numeric(time(fx$x)), tolerance = 1e-6)
    })

    test_that("weekly", {
        fx <- generate_int_time("weekly")
        expect_equal(decode_int_time(fx$it), as.numeric(time(fx$x)), tolerance = 1e-6)
    })

    test_that("monthly", {
        fx <- generate_int_time("monthly")
        expect_equal(decode_int_time(fx$it), as.numeric(time(fx$x)), tolerance = 1e-6)
    })

    test_that("weekly52", {
        fx <- generate_int_time("weekly52")
        expect_equal(decode_int_time(fx$it), as.numeric(time(fx$x)), tolerance = 1e-6)
    })
})

test_that("`[.int_time`", {

    test_that("preserves class and attributes on a range subset", {
        fx <- generate_int_time("monthly")
        sub <- fx$it[6:10]

        expect_equal(class(sub), c("int_time", "numeric"))
        expect_equal(attr(sub, "frequency"), 12)
        expect_equal(attr(sub, "extra"), 2)
        expect_equal(as.numeric(sub), as.numeric(fx$it)[6:10])
        expect_equal(attr(sub, "inner_times"), attr(fx$it, "inner_times")[6:10])
    })
})

test_that("`[[.int_time`", {

    test_that("extracts a single element matching `[`", {
        fx <- generate_int_time("monthly")

        expect_equal(fx$it[[5]], fx$it[5])
        expect_equal(as.numeric(fx$it[[5]]), 200005)
        expect_equal(attr(fx$it[[5]], "inner_times"), attr(fx$it[5], "inner_times"))
    })
})

test_that("`+.int_time`", {

    test_that("shifts within a series", {
        it <- generate_int_time("monthly")$it

        expect_equal(as.numeric(it[1] + 5), as.numeric(it[6]))
        expect_equal(as.numeric(it[1] + 25), as.numeric(it[26]))
        expect_equal(as.numeric(it[26] + (-25)), as.numeric(it[1]))

        it_q <- generate_int_time("quarterly")$it
        expect_equal(as.numeric(it_q[1] + 4), as.numeric(it_q[5]))
    })

    test_that("crosses year boundaries in both directions", {
        it <- generate_int_time("monthly")$it
        expect_equal(as.numeric(it[12] + 1), as.numeric(it[13]))
        expect_equal(as.numeric(it[13] + (-1)), as.numeric(it[12]))

        it_q <- generate_int_time("quarterly")$it
        expect_equal(as.numeric(it_q[2] + 1), as.numeric(it_q[3]))
        expect_equal(as.numeric(it_q[3] + (-1)), as.numeric(it_q[2]))
    })
})

test_that("`c.int_time`", {

    test_that("concatenates values and inner_times preserving attributes", {
        it1 <- int_time(build_ts(12, c(2000, 1), 12))
        it2 <- int_time(build_ts(12, c(2001, 1), 12))
        combined <- c(it1, it2)

        expect_equal(class(combined), c("int_time", "numeric"))
        expect_equal(attr(combined, "frequency"), 12)
        expect_equal(attr(combined, "extra"), 2)
        expect_equal(as.numeric(combined), c(as.numeric(it1), as.numeric(it2)))
        expect_equal(
            attr(combined, "inner_times"),
            c(attr(it1, "inner_times"), attr(it2, "inner_times"))
        )
    })
})

test_that("seq.int_time", {

    test_that("with an explicit upper bound", {
        it <- generate_int_time("monthly")$it

        generated <- seq(it[1], it[24], by = 1)
        expect_equal(as.numeric(generated), as.numeric(it[1:24]))

        generated_step <- seq(it[1], it[24], by = 3)
        expect_equal(as.numeric(generated_step), as.numeric(it[seq(1, 24, by = 3)]))
    })

    test_that("with length.out", {
        it <- generate_int_time("monthly")$it

        generated <- seq(it[1], by = 1, length.out = 5)
        expect_equal(as.numeric(generated), as.numeric(it[1:5]))
        expect_equal(class(generated), c("int_time", "numeric"))
    })
})

test_that("is.numeric.int_time", {

    test_that("returns FALSE", {
        it <- generate_int_time("monthly")$it
        expect_false(is.numeric(it))
    })
})
