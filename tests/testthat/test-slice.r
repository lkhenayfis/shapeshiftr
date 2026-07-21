
generate_simple_params <- function(type = c("date", "datetime")) {
    type <- match.arg(type)
    if (type == "date") {
        parse_slice_args(simple_dt_date,
            c("X1", "Y"),
            "date",
            "date",
            list(-3:0, 1:2),
            1,
            1,
            c("name_1", "name_2")
        )
    } else {
        parse_slice_args(simple_dt_datetime,
            c("X1", "Y"),
            "datetime",
            "datetime",
            list(-3:0, 1:2),
            1,
            1,
            c("name_1", "name_2")
        )
    }
}

generate_keyed_params <- function(type = c("date", "datetime")) {
    type <- match.arg(type)
    if (type == "date") {
        parse_slice_args(keyed_dt_date,
            c("X1", "X2"),
            "date",
            "target_date",
            list(0:3, 1:2),
            1,
            1,
            c("name_1", "name_2")
        )
    } else {
        parse_slice_args(keyed_dt_datetime,
            c("X1", "X2"),
            "datetime",
            "target_datetime",
            list(0:3, 1:2),
            1,
            1,
            c("name_1", "name_2")
        )
    }
}

build_simple_mts <- function(variables, n = 20, start = c(2000, 9), frequency = 12) {
    offsets <- seq_along(variables) * 100
    values <- vapply(offsets, function(offset) offset + seq_len(n), numeric(n))
    colnames(values) <- variables
    ts(values, start = start, frequency = frequency)
}

test_that("do_single_slice", {

    test_that("simple - Date", {
        params <- generate_simple_params("date")

        center <- as.Date("2025-01-02")
        slice <- do_single_slice(simple_dt_date, center, params)

        expect_true(inherits(slice, "slice_artifact"))
        expect_equal(attr(slice, "L"), structure(params$L, names = params$names))
        expect_equal(attr(slice, "index"), center)
        expect_equal(names(slice), params$names)

        expect_equal(slice$name_1[[1]], c(NA, NA, NA, 16))
        expect_equal(slice$name_2[[1]], c(122, 138))

        center <- as.Date("2025-01-21")
        slice <- do_single_slice(simple_dt_date, center, params)

        expect_true(inherits(slice, "slice_artifact"))
        expect_equal(attr(slice, "L"), structure(params$L, names = params$names))
        expect_equal(attr(slice, "index"), center)
        expect_equal(names(slice), params$names)

        expect_equal(slice$name_1[[1]], c(17, 1, 18, 3))
        expect_equal(slice$name_2[[1]], c(NA_integer_, NA_integer_))

        center <- as.Date("2025-01-10")
        slice <- do_single_slice(simple_dt_date, center, params)

        expect_true(inherits(slice, "slice_artifact"))
        expect_equal(attr(slice, "L"), structure(params$L, names = params$names))
        expect_equal(attr(slice, "index"), center)
        expect_equal(names(slice), params$names)

        expect_equal(slice$name_1[[1]], c(19, 6, 4, 2))
        expect_equal(slice$name_2[[1]], c(139, 136))
    })

    test_that("simple - POSIX", {
        params <- generate_simple_params("datetime")

        center <- as.POSIXct("2025-01-01 01:00:00", "GMT")
        slice <- do_single_slice(simple_dt_datetime, center, params)

        expect_true(inherits(slice, "slice_artifact"))
        expect_equal(attr(slice, "L"), structure(params$L, names = params$names))
        expect_equal(attr(slice, "index"), center)
        expect_equal(names(slice), params$names)

        expect_equal(slice$name_1[[1]], c(NA, NA, NA, 16))
        expect_equal(slice$name_2[[1]], c(122, 138))

        center <- as.POSIXct("2025-01-01 10:00:00", "GMT")
        slice <- do_single_slice(simple_dt_datetime, center, params)

        expect_true(inherits(slice, "slice_artifact"))
        expect_equal(attr(slice, "L"), structure(params$L, names = params$names))
        expect_equal(attr(slice, "index"), center)
        expect_equal(names(slice), params$names)

        expect_equal(slice$name_1[[1]], c(6, 4, 2, 7))
        expect_equal(slice$name_2[[1]], c(136, 124))

        center <- as.POSIXct("2025-01-01 20:00:00", "GMT")
        slice <- do_single_slice(simple_dt_datetime, center, params)

        expect_true(inherits(slice, "slice_artifact"))
        expect_equal(attr(slice, "L"), structure(params$L, names = params$names))
        expect_equal(attr(slice, "index"), center)
        expect_equal(names(slice), params$names)

        expect_equal(slice$name_1[[1]], c(17, 1, 18, 3))
        expect_equal(slice$name_2[[1]], c(NA_integer_, NA_integer_))
    })

    test_that("keyed - Date", {
        params <- generate_keyed_params("date")

        center <- as.Date("2025-01-02")
        slice <- do_single_slice(keyed_dt_date, center, params)

        expect_true(inherits(slice, "slice_artifact"))
        expect_equal(attr(slice, "L"), structure(params$L, names = params$names))
        expect_equal(attr(slice, "index"), center)
        expect_equal(names(slice), params$names)

        expect_equal(slice$name_1[[1]], c(15, 12, 3, 20))
        expect_equal(slice$name_2[[1]], c(-12, -16))

        center <- as.Date("2025-01-21")
        slice <- do_single_slice(keyed_dt_date[1:97], center, params)

        expect_true(inherits(slice, "slice_artifact"))
        expect_equal(attr(slice, "L"), structure(params$L, names = params$names))
        expect_equal(attr(slice, "index"), center)
        expect_equal(names(slice), params$names)

        expect_equal(slice$name_1[[1]], c(10, 9, NA, NA))
        expect_equal(slice$name_2[[1]], c(-20, NA))

        center <- as.Date("2025-01-10")
        slice <- do_single_slice(keyed_dt_date, center, params)

        expect_true(inherits(slice, "slice_artifact"))
        expect_equal(attr(slice, "L"), structure(params$L, names = params$names))
        expect_equal(attr(slice, "index"), center)
        expect_equal(names(slice), params$names)

        expect_equal(slice$name_1[[1]], c(15, 12, 3, 20))
        expect_equal(slice$name_2[[1]], c(-12, -16))
    })

    test_that("keyed - POSIX", {
        params <- generate_keyed_params("datetime")

        center <- as.POSIXct("2025-01-01 01:00:00", "GMT")
        slice <- do_single_slice(keyed_dt_datetime, center, params)

        expect_true(inherits(slice, "slice_artifact"))
        expect_equal(attr(slice, "L"), structure(params$L, names = params$names))
        expect_equal(attr(slice, "index"), center)
        expect_equal(names(slice), params$names)

        expect_equal(slice$name_1[[1]], c(27, 30, 34, 40))
        expect_equal(slice$name_2[[1]], c(-56, -83))

        center <- as.POSIXct("2025-01-01 10:00:00", "GMT")
        slice <- do_single_slice(keyed_dt_datetime[1:97], center, params)

        expect_true(inherits(slice, "slice_artifact"))
        expect_equal(attr(slice, "L"), structure(params$L, names = params$names))
        expect_equal(attr(slice, "index"), center)
        expect_equal(names(slice), params$names)

        expect_equal(slice$name_1[[1]], c(86, 53, 49, 98))
        expect_equal(slice$name_2[[1]], c(-39, -69))

        center <- as.POSIXct("2025-01-01 20:00:00", "GMT")
        slice <- do_single_slice(keyed_dt_datetime[1:97], center, params)

        expect_true(inherits(slice, "slice_artifact"))
        expect_equal(attr(slice, "L"), structure(params$L, names = params$names))
        expect_equal(attr(slice, "index"), center)
        expect_equal(names(slice), params$names)

        expect_equal(slice$name_1[[1]], c(36, 91, NA, NA))
        expect_equal(slice$name_2[[1]], c(-42, NA))
    })
})

test_that("do_slices", {

    test_that("simple - Date", {
        params <- generate_simple_params("date")
        slices <- do_slices(simple_dt_date, params)
        expect_equal(attr(slices, "index"), simple_dt_date$date)

        for (center in as.Date(c("2025-01-02", "2025-01-10", "2025-01-21"))) {
            center <- as.Date(center)
            compare <- do_single_slice(simple_dt_date, center, params)
            expect_equal(slices[, center], compare)
        }
    })

    test_that("simple - POSIX", {
        params <- generate_simple_params("datetime")
        slices <- do_slices(simple_dt_datetime, params)
        expect_equal(attr(slices, "index"), simple_dt_datetime$datetime)

        centers <- as.POSIXct(
            c("2025-01-01 01:00:00", "2025-01-01 10:00:00", "2025-01-01 20:00:00"), "GMT"
        )
        for (center in centers) {
            center <- as.POSIXct(center, origin = "1970-01-01", tz = "GMT")
            compare <- do_single_slice(simple_dt_datetime, center, params)
            expect_equal(slices[, center], compare)
        }
    })

    test_that("keyed - Date", {
        params <- generate_keyed_params("date")
        slices <- do_slices(keyed_dt_date[1:97], params)
        expect_equal(attr(slices, "index"), unique(keyed_dt_date$date))

        for (center in as.Date(c("2025-01-02", "2025-01-10", "2025-01-21"))) {
            center <- as.Date(center)
            compare <- do_single_slice(keyed_dt_date[1:97], center, params)
            expect_equal(slices[, center], compare)
        }
    })

    test_that("keyed - POSIX", {
        params <- generate_keyed_params("datetime")
        slices <- do_slices(keyed_dt_datetime[1:97], params)
        expect_equal(attr(slices, "index"), unique(keyed_dt_datetime$datetime))

        centers <- as.POSIXct(
            c("2025-01-01 01:00:00", "2025-01-01 10:00:00", "2025-01-01 20:00:00"), "GMT"
        )
        for (center in centers) {
            center <- as.POSIXct(center, origin = "1970-01-01", tz = "GMT")
            compare <- do_single_slice(keyed_dt_datetime[1:97], center, params)
            expect_equal(slices[, center], compare)
        }
    })
})

test_that("slice", {

    test_that("dispatches to slice.default for data.table input", {
        sliced <- slice(simple_dt_date, walk_on = "date", L = -1)

        expect_true(inherits(sliced, "slice_artifact"))
        expect_equal(names(sliced), setdiff(colnames(simple_dt_date), "date"))
    })

    test_that("dispatches to slice.ts for ts input", {
        x <- ts(1:20, start = c(2000, 1), frequency = 12)
        sliced <- slice(x, L = -1)

        expect_true(inherits(sliced, "slice_artifact"))
        expect_equal(names(sliced), "value")
        expect_true(is.na(sliced$value[[1]]))
        expect_equal(sliced$value[[2]], 1)
    })

    test_that("dispatches to slice.mts for mts input", {
        m <- ts(cbind(A = 1:20, B = 101:120), start = c(2000, 1), frequency = 12)
        sliced <- slice(m, variables = c("A", "B"), L = -1)

        expect_true(inherits(sliced, "slice_artifact"))
        expect_equal(names(sliced), c("A", "B"))
        expect_equal(sliced$A[[2]], 1)
        expect_equal(sliced$B[[2]], 101)
    })
})

test_that("slice.default", {

    test_that("succeeds when 'variables' is omitted, slicing all non-index columns", {
        sliced <- slice(simple_dt_date, walk_on = "date", L = -1)

        expect_true(inherits(sliced, "slice_artifact"))
        expect_equal(names(sliced), setdiff(colnames(simple_dt_date), "date"))
        expect_true(is.na(sliced$X1[[1]]))
        expect_equal(sliced$X1[[2]], simple_dt_date$X1[1])
    })

    test_that("succeeds when both 'variables' and 'walk_on' are omitted", {
        sliced <- slice(simple_dt_date, L = -1)

        expect_true(inherits(sliced, "slice_artifact"))
        expect_equal(attr(sliced, "index"), simple_dt_date$date)
        expect_equal(names(sliced), setdiff(colnames(simple_dt_date), "date"))
        expect_equal(sliced$X1[[2]], simple_dt_date$X1[1])
    })

    test_that("explicit 'variables' and 'walk_on' slice only the requested columns", {
        sliced <- slice.default(simple_dt_date,
            variables = c("X1", "X2"), walk_on = "date", L = -1)

        expect_equal(names(sliced), c("X1", "X2"))
        expect_true(is.na(sliced$X1[[1]]))
        expect_equal(sliced$X1[[2]], 16)
        expect_equal(sliced$X2[[2]], -18)
    })

    test_that("keyed slicing groups by 'walk_on' and slices on 'slice_on'", {
        sliced <- slice.default(keyed_dt_date, variables = "X1", walk_on = "date",
            slice_on = "target_date", L = c(1, 2))

        expect_equal(dim(sliced), c(1, 20))
        expect_equal(attr(sliced, "index")[1], as.Date("2025-01-02"))
        expect_equal(sliced$X1[[1]], c(12, 3))
    })

    test_that("'L' as a scalar applies the same lag to every variable", {
        sliced <- slice.default(simple_dt_date,
            variables = c("X1", "X2"), walk_on = "date", L = -2)

        expect_equal(sliced$X1[[3]], 16)
        expect_equal(sliced$X2[[3]], -18)
        expect_true(is.na(sliced$X1[[1]]))
        expect_true(is.na(sliced$X1[[2]]))
    })

    test_that("'L' as a vector applies the same lag structure to every variable", {
        sliced <- slice.default(simple_dt_date, variables = c("X1", "X2"), walk_on = "date",
            L = c(-1, -2))

        expect_equal(attr(sliced, "L")$X1, c(-1, -2))
        expect_equal(attr(sliced, "L")$X2, c(-1, -2))
        expect_equal(sliced$X1[[3]], c(5, 16))
        expect_equal(sliced$X2[[3]], c(-17, -18))
    })

    test_that("'L' as a named list assigns per-variable lag structures", {
        sliced <- slice.default(simple_dt_date, walk_on = "date",
            variables = c("X1", "X2"), L = list(X1 = -1, X2 = c(-1, -2)))

        expect_equal(sliced$X1[[3]], 5)
        expect_equal(sliced$X2[[3]], c(-17, -18))
    })

    test_that("custom 'start' and 'step' control the walked slice times", {
        sliced <- slice.default(simple_dt_date, variables = "X1", walk_on = "date",
            L = -1, start = 3, step = 2)
        idx <- attr(sliced, "index")

        expect_equal(idx[1], as.Date("2025-01-04"))
        expect_true(all(diff(idx) == 2))
    })

    test_that("custom 'names' are honored for the sliced variables", {
        sliced <- slice.default(simple_dt_date, variables = c("X1", "Y"), walk_on = "date",
            L = -1, names = c("lag_x1", "lag_y"))

        expect_equal(names(sliced), c("lag_x1", "lag_y"))
        expect_equal(names(attr(sliced, "L")), c("lag_x1", "lag_y"))
    })
})

test_that("slice.ts", {

    test_that("dispatches through slice.default using ts2dt", {
        x <- ts(1:20, start = c(2000, 1), frequency = 12)
        sliced <- slice.ts(x, L = -1)

        expect_true(inherits(sliced, "slice_artifact"))
        expect_equal(names(sliced), "value")
        expect_true(is.na(sliced$value[[1]]))
        expect_equal(sliced$value[[2]], 1)
        expect_true(inherits(attr(sliced, "index"), "int_time"))
    })
})

test_that("slice.mts", {

    test_that("dispatches through slice.default using ts2dt for multiple variables", {
        m <- build_simple_mts(c("X1", "Y"))
        sliced <- slice.mts(m, variables = c("X1", "Y"), L = list(X1 = -4:0, Y = 1:3))

        expect_true(inherits(sliced, "slice_artifact"))
        expect_equal(names(sliced), c("X1", "Y"))
        expect_equal(attr(sliced, "L")$X1, -4:0)
        expect_equal(attr(sliced, "L")$Y, 1:3)
    })
})
