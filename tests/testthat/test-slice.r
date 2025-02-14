
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
            c("name_1", "name_2"),
            1
        )
    } else {
        parse_slice_args(simple_dt_datetime,
            c("X1", "Y"),
            "datetime",
            "datetime",
            list(-3:0, 1:2),
            1,
            1,
            c("name_1", "name_2"),
            1
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
            c("name_1", "name_2"),
            1
        )
    } else {
        parse_slice_args(keyed_dt_datetime,
            c("X1", "X2"),
            "datetime",
            "target_datetime",
            list(0:3, 1:2),
            1,
            1,
            c("name_1", "name_2"),
            1
        )
    }
}

test_that("do_single_slice - Date", {

    # simple -------------------------------------------------------------

    params <- generate_simple_params("date")

    center <- as.Date("2025-01-02")
    slice <- do_single_slice(simple_dt_date, center, params)

    expect_true(inherits(slice, "slice_artifact"))
    expect_equal(attr(slice, "L"), params$L)
    expect_equal(attr(slice, "index"), center)
    expect_equal(names(slice), params$names)

    expect_equal(slice$name_1[[1]], c(NA, NA, NA, 16))
    expect_equal(slice$name_2[[1]], c(122, 138))

    center <- as.Date("2025-01-21")
    slice <- do_single_slice(simple_dt_date, center, params)

    expect_true(inherits(slice, "slice_artifact"))
    expect_equal(attr(slice, "L"), params$L)
    expect_equal(attr(slice, "index"), center)
    expect_equal(names(slice), params$names)

    expect_equal(slice$name_1[[1]], c(17, 1, 18, 3))
    expect_equal(slice$name_2[[1]], c(NA_integer_, NA_integer_))

    center <- as.Date("2025-01-10")
    slice <- do_single_slice(simple_dt_date, center, params)

    expect_true(inherits(slice, "slice_artifact"))
    expect_equal(attr(slice, "L"), params$L)
    expect_equal(attr(slice, "index"), center)
    expect_equal(names(slice), params$names)

    expect_equal(slice$name_1[[1]], c(19, 6, 4, 2))
    expect_equal(slice$name_2[[1]], c(139, 136))

    # keyed --------------------------------------------------------------

    params <- generate_keyed_params("date")

    center <- as.Date("2025-01-02")
    slice <- do_single_slice(keyed_dt_date, center, params)

    expect_true(inherits(slice, "slice_artifact"))
    expect_equal(attr(slice, "L"), params$L)
    expect_equal(attr(slice, "index"), center)
    expect_equal(names(slice), params$names)

    expect_equal(slice$name_1[[1]], c(15, 12, 3, 20))
    expect_equal(slice$name_2[[1]], c(-12, -16))

    center <- as.Date("2025-01-21")
    slice <- do_single_slice(keyed_dt_date[1:97], center, params)

    expect_true(inherits(slice, "slice_artifact"))
    expect_equal(attr(slice, "L"), params$L)
    expect_equal(attr(slice, "index"), center)
    expect_equal(names(slice), params$names)

    expect_equal(slice$name_1[[1]], c(10, 9, NA, NA))
    expect_equal(slice$name_2[[1]], c(-20, NA))

    center <- as.Date("2025-01-10")
    slice <- do_single_slice(keyed_dt_date, center, params)

    expect_true(inherits(slice, "slice_artifact"))
    expect_equal(attr(slice, "L"), params$L)
    expect_equal(attr(slice, "index"), center)
    expect_equal(names(slice), params$names)

    expect_equal(slice$name_1[[1]], c(15, 12, 3, 20))
    expect_equal(slice$name_2[[1]], c(-12, -16))
})

test_that("do_single_slice - POSIX", {

    # simple -------------------------------------------------------------

    params <- generate_simple_params("datetime")

    center <- as.POSIXct("2025-01-01 01:00:00", "GMT")
    slice <- do_single_slice(simple_dt_datetime, center, params)

    expect_true(inherits(slice, "slice_artifact"))
    expect_equal(attr(slice, "L"), params$L)
    expect_equal(attr(slice, "index"), center)
    expect_equal(names(slice), params$names)

    expect_equal(slice$name_1[[1]], c(NA, NA, NA, 20))
    expect_equal(slice$name_2[[1]], c(127, 138))

    center <- as.POSIXct("2025-01-01 10:00:00", "GMT")
    slice <- do_single_slice(simple_dt_datetime, center, params)

    expect_true(inherits(slice, "slice_artifact"))
    expect_equal(attr(slice, "L"), params$L)
    expect_equal(attr(slice, "index"), center)
    expect_equal(names(slice), params$names)

    expect_equal(slice$name_1[[1]], c(13, 1, 2, 19))
    expect_equal(slice$name_2[[1]], c(124, 129))

    center <- as.POSIXct("2025-01-01 20:00:00", "GMT")
    slice <- do_single_slice(simple_dt_datetime, center, params)

    expect_true(inherits(slice, "slice_artifact"))
    expect_equal(attr(slice, "L"), params$L)
    expect_equal(attr(slice, "index"), center)
    expect_equal(names(slice), params$names)

    expect_equal(slice$name_1[[1]], c(7, 9, 8, 14))
    expect_equal(slice$name_2[[1]], c(NA_integer_, NA_integer_))

    # keyed --------------------------------------------------------------

    params <- generate_keyed_params("datetime")

    center <- as.POSIXct("2025-01-01 01:00:00", "GMT")
    slice <- do_single_slice(keyed_dt_datetime, center, params)

    expect_true(inherits(slice, "slice_artifact"))
    expect_equal(attr(slice, "L"), params$L)
    expect_equal(attr(slice, "index"), center)
    expect_equal(names(slice), params$names)

    expect_equal(slice$name_1[[1]], c(27, 30, 34, 40))
    expect_equal(slice$name_2[[1]], c(-56, -83))

    center <- as.POSIXct("2025-01-01 10:00:00", "GMT")
    slice <- do_single_slice(keyed_dt_datetime[1:97], center, params)

    expect_true(inherits(slice, "slice_artifact"))
    expect_equal(attr(slice, "L"), params$L)
    expect_equal(attr(slice, "index"), center)
    expect_equal(names(slice), params$names)

    expect_equal(slice$name_1[[1]], c(86, 53, 49, 98))
    expect_equal(slice$name_2[[1]], c(-39, -69))

    center <- as.POSIXct("2025-01-01 20:00:00", "GMT")
    slice <- do_single_slice(keyed_dt_datetime[1:97], center, params)

    expect_true(inherits(slice, "slice_artifact"))
    expect_equal(attr(slice, "L"), params$L)
    expect_equal(attr(slice, "index"), center)
    expect_equal(names(slice), params$names)

    expect_equal(slice$name_1[[1]], c(36, 91, NA, NA))
    expect_equal(slice$name_2[[1]], c(-42, NA))
})
