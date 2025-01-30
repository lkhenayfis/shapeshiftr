
test_that("extract_lagleads", {

    # Date - simple

    center <- as.Date("2025-01-10")
    L <- list(-7:-2, -2:1, 0:4)
    ll <- extract_lagleads(
        simple_dt_date,
        center,
        "date",
        c("X1", "X3", "Y"),
        L
    )

    X1_ref <- list(simple_dt_date[match(center + L[[1]], date), X1])
    expect_equal(ll$X1, X1_ref)

    X3_ref <- list(simple_dt_date[match(center + L[[2]], date), X3])
    expect_equal(ll$X3, X3_ref)

    Y_ref <- list(simple_dt_date[match(center + L[[3]], date), Y])
    expect_equal(ll$Y, Y_ref)

    # Date - keyed

    center <- as.Date("2025-01-10")
    L <- list(0:2, 1:4)
    ll <- extract_lagleads(
        keyed_dt_date[date == center],
        center,
        "target_date",
        c("X1", "X2"),
        L
    )

    dt <- keyed_dt_date[(date == center)]
    X1_ref <- list(dt[match(center + L[[1]], target_date), X1])
    expect_equal(ll$X1, X1_ref)

    X2_ref <- list(dt[match(center + L[[2]], target_date), X2])
    expect_equal(ll$X2, X2_ref)
})

test_that("do_single_slice", {

    # Date - simple

    center <- as.Date("2025-01-10")
    L <- list(-7:-2, -2:1, 0:4)
    sa <- do_single_slice(
        simple_dt_date,
        center,
        "date",
        c("X1", "X3", "Y"),
        L,
        c("X1", "X3", "Y")
    )

    expect_equal(class(sa), c("slice_artifact", "list"))
    expect_equal(attr(sa, "index"), center)
    expect_equal(attr(sa, "L"), L)

    X1_ref <- list(simple_dt_date[match(center + L[[1]], date), X1])
    expect_equal(sa$X1, X1_ref)

    X3_ref <- list(simple_dt_date[match(center + L[[2]], date), X3])
    expect_equal(sa$X3, X3_ref)

    Y_ref <- list(simple_dt_date[match(center + L[[3]], date), Y])
    expect_equal(sa$Y, Y_ref)

    # Date - keyed

    center <- as.Date("2025-01-10")
    L <- list(0:2, 1:4)
    sa <- do_single_slice(
        keyed_dt_date[date == center],
        center,
        "target_date",
        c("X1", "X2"),
        L,
        c("X1", "X2")
    )

    expect_equal(class(sa), c("slice_artifact", "list"))
    expect_equal(attr(sa, "index"), center)
    expect_equal(attr(sa, "L"), L)

    dt <- keyed_dt_date[(date == center)]

    X1_ref <- list(dt[match(center + L[[1]], target_date), X1])
    expect_equal(sa$X1, X1_ref)

    X2_ref <- list(dt[match(center + L[[2]], target_date), X2])
    expect_equal(sa$X2, X2_ref)
})

test_that("check_index_column", {

    test_fun <- function(data, walk_on) {
        check_index_column(data, walk_on)
        return(TRUE)
    }

    expect_true(test_fun(simple_dt_date, "X1"))
    expect_true(test_fun(simple_dt_date, "X2"))
    expect_true(test_fun(simple_dt_date, "X3"))
    expect_true(test_fun(simple_dt_date, "Y"))

    expect_error(test_fun(simple_dt_date, "error"))
})

test_that("parse_variables", {

    vars <- parse_variables(simple_dt_date, "date", NULL)
    expect_equal(vars, colnames(simple_dt_date)[-1])

    vars <- parse_variables(keyed_dt_date, "date", "target_date")
    expect_equal(vars, colnames(keyed_dt_date)[-(1:2)])

    vars <- parse_variables(simple_dt_date, "date", NULL, c("X1", "X2"))
    expect_equal(vars, c("X1", "X2"))

    expect_error(parse_variables(simple_dt_date, NULL, NULL, "error"))
})

test_that("guess_sample_freq", {
    sf <- guess_sample_freq(simple_dt_date, "date")
    expect_equal(sf, structure(1, unit = "days"))

    sf <- guess_sample_freq(simple_dt_datetime, "datetime")
    expect_equal(sf, structure(3600, unit = "secs"))

    sf <- guess_sample_freq(keyed_dt_datetime, "target_datetime")
    expect_equal(sf, structure(1800, unit = "secs"))
})

test_that("parse_laglead_times", {

    L_list <- list(seq(-5, -2), seq(1, 3), 4)
    L <- parse_laglead_times(simple_dt_date, "date", L_list, letters[1:3])
    expect_equal(L, L_list)

    L <- parse_laglead_times(simple_dt_datetime, "datetime", L_list, letters[1:3])
    expect_equal(L, lapply(L_list, "*", 3600))

    expect_error(parse_laglead_times(simple_dt_date, "date", L_list, letters[1:2]))

    L_int <- 2
    L <- parse_laglead_times(simple_dt_datetime, "datetime", L_int, letters[1:3])
    expect_equal(L, lapply(seq_len(3), function(i) L_int * 3600))
})

test_that("parse_start_time", {

    start_int <- 2

    # Date

    ref_date  <- simple_dt_date$date[2]
    start_date <- as.Date("2025-01-03")
    start_date_string <- "2025-01-03"

    start <- parse_start_time(simple_dt_date, "date", start_date)
    expect_equal(start, ref_date)

    start <- parse_start_time(simple_dt_date, "date", start_date_string)
    expect_equal(start, ref_date)

    start <- parse_start_time(simple_dt_date, "date", start_int)
    expect_equal(start, ref_date)

    # POSIXct

    ref_datetime   <- simple_dt_datetime$datetime[2]
    start_datetime <- as.POSIXct("2025-01-01 02:00:00", "GMT")
    start_datetime_string <- "2025-01-01 02:00:00"

    start <- parse_start_time(simple_dt_datetime, "datetime", start_datetime)
    expect_equal(start, ref_datetime)

    start <- parse_start_time(simple_dt_datetime, "datetime", start_datetime_string)
    expect_equal(start, ref_datetime)
    expect_equal(attr(start, "tzone"), attr(ref_datetime, "tzone"))
    expect_equal(attr(start, "tzone"), attr(simple_dt_datetime$datetime[1], "tzone"))

    start <- parse_start_time(simple_dt_datetime, "datetime", start_int)
    expect_equal(start, ref_datetime)

    start <- parse_start_time(keyed_dt_datetime, "datetime", start_int)
    expect_equal(start, ref_datetime)
})

test_that("parse_step_time", {

    step_int <- 2

    # Date

    step <- parse_step_time(simple_dt_date, "date", step_int)
    expect_equal(step, step_int)

    step_string <- "2 days"
    step <- parse_step_time(simple_dt_date, "date", step_string)
    expect_equal(step, step_int)

    # POSIXct

    step <- parse_step_time(simple_dt_datetime, "datetime", step_int)
    expect_equal(step, step_int * 3600)

    step <- parse_step_time(keyed_dt_datetime, "target_datetime", step_int)
    expect_equal(step, step_int * 1800)

    step_string <- "1 hour"
    step <- parse_step_time(simple_dt_datetime, "datetime", step_string)
    expect_equal(step, 3600)

    step_string <- "3 hours"
    step <- parse_step_time(simple_dt_datetime, "datetime", step_string)
    expect_equal(step, 3 * 3600)

    step_string <- "30 mins"
    step <- parse_step_time(simple_dt_datetime, "datetime", step_string)
    expect_equal(step, 1800)

    step_string <- "30 mins"
    step <- parse_step_time(keyed_dt_datetime, "target_datetime", step_string)
    expect_equal(step, 1800)
})

test_that("parse_slice_times", {

    # Date

    ref_date <- seq(as.Date("2025-01-03"), as.Date("2025-01-21"), by = "1 day")

    slice_times <- parse_slice_times(simple_dt_date, "date", 2, 1)
    expect_equal(slice_times, ref_date)

    slice_times <- parse_slice_times(simple_dt_date, "date", "2025-01-3", 1)
    expect_equal(slice_times, ref_date)

    slice_times <- parse_slice_times(simple_dt_date, "date", as.Date("2025-01-03"), 1)
    expect_equal(slice_times, ref_date)

    slice_times <- parse_slice_times(simple_dt_date, "date", 2, "1 day")
    expect_equal(slice_times, ref_date)

    ref_date <- seq(as.Date("2025-01-03"), as.Date("2025-01-21"), by = "2 days")

    slice_times <- parse_slice_times(simple_dt_date, "date", 2, 2)
    expect_equal(slice_times, ref_date)

    slice_times <- parse_slice_times(simple_dt_date, "date", "2025-01-3", "2 days")
    expect_equal(slice_times, ref_date)

    # POSIXct

    ref_datetime <- seq(as.POSIXct("2025-01-01 01:00:00", "GMT"), as.POSIXct("2025-01-01 20:00:00", "GMT"),
        by = "1 hour")

    slice_times <- parse_slice_times(simple_dt_datetime, "datetime", 1, 1)
    expect_equal(slice_times, ref_datetime)

    slice_times <- parse_slice_times(simple_dt_datetime, "datetime", "2025-01-01 01:00:00", 1)
    expect_equal(slice_times, ref_datetime)

    slice_times <- parse_slice_times(simple_dt_datetime, "datetime", as.POSIXct("2025-01-01 01:00:00", "GMT"), 1)
    expect_equal(slice_times, ref_datetime)

    slice_times <- parse_slice_times(simple_dt_datetime, "datetime", 1, "1 hour")
    expect_equal(slice_times, ref_datetime)

    ref_datetime <- seq(as.POSIXct("2025-01-01 01:00:00", "GMT"), as.POSIXct("2025-01-01 20:00:00", "GMT"),
        by = "2 hours")

    slice_times <- parse_slice_times(simple_dt_datetime, "datetime", 1, 2)
    expect_equal(slice_times, ref_datetime)

    slice_times <- parse_slice_times(simple_dt_datetime, "datetime", "2025-01-01 01:00:00", "2 hours")
    expect_equal(slice_times, ref_datetime)

})