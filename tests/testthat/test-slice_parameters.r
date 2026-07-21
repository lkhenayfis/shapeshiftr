
test_that("new_slice_parameters", {
    params <- parse_slice_args(
        simple_dt_date, c("X1", "X2"), "date", "date", 1, 1, 1, c("X1", "X2"))
    expect_true(inherits(params, "simple_slice_params"))

    params <- parse_slice_args(
        keyed_dt_date, c("X1", "X2"), "date", "target_date", 1, 1, 1, c("X1", "X2"))
    expect_true(inherits(params, "keyed_slice_params"))
})

test_that("parse_index_column", {

    test_fun <- function(data, walk_on) {
        parse_index_column(data, walk_on)
        return(TRUE)
    }

    expect_true(test_fun(simple_dt_date, "X1"))
    expect_true(test_fun(simple_dt_date, "X2"))
    expect_true(test_fun(simple_dt_date, "X3"))
    expect_true(test_fun(simple_dt_date, "Y"))

    expect_error(test_fun(simple_dt_date, "error"))
})

test_that("parse_variables", {

    vars <- parse_variables(simple_dt_date, c("date", NULL), NULL)
    expect_equal(vars, colnames(simple_dt_date)[-1])

    vars <- parse_variables(keyed_dt_date, c("date", "target_date"), NULL)
    expect_equal(vars, colnames(keyed_dt_date)[-(1:2)])

    vars <- parse_variables(simple_dt_date, c("date", NULL), c("X1", "X2"))
    expect_equal(vars, c("X1", "X2"))

    expect_error(parse_variables(simple_dt_date, c(NULL, NULL), "error"))
})

test_that("parse_names", {

    test_that("returns explicit names unchanged when length matches variables", {
        names <- parse_names(c("a", "b"), c("X1", "X2"))
        expect_equal(names, c("a", "b"))

        names <- parse_names("only_one", "X1")
        expect_equal(names, "only_one")
    })

    test_that("errors when explicit names length does not match variables", {
        expect_error(
            parse_names(c("a", "b", "c"), c("X1", "X2")),
            "'names' must have same length as 'variables'"
        )
        expect_error(parse_names("a", c("X1", "X2")))
    })

    test_that("NULL names on unique variables returns the variables unchanged", {
        names <- parse_names(NULL, c("X1", "X2", "Y"))
        expect_equal(names, c("X1", "X2", "Y"))
    })

    test_that("NULL names on duplicated variables suffixes the dupes in original order", {
        names <- parse_names(NULL, c("X1", "X1", "Y"))
        expect_equal(names, c("X1_1", "X1_2", "Y"))

        names <- parse_names(NULL, c("Y", "X1", "X1"))
        expect_equal(names, c("Y", "X1_1", "X1_2"))

        names <- parse_names(NULL, c("X1", "Y", "X1", "Y"))
        expect_equal(names, c("X1_1", "Y_1", "X1_2", "Y_2"))
    })
})

test_that("guess_sample_freq", {
    # Daily data
    sf <- guess_sample_freq(simple_dt_date, "date")
    expect_equal(sf, structure(1, unit = "days"))

    # Hourly data
    sf <- guess_sample_freq(simple_dt_datetime, "datetime")
    expect_equal(sf, structure(3600, unit = "secs"))

    # Half-hourly data
    sf <- guess_sample_freq(keyed_dt_datetime, "target_datetime")
    expect_equal(sf, structure(1800, unit = "secs"))

    # Monthly data
    monthly_data <- data.frame(
        date = seq(as.Date("2020-01-01"), as.Date("2020-12-01"), by = "1 month")
    )
    sf <- guess_sample_freq(monthly_data, "date")
    expect_equal(sf, structure(1, unit = "months"))

    # Monthly data with varying lengths (28-31 days)
    monthly_data_2 <- data.frame(
        date = as.Date(c("2020-01-15", "2020-02-15", "2020-03-15", "2020-04-15"))
    )
    sf <- guess_sample_freq(monthly_data_2, "date")
    expect_equal(sf, structure(1, unit = "months"))

    # Yearly data
    yearly_data <- data.frame(
        date = seq(as.Date("2015-06-01"), as.Date("2020-06-01"), by = "1 year")
    )
    sf <- guess_sample_freq(yearly_data, "date")
    expect_equal(sf, structure(1, unit = "years"))

    # Yearly data including leap year
    yearly_data_leap <- data.frame(
        date = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01"))
    )
    sf <- guess_sample_freq(yearly_data_leap, "date")
    expect_equal(sf, structure(1, unit = "years"))
})

test_that("parse_laglead_times", {

    vars <- letters[1:3]

    L_list_nameless <- list(seq(-5, -2), seq(1, 3), 4)
    L <- parse_laglead_times(simple_dt_date, "date", L_list_nameless, vars)
    expect_equal(names(L), vars)
    expect_equal(unname(L), L_list_nameless)

    L_list <- list("b" = seq(1, 3), "a" = seq(-5, -2), "c" = 4)
    L <- parse_laglead_times(simple_dt_date, "date", L_list, vars)
    expect_equal(names(L), vars)
    expect_equal(unname(L), L_list_nameless)

    L <- parse_laglead_times(simple_dt_datetime, "datetime", L_list, vars)
    expect_equal(unname(L), lapply(L_list_nameless, "*", 3600))

    # lista L de tamanho diferente de variables
    expect_error(parse_laglead_times(simple_dt_date, "date", L_list, vars[1:2]))

    # L como inteiro sendo expandido para lista

    L_int <- 2
    L <- parse_laglead_times(simple_dt_datetime, "datetime", L_int, vars)
    expect_equal(L, list(a = 7200, b = 7200, c = 7200))
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
