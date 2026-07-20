
build_ts <- function(n, start, frequency) {
    ts(seq_len(n), start = start, frequency = frequency)
}

build_mts <- function(n, start, frequency, offsets) {
    columns <- lapply(offsets, function(offset) offset + seq_len(n))
    columns <- do.call(cbind, columns)
    colnames(columns) <- names(offsets)
    ts(columns, start = start, frequency = frequency)
}

check_no_spurious_na_lags <- function(frequency, ts_start, row_start = 6, n_periods = 4) {
    n <- frequency * n_periods + 10
    x <- build_ts(n, ts_start, frequency)
    sliced <- slice.ts(x, L = c(-1, -2, -3), start = row_start)
    dt <- as.data.table(sliced)

    expect_false(anyNA(dt$value_1))
    expect_false(anyNA(dt$value_2))
    expect_false(anyNA(dt$value_3))

    expect_equal(dt$value_2[-1], dt$value_1[-nrow(dt)])
    expect_equal(dt$value_3[-1], dt$value_2[-nrow(dt)])

    n_out <- nrow(dt)
    expected_value_1 <- row_start + seq_len(n_out) - 2
    expected_value_2 <- expected_value_1 - 1
    expected_value_3 <- expected_value_2 - 1

    expect_equal(dt$value_1, expected_value_1)
    expect_equal(dt$value_2, expected_value_2)
    expect_equal(dt$value_3, expected_value_3)

    encoded_index <- as.numeric(attr(sliced, "index"))
    expect_equal(anyDuplicated(encoded_index), 0L)
    expect_true(all(diff(decode_int_time(attr(sliced, "index"))) > 0))

    invisible(TRUE)
}

test_that("slice generic dispatches to slice.ts and slice.mts", {
    x <- build_ts(24, c(2000, 1), 12)
    sliced_ts <- slice(x)
    expect_true(inherits(sliced_ts, "slice_artifact"))

    m <- build_mts(24, c(2000, 1), 12, list(A = 0, B = 1000))
    sliced_mts <- slice(m, variables = c("A", "B"))
    expect_true(inherits(sliced_mts, "slice_artifact"))
})

test_that("slice.ts has no spurious NA at monthly year boundaries with start > 1", {
    x <- build_ts(24, c(2000, 1), 12)
    sliced <- slice.ts(x, L = c(-1, -2, -3), start = 6)
    dt <- as.data.table(sliced)

    expect_equal(dt$value_1, 5:23)
    expect_equal(dt$value_2, 4:22)
    expect_equal(dt$value_3, 3:21)
    expect_false(anyNA(dt[, -1]))

    encoded_index <- as.numeric(attr(sliced, "index"))
    expect_equal(anyDuplicated(encoded_index), 0L)
})

ts_starts_grid <- list(c(1, 1), c(1987, 3))

test_that("slice.ts lag correctness holds across frequencies and start years - frequency 1", {
    Map(check_no_spurious_na_lags, 1, ts_starts_grid)
})

test_that("slice.ts lag correctness holds across frequencies and start years - frequency 4", {
    Map(check_no_spurious_na_lags, 4, ts_starts_grid)
})

test_that("slice.ts lag correctness holds across frequencies and start years - frequency 7", {
    Map(check_no_spurious_na_lags, 7, ts_starts_grid)
})

test_that("slice.ts lag correctness holds across frequencies and start years - frequency 12", {
    Map(check_no_spurious_na_lags, 12, ts_starts_grid)
})

test_that("slice.ts lag correctness holds across frequencies and start years - frequency 52", {
    Map(check_no_spurious_na_lags, 52, ts_starts_grid)
})

test_that("slice.ts lead correctness has NA only at the tail", {
    x <- build_ts(24, c(2000, 1), 12)
    sliced <- slice.ts(x, L = c(1, 2), start = 1)
    dt <- as.data.table(sliced)

    n_out <- nrow(dt)
    expected_value_1 <- c(2:24, NA_integer_)
    expected_value_2 <- c(3:24, NA_integer_, NA_integer_)

    expect_equal(dt$value_1, expected_value_1)
    expect_equal(dt$value_2, expected_value_2)

    expect_equal(sum(is.na(dt$value_1)), 1L)
    expect_equal(sum(is.na(dt$value_2)), 2L)
    expect_false(anyNA(dt$value_1[seq_len(n_out - 1)]))
    expect_false(anyNA(dt$value_2[seq_len(n_out - 2)]))
})

test_that("slice.ts preserves int_time index attribute and round-trips through decode_int_time", {
    x <- build_ts(36, c(1987, 3), 4)
    sliced <- slice.ts(x, L = -1, start = 6)

    idx <- attr(sliced, "index")

    expect_true(inherits(idx, "int_time"))
    expect_true(all(idx == round(idx)))

    decoded <- decode_int_time(idx)
    original_times <- as.numeric(time(x))[seq(6, 36)]

    expect_equal(decoded, original_times, tolerance = 1e-6)
})

test_that("slice.mts slices multiple variables independently", {
    m <- build_mts(24, c(2000, 1), 12, list(A = 0, B = 1000))
    sliced <- slice.mts(m, variables = c("A", "B"), L = c(-1, -2), start = 6)
    dt <- as.data.table(sliced)

    expect_equal(colnames(dt), c("index", "A_1", "A_2", "B_1", "B_2"))

    expect_equal(dt$A_1, 5:23)
    expect_equal(dt$A_2, 4:22)
    expect_equal(dt$B_1, 1005:1023)
    expect_equal(dt$B_2, 1004:1022)
})

test_that("slice.mts supports variable subsetting and custom output names", {
    m <- build_mts(24, c(2000, 1), 12, list(A = 0, B = 1000))
    sliced <- slice.mts(m, variables = "B", L = c(-1, -2), start = 6, names = "lagB")
    dt <- as.data.table(sliced)

    expect_equal(colnames(dt), c("index", "lagB_1", "lagB_2"))
    expect_equal(dt$lagB_1, 1005:1023)
    expect_equal(dt$lagB_2, 1004:1022)
    expect_false("A_1" %in% colnames(dt))
})

test_that("as.data.table conversion of a ts slice has expected columns and row count", {
    x <- build_ts(24, c(2000, 1), 12)
    sliced <- slice.ts(x, L = c(-1, -2, -3), start = 6)
    dt <- as.data.table(sliced)

    expect_equal(colnames(dt), c("index", "value_1", "value_2", "value_3"))
    expect_equal(nrow(dt), length(attr(sliced, "index")))
    expect_equal(nrow(dt), 19L)
})

test_that("slice.ts default arguments produce a lag-1 feature with NA on the first row", {
    x <- build_ts(24, c(2000, 1), 12)
    sliced <- slice(x)
    dt <- as.data.table(sliced)

    expect_true(is.na(dt$value_1[1]))
    expect_equal(dt$value_1[-1], 1:23)
    expect_equal(nrow(dt), 24L)
})
