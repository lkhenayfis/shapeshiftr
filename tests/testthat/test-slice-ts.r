
build_ts <- function(n, start, frequency) {
    ts(seq_len(n), start = start, frequency = frequency)
}

build_mts <- function(n, start, frequency, offsets) {
    columns <- lapply(offsets, function(offset) offset + seq_len(n))
    columns <- do.call(cbind, columns)
    colnames(columns) <- names(offsets)
    ts(columns, start = start, frequency = frequency)
}

test_that("slice", {

    test_that("dispatches to the ts and mts methods", {
        x <- build_ts(24, c(2000, 1), 12)
        expect_true(inherits(slice(x), "slice_artifact"))

        m <- build_mts(24, c(2000, 1), 12, list(A = 0, B = 1000))
        expect_true(inherits(slice(m, variables = c("A", "B")), "slice_artifact"))
    })
})

test_that("slice.ts", {

    test_that("has no spurious NA at monthly year boundaries with start > 1", {
        x <- build_ts(24, c(2000, 1), 12)
        sliced <- slice.ts(x, L = c(-1, -2, -3), start = 6)
        dt <- as.data.table(sliced)

        expect_equal(dt$value_1, 5:23)
        expect_equal(dt$value_2, 4:22)
        expect_equal(dt$value_3, 3:21)
        expect_false(anyNA(dt[, -1]))

        expect_equal(anyDuplicated(as.numeric(attr(sliced, "index"))), 0L)
    })

    test_that("lag correctness holds across frequencies and start years", {
        frequencies <- c(1, 4, 7, 12, 52)
        starts <- list(c(1, 1), c(1987, 3))

        for (freq in frequencies) {
            for (ts_start in starts) {
                x <- build_ts(freq * 4 + 10, ts_start, freq)
                sliced <- slice.ts(x, L = c(-1, -2, -3), start = 6)
                dt <- as.data.table(sliced)
                n_out <- nrow(dt)

                expect_false(anyNA(dt$value_1))
                expect_false(anyNA(dt$value_2))
                expect_false(anyNA(dt$value_3))

                expected_value_1 <- 6 + seq_len(n_out) - 2
                expect_equal(dt$value_1, expected_value_1)
                expect_equal(dt$value_2, expected_value_1 - 1)
                expect_equal(dt$value_3, expected_value_1 - 2)

                expect_equal(dt$value_2[-1], dt$value_1[-n_out])
                expect_equal(dt$value_3[-1], dt$value_2[-n_out])

                encoded_index <- attr(sliced, "index")
                expect_equal(anyDuplicated(as.numeric(encoded_index)), 0L)
                expect_true(all(diff(decode_int_time(encoded_index)) > 0))
            }
        }
    })

    test_that("lead correctness has NA only at the tail", {
        x <- build_ts(24, c(2000, 1), 12)
        sliced <- slice.ts(x, L = c(1, 2), start = 1)
        dt <- as.data.table(sliced)
        n_out <- nrow(dt)

        expect_equal(dt$value_1, c(2:24, NA_integer_))
        expect_equal(dt$value_2, c(3:24, NA_integer_, NA_integer_))
        expect_equal(sum(is.na(dt$value_1)), 1L)
        expect_equal(sum(is.na(dt$value_2)), 2L)
        expect_false(anyNA(dt$value_1[seq_len(n_out - 1)]))
        expect_false(anyNA(dt$value_2[seq_len(n_out - 2)]))
    })

    test_that("preserves the int_time index and round-trips through decode_int_time", {
        x <- build_ts(36, c(1987, 3), 4)
        sliced <- slice.ts(x, L = -1, start = 6)
        idx <- attr(sliced, "index")

        expect_true(inherits(idx, "int_time"))
        expect_true(all(idx == round(idx)))
        expect_equal(decode_int_time(idx), as.numeric(time(x))[seq(6, 36)], tolerance = 1e-6)
    })

    test_that("converts to a data.table with the expected columns and rows", {
        x <- build_ts(24, c(2000, 1), 12)
        sliced <- slice.ts(x, L = c(-1, -2, -3), start = 6)
        dt <- as.data.table(sliced)

        expect_equal(colnames(dt), c("index", "value_1", "value_2", "value_3"))
        expect_equal(nrow(dt), length(attr(sliced, "index")))
        expect_equal(nrow(dt), 19L)
    })

    test_that("default arguments produce a lag-1 feature with NA on the first row", {
        x <- build_ts(24, c(2000, 1), 12)
        dt <- as.data.table(slice(x))

        expect_true(is.na(dt$value_1[1]))
        expect_equal(dt$value_1[-1], 1:23)
        expect_equal(nrow(dt), 24L)
    })
})

test_that("slice.mts", {

    test_that("slices multiple variables independently", {
        m <- build_mts(24, c(2000, 1), 12, list(A = 0, B = 1000))
        sliced <- slice.mts(m, variables = c("A", "B"), L = c(-1, -2), start = 6)
        dt <- as.data.table(sliced)

        expect_equal(colnames(dt), c("index", "A_1", "A_2", "B_1", "B_2"))
        expect_equal(dt$A_1, 5:23)
        expect_equal(dt$A_2, 4:22)
        expect_equal(dt$B_1, 1005:1023)
        expect_equal(dt$B_2, 1004:1022)
    })

    test_that("supports variable subsetting and custom output names", {
        m <- build_mts(24, c(2000, 1), 12, list(A = 0, B = 1000))
        sliced <- slice.mts(m, variables = "B", L = c(-1, -2), start = 6, names = "lagB")
        dt <- as.data.table(sliced)

        expect_equal(colnames(dt), c("index", "lagB_1", "lagB_2"))
        expect_equal(dt$lagB_1, 1005:1023)
        expect_equal(dt$lagB_2, 1004:1022)
        expect_false("A_1" %in% colnames(dt))
    })
})
