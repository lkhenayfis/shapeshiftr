
build_simple_mts <- function(variables, n = 20, start = c(2000, 9), frequency = 12) {
    offsets <- seq_along(variables) * 100
    values <- vapply(offsets, function(offset) offset + seq_len(n), numeric(n))
    colnames(values) <- variables
    ts(values, start = start, frequency = frequency)
}

rep_index <- function(index, times) Reduce(c, rep(list(index), times))

generate_simple_slice <- function(type = c("date", "datetime", "ts")) {
    type <- match.arg(type)
    mc <- match.call()
    fun <- as.name(paste0("generate_simple_slice_", type))
    mc[[1]] <- fun
    eval(mc, parent.frame())
}

generate_simple_slice_date <- function(...) {
    slice(simple_dt_date, c("X1", "Y"), "date", L = list(X1 = -4:0, Y = 1:3))
}

generate_simple_slice_datetime <- function(...) {
    slice(simple_dt_datetime, c("X1", "Y"), "datetime", L = list(X1 = -4:0, Y = 1:3))
}

generate_simple_slice_ts <- function(...) {
    slice(build_simple_mts(c("X1", "Y")), c("X1", "Y"), L = list(X1 = -4:0, Y = 1:3))
}

# there is no time-series equivalent of keyed slicing -- a ts/mts carries a single temporal index,
# so `generate_keyed_slice` stays data.table-only and remains unused, exactly as before

generate_keyed_slice <- function(type = c("date", "datetime")) {
    type <- match.arg(type)
    if (type == "date") {
        out <- slice(keyed_dt_date, "date", "target_date",
            variables = c("X1"), L = list(X1 = 1:3), names = "X1_prev")
    } else {
        out <- slice(keyed_dt_datetime, "datetime", "target_datetime",
            variables = c("X1"), L = list(X1 = 1:3), names = "X1_prev")
    }

    return(out)
}

test_that("names.slice_artifact", {
    test_that("slicing data.tables", {
        simple_date <- generate_simple_slice()

        expect_equal(names(simple_date), c("X1", "Y"))

        novos <- letters[1:2]
        names(simple_date) <- novos

        expect_equal(names(simple_date), novos)
        expect_equal(names(attr(simple_date, "L")), novos)
    })

    test_that("slicing time series", {
        simple_ts <- generate_simple_slice("ts")

        expect_equal(names(simple_ts), c("X1", "Y"))

        novos <- letters[1:2]
        names(simple_ts) <- novos

        expect_equal(names(simple_ts), novos)
        expect_equal(names(attr(simple_ts, "L")), novos)
    })
})

test_that("dim.slice_artifact", {
    test_that("slicing data.tables", {
        simple_date <- generate_simple_slice("date")
        expect_equal(dim(simple_date), c(2, 20))
    })

    test_that("slicing time series", {
        simple_ts <- generate_simple_slice("ts")
        expect_equal(dim(simple_ts), c(2, 20))
    })
})

test_that("c.slice_artifact", {
    test_that("slicing data.tables", {

        simple_date <- generate_simple_slice()

        concated <- c(simple_date, simple_date)
        expect_true(inherits(concated, "slice_artifact"))
        expect_equal(attr(concated, "index"), rep(attr(simple_date, "index"), 2))
        expect_equal(concated$X1, rep(simple_date$X1, 2))
        expect_equal(concated$Y, rep(simple_date$Y, 2))
        expect_equal(attr(concated, "L"), attr(simple_date, "L"))

        N <- 10
        list <- lapply(seq_len(N), function(i) simple_date)

        concated <- do.call(c, list)
        expect_true(inherits(concated, "slice_artifact"))
        expect_equal(attr(concated, "index"), rep(attr(simple_date, "index"), N))
        expect_equal(concated$X1, rep(simple_date$X1, N))
        expect_equal(concated$Y, rep(simple_date$Y, N))
        expect_equal(attr(concated, "L"), attr(simple_date, "L"))
    })

    test_that("slicing time series", {

        simple_ts <- generate_simple_slice("ts")

        concated <- c(simple_ts, simple_ts)
        expect_true(inherits(concated, "slice_artifact"))
        expect_equal(attr(concated, "index"), rep_index(attr(simple_ts, "index"), 2))
        expect_equal(concated$X1, rep(simple_ts$X1, 2))
        expect_equal(concated$Y, rep(simple_ts$Y, 2))
        expect_equal(attr(concated, "L"), attr(simple_ts, "L"))

        N <- 10
        list <- lapply(seq_len(N), function(i) simple_ts)

        concated <- do.call(c, list)
        expect_true(inherits(concated, "slice_artifact"))
        expect_equal(attr(concated, "index"), rep_index(attr(simple_ts, "index"), N))
        expect_equal(concated$X1, rep(simple_ts$X1, N))
        expect_equal(concated$Y, rep(simple_ts$Y, N))
        expect_equal(attr(concated, "L"), attr(simple_ts, "L"))
    })
})

test_that("`[.slice_artifact`", {
    test_that("slicing data.tables", {

        simple_date <- generate_simple_slice()

        sub_var <- simple_date["X1"]
        expect_true(inherits(sub_var, "slice_artifact"))
        expect_equal(simple_date$X1, sub_var$X1)
        expect_equal(attr(simple_date, "index"), attr(sub_var, "index"))
        expect_equal(attr(simple_date, "L")[c("X1")], attr(sub_var, "L"))

        indexes <- c("2025-01-02", "2025-01-03", "2025-01-05")
        integer_indexes <- match(indexes, attr(simple_date, "index"))

        sub_index <- simple_date[, indexes]
        expect_true(inherits(sub_index, "slice_artifact"))
        expect_equal(simple_date$X1[integer_indexes], sub_index$X1)
        expect_equal(simple_date$Y[integer_indexes], sub_index$Y)
        expect_equal(attr(simple_date, "index")[integer_indexes], attr(sub_index, "index"))
        expect_equal(attr(simple_date, "L"), attr(sub_index, "L"))

        # integer subsets

        sub_var_int <- simple_date[1]
        expect_identical(sub_var, sub_var_int)

        sub_index_int <- simple_date[, c(1, 2, 4)]
        expect_identical(sub_index, sub_index_int)
    })

    test_that("slicing time series", {

        simple_ts <- generate_simple_slice("ts")

        sub_var <- simple_ts["X1"]
        expect_true(inherits(sub_var, "slice_artifact"))
        expect_equal(simple_ts$X1, sub_var$X1)
        expect_equal(attr(simple_ts, "index"), attr(sub_var, "index"))
        expect_equal(attr(simple_ts, "L")[c("X1")], attr(sub_var, "L"))

        positions <- c(1, 2, 4)
        indexes <- attr(simple_ts, "index")[positions]
        integer_indexes <- match(indexes, attr(simple_ts, "index"))

        sub_index <- simple_ts[, indexes]
        expect_true(inherits(sub_index, "slice_artifact"))
        expect_equal(simple_ts$X1[integer_indexes], sub_index$X1)
        expect_equal(simple_ts$Y[integer_indexes], sub_index$Y)
        expect_equal(attr(simple_ts, "index")[integer_indexes], attr(sub_index, "index"))
        expect_equal(attr(simple_ts, "L"), attr(sub_index, "L"))

        # integer subsets

        sub_var_int <- simple_ts[1]
        expect_identical(sub_var, sub_var_int)

        sub_index_int <- simple_ts[, positions]
        expect_identical(sub_index, sub_index_int)
    })
})

test_that("merge.slice_artifact", {
    test_that("slicing data.tables", {

        simple_date <- generate_simple_slice()
        simple_date_x1 <- simple_date["X1"]
        simple_date_y  <- simple_date["Y"]

        merged <- merge(simple_date_x1, simple_date_y)
        expect_identical(merged, simple_date)

        simple_date_x1 <- simple_date["X1", c("2025-01-02", "2025-01-04", "2025-01-05", "2025-01-07")]
        simple_date_y  <- simple_date["Y",  c("2025-01-03", "2025-01-04", "2025-01-05", "2025-01-07")]

        merged <- merge(simple_date_x1, simple_date_y)
        expect_true(inherits(merged, "slice_artifact"))
        expect_equal(simple_date$X1[c(3, 4, 6)], merged$X1)
        expect_equal(simple_date$Y[c(3, 4, 6)], merged$Y)
        expect_equal(attr(simple_date, "index")[c(3, 4, 6)], attr(merged, "index"))
        expect_equal(attr(merged, "L"), attr(simple_date, "L"))
    })

    test_that("slicing time series", {

        simple_ts <- generate_simple_slice("ts")
        simple_ts_x1 <- simple_ts["X1"]
        simple_ts_y  <- simple_ts["Y"]

        merged <- merge(simple_ts_x1, simple_ts_y)
        expect_identical(merged, simple_ts)

        idx <- attr(simple_ts, "index")
        simple_ts_x1 <- simple_ts["X1", idx[c(1, 3, 4, 6)]]
        simple_ts_y  <- simple_ts["Y",  idx[c(2, 3, 4, 6)]]

        merged <- merge(simple_ts_x1, simple_ts_y)
        expect_true(inherits(merged, "slice_artifact"))
        expect_equal(simple_ts$X1[c(3, 4, 6)], merged$X1)
        expect_equal(simple_ts$Y[c(3, 4, 6)], merged$Y)
        expect_equal(attr(simple_ts, "index")[c(3, 4, 6)], attr(merged, "index"))
        expect_equal(attr(merged, "L"), attr(simple_ts, "L"))
    })
})

test_that("na.exclude.slice_artifact", {
    test_that("slicing data.tables", {

        simple_date <- generate_simple_slice()["X1"]

        no_na <- na.exclude(simple_date)
        expect_true(inherits(no_na, "slice_artifact"))
        expect_equal(dim(no_na), c(1, 16))
    })

    test_that("slicing time series", {

        simple_ts <- generate_simple_slice("ts")["X1"]

        no_na <- na.exclude(simple_ts)
        expect_true(inherits(no_na, "slice_artifact"))
        expect_equal(dim(no_na), c(1, 16))
    })
})

test_that("combine_features", {
    test_that("slicing data.tables", {

        simple_date <- generate_simple_slice()

        combined <- combine_features(simple_date, "X1", "Y")
        outer_comb <- mapply(c, simple_date$X1, simple_date$Y, SIMPLIFY = FALSE)
        expect_true(inherits(combined, "slice_artifact"))
        expect_equal(names(combined), "X1_c_Y")
        expect_equal(combined$X1_c_Y, outer_comb)
        expect_equal(attr(combined, "index"), attr(simple_date, "index"))
        expect_equal(attr(combined, "L"), list("X1_c_Y" = Reduce("c", attr(simple_date, "L"))))

        # return.all = TRUE

        simple_date_2 <- slice(simple_dt_date, c("X1", "Y", "X2"), "date",
            L = list(X1 = -4:0, X2 = -1:1, Y = 1:3))

        outer_comb <- mapply(c, simple_date_2$X1, simple_date_2$Y, SIMPLIFY = FALSE)
        outer_L <- c(list(X2 = -1:1), list(X1_c_Y = -4:3))

        combined <- combine_features(simple_date_2, "X1", "Y", TRUE)
        expect_true(inherits(combined, "slice_artifact"))
        expect_equal(names(combined), c("X2", "X1_c_Y"))
        expect_equal(combined$X1_c_Y, outer_comb)
        expect_equal(attr(combined, "index"), attr(simple_date_2, "index"))
        expect_equal(attr(combined, "L"), outer_L)
    })

    test_that("slicing time series", {

        simple_ts <- generate_simple_slice("ts")

        combined <- combine_features(simple_ts, "X1", "Y")
        outer_comb <- mapply(c, simple_ts$X1, simple_ts$Y, SIMPLIFY = FALSE)
        expect_true(inherits(combined, "slice_artifact"))
        expect_equal(names(combined), "X1_c_Y")
        expect_equal(combined$X1_c_Y, outer_comb)
        expect_equal(attr(combined, "index"), attr(simple_ts, "index"))
        expect_equal(attr(combined, "L"), list("X1_c_Y" = Reduce("c", attr(simple_ts, "L"))))

        # return.all = TRUE

        simple_ts_2 <- slice(build_simple_mts(c("X1", "Y", "X2")), c("X1", "Y", "X2"),
            L = list(X1 = -4:0, X2 = -1:1, Y = 1:3))

        outer_comb <- mapply(c, simple_ts_2$X1, simple_ts_2$Y, SIMPLIFY = FALSE)
        L2 <- attr(simple_ts_2, "L")
        outer_L <- c(L2["X2"], list(X1_c_Y = c(L2$X1, L2$Y)))

        combined <- combine_features(simple_ts_2, "X1", "Y", TRUE)
        expect_true(inherits(combined, "slice_artifact"))
        expect_equal(names(combined), c("X2", "X1_c_Y"))
        expect_equal(combined$X1_c_Y, outer_comb)
        expect_equal(attr(combined, "index"), attr(simple_ts_2, "index"))
        expect_equal(attr(combined, "L"), outer_L)
    })
})

test_that("as.data.table.slice_artifact", {
    test_that("slicing data.tables", {

        simple_date <- generate_simple_slice()["X1"]

        dt <- as.data.table(simple_date)
        expect_equal(dt$index, attr(simple_date, "index"))

        for (i in seq_len(5)) {
            vec <- sapply(seq_len(20), function(j) simple_date$X1[[j]][i])
            expect_equal(dt[[i + 1]], vec)
        }
    })

    test_that("slicing time series", {

        simple_ts <- generate_simple_slice("ts")["X1"]

        dt <- as.data.table(simple_ts)
        expect_equal(dt$index, attr(simple_ts, "index"))

        for (i in seq_len(5)) {
            vec <- vapply(seq_len(20), function(j) simple_ts$X1[[j]][i], numeric(1))
            expect_equal(dt[[i + 1]], vec)
        }
    })
})
