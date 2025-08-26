
generate_simple_slice <- function(type = c("date", "datetime")) {
    type <- match.arg(type)
    if (type == "date") {
        out <- slice(simple_dt_date, c("X1", "Y"), "date", L = list(X1 = -4:0, Y = 1:3))
    } else {
        out <- slice(simple_dt_datetime, c("X1", "Y"), "datetime", L = list(X1 = -4:0, Y = 1:3))
    }

    return(out)
}

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
    simple_date <- generate_simple_slice()

    expect_equal(names(simple_date), c("X1", "Y"))

    novos <- letters[1:2]
    names(simple_date) <- novos

    expect_equal(names(simple_date), novos)
    expect_equal(names(attr(simple_date, "L")), novos)
})

test_that("dim.slice_artifact", {
    simple_date <- generate_simple_slice("date")
    expect_equal(dim(simple_date), c(2, 20))
})

test_that("c.slice_artifact", {

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

test_that("`[.slice_artifact`", {

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

test_that("merge.slice_artifact", {

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

test_that("na.exclude.slice_artifact", {

    simple_date <- generate_simple_slice()["X1"]

    no_na <- na.exclude(simple_date)
    expect_true(inherits(no_na, "slice_artifact"))
    expect_equal(dim(no_na), c(1, 16))
})

test_that("combine_features", {

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

test_that("as.data.table.slice_artifact", {

    simple_date <- generate_simple_slice()["X1"]

    dt <- as.data.table(simple_date)
    expect_equal(dt$index, attr(simple_date, "index"))

    for (i in seq_len(5)) {
        vec <- sapply(seq_len(20), function(j) simple_date$X1[[j]][i])
        expect_equal(dt[[i + 1]], vec)
    }
})
