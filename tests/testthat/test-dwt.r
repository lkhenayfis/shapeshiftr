
test_that("dwt.slice_artifact", {

    slices <- slice(simple_dt_date, "X1", "date", L = -3:0)
    slices <- na.exclude(slices)

    slices_dwt <- dwt(slices, "X1", filter = "haar")
    expect_true(inherits(slices_dwt, "slice_artifact"))
    expect_equal(attr(slices, "index"), attr(slices_dwt, "index"))
    expect_equal(dim(slices), dim(slices_dwt))
    expect_equal(names(slices), names(slices_dwt))

    outer <- dwt(as.numeric(slices$X1[[1]]), filter = "haar")
    outer <- c(unlist(outer@W), unlist(outer@V))
    expect_equal(unname(slices_dwt$X1[[1]]), unname(outer))

    outer <- dwt(as.numeric(slices$X1[[10]]), filter = "haar")
    outer <- c(unlist(outer@W), unlist(outer@V))
    expect_equal(unname(slices_dwt$X1[[10]]), unname(outer))
})
