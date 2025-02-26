
if (Sys.info()["sysname"] == "Linux") {

    test_that("set_up_cluster", {
        cl <- get_cluster()
        run_post_hook(cl)

        unlockBinding(".SHAPESHIFTR_CLUSTER", asNamespace("shapeshiftr"))

        set_up_cluster(2)
        cl <- get_cluster()
        expect_true(inherits(cl, "cluster"))
        run_post_hook(cl)

        set_up_cluster(0)
        cl <- get_cluster()
        expect_equal(length(cl), 0)
        expect_true(is.list(cl))
        run_post_hook(cl)
    })

    test_that("refresh_cluster", {
        cl <- get_cluster()
        run_post_hook(cl)

        unlockBinding(".SHAPESHIFTR_CLUSTER", asNamespace("shapeshiftr"))

        ee <- new.env()
        assign("vv", seq_len(10), envir = ee)

        set_up_cluster(2)
        cl <- get_cluster()

        refresh_cluster(cl, envir = ee)
        ss <- parallel::parSapply(cl, seq_len(2), function(i) max(vv / i))
        expect_equal(ss, c(10, 5))
        run_post_hook(cl)

        set_up_cluster(0)
        cl <- get_cluster()

        rr <- refresh_cluster(cl, envir = ee)
        expect_true(is.null(rr))
    })

    test_that("run_loop", {
        cl <- get_cluster()
        run_post_hook(cl)

        unlockBinding(".SHAPESHIFTR_CLUSTER", asNamespace("shapeshiftr"))

        set_up_cluster(2)
        run1 <- run_loop(seq_len(10), function(i) i + 1)
        cl <- get_cluster()
        run_post_hook(cl)

        set_up_cluster(0)
        run2 <- run_loop(seq_len(10), function(i) i + 1)
        cl <- get_cluster()
        run_post_hook(cl)

        expect_equal(run1, run2)
    })
}