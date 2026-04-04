test_that("via.test_lazy sets recalibrate_spec on test_lazy", {
    lazy = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        via("boot", n = 200)

    expect_s3_class(lazy, "test_lazy")
    expect_false(is.null(lazy$recalibrate_spec))
    expect_equal(lazy$recalibrate_spec$method_name, "boot")
    expect_equal(lazy$recalibrate_spec$engine, "default")
    expect_equal(lazy$recalibrate_spec$args$n, 200)
})

test_that("via.test_lazy merges defaults with user args", {
    # ttest_def_boot default n = 1000L, seed = NULL
    lazy = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        via("boot")  # no overrides

    expect_equal(lazy$recalibrate_spec$args$n, 1000L)
    expect_null(lazy$recalibrate_spec$args$seed)
})

test_that("via.test_lazy errors on unknown method", {
    lazy = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST)

    expect_error(via(lazy, "nonexistent_method"))
})

test_that("via.engine_set removes engine_set class and sets recalibrate_spec", {
    lazy = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        through("default") |>  # → engine_set
        via("permute", n = 100)

    # After via.engine_set, "engine_set" is removed
    expect_false("engine_set" %in% class(lazy))
    expect_s3_class(lazy, "test_lazy")
    expect_equal(lazy$recalibrate_spec$method_name, "permute")
    expect_equal(lazy$recalibrate_spec$args$n, 100)
})

test_that("via.test_lazy uses engine from through() when set", {
    lazy = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        through("default") |>
        via("permute")

    expect_equal(lazy$recalibrate_spec$engine, "default")
})
