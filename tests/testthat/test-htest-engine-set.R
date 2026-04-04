test_that("through.test_lazy adds engine_set class", {
    lazy = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        through("default")

    expect_true("engine_set" %in% class(lazy))
    expect_s3_class(lazy, "test_lazy")
})

test_that("through.test_lazy sets engine field", {
    lazy = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        through("myengine")

    expect_equal(lazy$engine, "myengine")
})

test_that("through.test_lazy stores engine_args from ...", {
    lazy = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        through("default", threads = 4L)

    expect_equal(lazy$engine_args$threads, 4L)
})

test_that("through() followed by conclude() works (classical path)", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        through("default") |>
        conclude()

    expect_s3_class(result, "htest_spec")
    expect_s3_class(result, "ttest_two")
})
