test_that("prepare_test.def_model returns test_lazy", {
    dm = sleep |> define_model(x_by(extra, group))
    result = prepare_test(dm, TTEST)
    expect_s3_class(result, "test_lazy")
})

test_that("prepare_test.def_model stores model_id, processed, test_spec", {
    dm = sleep |> define_model(x_by(extra, group))
    result = prepare_test(dm, TTEST)
    expect_s3_class(result$model_id, "x_by")
    expect_named(result$processed, c("x_data", "group_data"))
    expect_s3_class(result$test_spec, "test_spec")
    expect_null(result$recalibrate_spec)
    expect_null(result$claims)
})

test_that("prepare_test accepts a test_spec directly", {
    dm = sleep |> define_model(x_by(extra, group))
    spec = TTEST()  # returns test_spec when called with no model
    result = prepare_test(dm, spec)
    expect_s3_class(result, "test_lazy")
})

test_that("as_test_spec returns spec unchanged when already test_spec", {
    spec = TTEST()
    result = as_test_spec(spec)
    expect_identical(result, spec)
})

test_that("as_test_spec calls function and returns test_spec", {
    result = as_test_spec(TTEST)
    expect_s3_class(result, "test_spec")
})

test_that("as_test_spec errors on non-function non-test_spec", {
    expect_error(as_test_spec("not a function or spec"))
})

test_that("update.test_lazy modifies test_spec args before via", {
    dm = sleep |> define_model(x_by(extra, group))
    lazy = prepare_test(dm, TTEST)

    updated = update(lazy, .paired = TRUE)
    expect_equal(updated$test_spec$args$.paired, TRUE)
    # recalibrate_spec still NULL
    expect_null(updated$recalibrate_spec)
})

test_that("update.test_lazy merges with existing test_spec args", {
    dm = sleep |> define_model(x_by(extra, group))
    lazy = prepare_test(dm, TTEST)
    updated = update(lazy, .mu = 1, .ci = 0.99)
    expect_equal(updated$test_spec$args$.mu, 1)
    expect_equal(updated$test_spec$args$.ci, 0.99)
})

test_that("update.test_lazy modifies recalibrate_spec args when present", {
    dm = sleep |> define_model(x_by(extra, group))
    lazy = prepare_test(dm, TTEST) |> via("boot", n = 500)

    updated = update(lazy, seed = 123)
    expect_equal(updated$recalibrate_spec$args$seed, 123)
    expect_equal(updated$recalibrate_spec$args$n, 500)
})
