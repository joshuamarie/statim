test_that("conclude.test_lazy runs classical TTEST pipeline", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        conclude()

    expect_s3_class(result, "htest_spec")
    expect_s3_class(result, "ttest_two")
    expect_s3_class(result, "ttest")
    expect_equal(result$name, "T-Test")
})

test_that("conclude.test_lazy result data is a tibble with group and ttest columns", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        conclude()

    expect_s3_class(result$data, "data.frame")
    expect_named(result$data, c("group", "ttest"))
    expect_equal(nrow(result$data), 1)
    expect_s3_class(result$data$ttest[[1]], "htest")
})

test_that("conclude.test_lazy with via boot runs bootstrap variant", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        via("boot", n = 50, seed = 42) |>
        conclude()

    expect_s3_class(result, "htest_spec")
    expect_s3_class(result, "ttest_boot")
    expect_equal(result$data$n, 50)
    expect_length(result$data$boot_dist, 50)
})

test_that("conclude.test_lazy with via permute runs permutation variant", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        via("permute", n = 50, seed = 1) |>
        conclude()

    expect_s3_class(result, "ttest_permute")
    expect_equal(result$data$n, 50)
    expect_true(is.numeric(result$data$p.value))
    expect_gte(result$data$p.value, 0)
    expect_lte(result$data$p.value, 1)
})

test_that("conclude.test_lazy honours update() args", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        update(.ci = 0.99) |>
        conclude()

    # CI at 99% — ttest object should reflect it
    ttest_obj = result$data$ttest[[1]]
    expect_equal(attr(ttest_obj$conf.int, "conf.level"), 0.99)
})

test_that("conclude.engine_set with via+through runs engine_set path", {
    # via() first, then through() → engine_set + recalibrate_spec
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        via("permute", n = 50) |>
        through("default") |>
        conclude()

    expect_s3_class(result, "htest_spec")
    expect_s3_class(result, "ttest_permute")
})

test_that("conclude works on formula-based pipeline", {
    result = sleep |>
        define_model(extra ~ group) |>
        prepare_test(TTEST) |>
        conclude()

    expect_s3_class(result, "htest_spec")
    expect_s3_class(result, "ttest_formula")
})
