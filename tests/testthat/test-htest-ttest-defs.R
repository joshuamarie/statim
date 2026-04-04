test_that("full TTEST pipeline works on sleep with x_by", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        conclude()

    expect_s3_class(result, "htest_spec")
    expect_s3_class(result, "ttest")
    expect_s3_class(result$data, "data.frame")
})

test_that("TTEST eager path with x_by works", {
    result = TTEST(x_by(extra, group), sleep)
    expect_s3_class(result, "htest_spec")
    expect_s3_class(result, "ttest_two")
    expect_equal(result$name, "T-Test")
})

test_that("TTEST returns test_spec when called with no model", {
    spec = TTEST()
    expect_s3_class(spec, "test_spec")
    expect_equal(spec$cls, "ttest")
})

test_that("ttest_def_two produces correct t-test result", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        conclude()

    ttest_obj = result$data$ttest[[1]]
    expect_s3_class(ttest_obj, "htest")
    expect_equal(names(result$data), c("group", "ttest"))
    expect_equal(result$data$group, "group")
})

test_that("ttest_def_two respects .paired argument", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        update(.paired = TRUE) |>
        conclude()

    expect_s3_class(result, "htest_spec")
    ttest_obj = result$data$ttest[[1]]
    # Paired t-test has 1 df less
    expect_true(ttest_obj$parameter["df"] < 18)
})

test_that("ttest_def_two respects .alt argument", {
    result_greater = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        update(.alt = "greater") |>
        conclude()

    ttest_obj = result_greater$data$ttest[[1]]
    expect_equal(ttest_obj$alternative, "greater")
})

test_that("ttest_def_two errors when group has != 2 levels", {
    # iris has 3 species
    expect_error(
        iris |>
            define_model(x_by(Sepal.Length, Species)) |>
            prepare_test(TTEST) |>
            conclude()
    )
})

test_that("ttest_def_boot pipeline works", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        via("boot", n = 100, seed = 42) |>
        conclude()

    expect_s3_class(result, "ttest_boot")
    expect_equal(result$data$n, 100)
    expect_length(result$data$boot_dist, 100)
    expect_named(result$data$ci, c("2.5%", "97.5%"))
})

test_that("ttest_def_boot seed makes results reproducible", {
    run = function() {
        sleep |>
            define_model(x_by(extra, group)) |>
            prepare_test(TTEST) |>
            via("boot", n = 100, seed = 99) |>
            conclude()
    }
    r1 = run()
    r2 = run()
    expect_equal(r1$data$boot_dist, r2$data$boot_dist)
})

test_that("ttest_def_permute pipeline works", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        via("permute", n = 100, seed = 1) |>
        conclude()

    expect_s3_class(result, "ttest_permute")
    expect_equal(result$data$n, 100)
    expect_true(is.numeric(result$data$observed))
    expect_true(is.numeric(result$data$p.value))
    expect_gte(result$data$p.value, 0)
    expect_lte(result$data$p.value, 1)
    expect_length(result$data$null_dist, 100)
})

test_that("ttest_def_permute seed makes results reproducible", {
    run = function() {
        sleep |>
            define_model(x_by(extra, group)) |>
            prepare_test(TTEST) |>
            via("permute", n = 100, seed = 7) |>
            conclude()
    }
    r1 = run()
    r2 = run()
    expect_equal(r1$data$observed, r2$data$observed)
    expect_equal(r1$data$null_dist, r2$data$null_dist)
})

test_that("ttest_def_permute_rfast pipeline works", {
    skip_if_not_installed("Rfast2")
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        via("permute", engine = "rfast", B = 199) |>
        conclude()

    expect_s3_class(result, "ttest_permute_rfast")
    expect_true(is.numeric(result$data$stat))
    expect_true(is.numeric(result$data$p.value))
    expect_equal(result$data$B, 199)
})

test_that("ttest_def_formula two-sample pipeline works", {
    result = sleep |>
        define_model(extra ~ group) |>
        prepare_test(TTEST) |>
        conclude()

    expect_s3_class(result, "ttest_formula")
    expect_named(result$data, c("type", "group", "ttest"))
    expect_equal(result$data$type[[1]], "two sample")
})

test_that("ttest_def_formula eager path works", {
    result = TTEST(extra ~ group, sleep)
    expect_s3_class(result, "ttest_formula")
})

test_that("ttest_def_formula one-sample detection works", {
    # one-sample formula: extra ~ 1
    result = TTEST(extra ~ 1, sleep)
    expect_s3_class(result, "ttest_formula")
    expect_equal(result$data$type[[1]], "one sample")
})

test_that("ttest_def_formula errors when group has != 2 levels", {
    expect_error(
        iris |>
            define_model(Sepal.Length ~ Species) |>
            prepare_test(TTEST) |>
            conclude()
    )
})

test_that("TTEST with .extra_defs adds new implementations", {
    my_def = test_define(
        model_type = "x_by",
        impl_class = "custom_impl",
        method = method_spec("custom_m", "custom"),
        vars = list(
            x = function(p) p$x_data[[1]],
            group = function(p) p$group_data[[1]]
        ),
        run = function(self) list(result = "custom")
    )
    fn = TTEST(.extra_defs = list(my_def))
    expect_s3_class(fn, "test_spec")
    expect_true("x_by::custom_m::default" %in% names(fn$lookup))
})
