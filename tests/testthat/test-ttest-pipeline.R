test_that("TTEST() eager form returns stat_infer_spec", {
    result = TTEST(x_by(extra, group), sleep)

    expect_s7_class(result, stat_infer_spec)
})

test_that("TTEST() eager result data is a data frame with group and ttest cols", {
    result = TTEST(x_by(extra, group), sleep)

    expect_s3_class(result@data, "data.frame")
    expect_named(result@data, c("group", "ttest"))
})

test_that("TTEST() eager result contains an htest object", {
    result = TTEST(x_by(extra, group), sleep)

    expect_s3_class(result@data$ttest[[1]], "htest")
})

test_that("TTEST() eager result matches base R t.test()", {
    result = TTEST(x_by(extra, group), sleep)
    base = t.test(extra ~ group, data = sleep)

    expect_equal(result@data$ttest[[1]]$statistic, base$statistic, tolerance = 1e-6)
    expect_equal(result@data$ttest[[1]]$p.value, base$p.value, tolerance = 1e-6)
})

test_that("TTEST() eager print returns invisibly", {
    result = TTEST(x_by(extra, group), sleep)

    expect_invisible(print(result))
})

test_that("classical pipeline returns cld_exec", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        conclude()

    expect_s7_class(result, cld_exec)
})

test_that("classical pipeline result matches eager result numerically", {
    eager = TTEST(x_by(extra, group), sleep)
    pipeline = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        conclude()

    expect_equal(
        eager@data$ttest[[1]]$statistic,
        pipeline@data$ttest[[1]]$statistic,
        tolerance = 1e-6
    )
})

test_that("classical pipeline with .paired = TRUE runs without error", {
    expect_no_error(
        sleep |>
            define_model(x_by(extra, group)) |>
            prepare_test(TTEST) |>
            update(.paired = TRUE) |>
            conclude()
    )
})

test_that("classical pipeline with wrong number of groups errors", {
    expect_error(
        iris |>
            define_model(x_by(Sepal.Length, Species)) |>
            prepare_test(TTEST) |>
            conclude(),
        class = "rlang_error"
    )
})

test_that("permute variant returns cld_exec with method = 'permute'", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        via("permute", n = 200L, seed = 1L) |>
        conclude()

    expect_s7_class(result, cld_exec)
    expect_equal(result@cld_meta$method, "permute")
})

test_that("permute variant result contains observed, null_dist, p.value, n", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        via("permute", n = 200L, seed = 1L) |>
        conclude()

    expect_named(
        result@data,
        c("observed", "null_dist", "p.value", "n"),
        ignore.order = TRUE
    )
    expect_length(result@data$null_dist, 200L)
})

test_that("permute variant is reproducible with seed", {
    run = function() {
        sleep |>
            define_model(x_by(extra, group)) |>
            prepare_test(TTEST) |>
            via("permute", n = 200L, seed = 42L) |>
            conclude()
    }

    expect_equal(run()@data$p.value, run()@data$p.value)
})

test_that("permute variant p.value is in [0, 1]", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        via("permute", n = 500L, seed = 1L) |>
        conclude()

    expect_gte(result@data$p.value, 0)
    expect_lte(result@data$p.value, 1)
})

test_that("boot variant returns cld_exec with method = 'boot'", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        via("boot", n = 200L, seed = 1L) |>
        conclude()

    expect_s7_class(result, cld_exec)
    expect_equal(result@cld_meta$method, "boot")
})

test_that("boot variant result contains boot_dist, ci, n", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        via("boot", n = 200L, seed = 1L) |>
        conclude()

    expect_named(result@data, c("boot_dist", "ci", "n"), ignore.order = TRUE)
    expect_length(result@data$boot_dist, 200L)
    expect_length(result@data$ci, 2L)
})

test_that("boot variant ci lower is less than upper", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        via("boot", n = 500L, seed = 1L) |>
        conclude()

    expect_lt(result@data$ci[[1]], result@data$ci[[2]])
})

test_that("boot variant is reproducible with seed", {
    run = function() {
        sleep |>
            define_model(x_by(extra, group)) |>
            prepare_test(TTEST) |>
            via("boot", n = 200L, seed = 7L) |>
            conclude()
    }

    expect_equal(run()@data$ci, run()@data$ci)
})

test_that("contrast variant returns cld_exec", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        via("contrast") |>
        conclude()

    expect_s7_class(result, cld_exec)
    expect_equal(result@cld_meta$method, "contrast")
})

test_that("contrast variant result contains tstat, p.value, ci", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        via("contrast") |>
        conclude()

    expect_true(!is.null(result@data$tstat))
    expect_true(!is.null(result@data$p.value))
    expect_length(result@data$ci, 2L)
})

test_that("contrast variant with wrong number of groups errors", {
    expect_error(
        iris |>
            define_model(x_by(Sepal.Length, Species)) |>
            prepare_test(TTEST) |>
            via("contrast") |>
            conclude(),
        class = "rlang_error"
    )
})

test_that("state_null() with MU >= 0 runs through to conclude()", {
    result = sleep |>
        define_model(extra %by% group) |>
        prepare_test(TTEST) |>
        state_null(MU(extra) >= 0) |>
        conclude()

    expect_s7_class(result, cld_exec)
})

test_that("state_null() with unsupported param type errors", {
    expect_error(
        sleep |>
            define_model(x_by(extra, group)) |>
            prepare_test(TTEST) |>
            state_null(PI(extra) == 0.5) |>
            conclude(),
        class = "rlang_error"
    )
})

test_that("tidy() on classical pipeline result returns a tibble", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        conclude() |>
        tidy()

    expect_s3_class(result, "tbl_df")
})

test_that("tidy() on classical pipeline result has correct columns", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        conclude() |>
        tidy()

    expect_true(all(c("group", "estimate", "statistic", "p.value") %in% names(result)))
})

test_that("tidy() on `boot` variant returns a tibble", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        via("boot") |>
        conclude() |>
        tidy()

    expect_s3_class(result, "tbl_df")
})

test_that("tidy() on boot variant returns a tibble with lower and upper", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        via("boot", n = 200L, seed = 1L) |>
        conclude() |>
        tidy()

    expect_s3_class(result, "tbl_df")
    expect_true(all(c("lower", "upper") %in% names(result)))
})

test_that("tidy() on contrast variant returns a tibble with tstat and p_value", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        via("contrast") |>
        conclude() |>
        tidy()

    expect_s3_class(result, "tbl_df")
    expect_true(all(c("tstat", "p_value") %in% names(result)))
})
