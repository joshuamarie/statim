# TTEST formula — two-sample -----------------------------------------------

test_that("TTEST formula pipeline returns cld_exec", {
    result = sleep |>
        define_model(extra ~ group) |>
        prepare_test(TTEST) |>
        conclude()

    expect_s7_class(result, cld_exec)
})

test_that("TTEST formula result data is a data frame with type, group, ttest cols", {
    result = sleep |>
        define_model(extra ~ group) |>
        prepare_test(TTEST) |>
        conclude()

    expect_s3_class(result@data, "data.frame")
    expect_named(result@data, c("type", "group", "ttest"))
})

test_that("TTEST formula result contains an htest object", {
    result = sleep |>
        define_model(extra ~ group) |>
        prepare_test(TTEST) |>
        conclude()

    expect_s3_class(result@data$ttest[[1]], "htest")
})

test_that("TTEST formula result matches base R t.test()", {
    result = sleep |>
        define_model(extra ~ group) |>
        prepare_test(TTEST) |>
        conclude()

    base = t.test(extra ~ group, data = sleep)

    expect_equal(result@data$ttest[[1]]$statistic, base$statistic, tolerance = 1e-6)
    expect_equal(result@data$ttest[[1]]$p.value, base$p.value, tolerance = 1e-6)
    expect_equal(result@data$ttest[[1]]$conf.int, base$conf.int, tolerance = 1e-4)
})

test_that("TTEST formula type field is 'two sample' for a grouping variable", {
    result = sleep |>
        define_model(extra ~ group) |>
        prepare_test(TTEST) |>
        conclude()

    expect_equal(result@data$type[[1]], "two sample")
})

test_that("TTEST formula group field matches the RHS variable name", {
    result = sleep |>
        define_model(extra ~ group) |>
        prepare_test(TTEST) |>
        conclude()

    expect_equal(result@data$group[[1]], "group")
})

test_that("TTEST formula errors when group has more than 2 levels", {
    expect_error(
        iris |>
            define_model(Sepal.Length ~ Species) |>
            prepare_test(TTEST) |>
            conclude(),
        class = "rlang_error"
    )
})

test_that("TTEST formula with .alt = 'greater' gives one-sided p-value", {
    result = sleep |>
        define_model(extra ~ group) |>
        prepare_test(TTEST) |>
        update(.alt = "greater") |>
        conclude()

    base = t.test(extra ~ group, data = sleep, alternative = "greater")

    expect_equal(result@data$ttest[[1]]$p.value, base$p.value, tolerance = 1e-6)
})

test_that("TTEST formula print returns invisibly", {
    result = sleep |>
        define_model(extra ~ group) |>
        prepare_test(TTEST) |>
        conclude()

    expect_invisible(print(result))
})

# TTEST formula — one-sample -----------------------------------------------

test_that("TTEST formula one-sample pipeline returns cld_exec", {
    result = sleep |>
        define_model(extra ~ 1) |>
        prepare_test(TTEST) |>
        conclude()

    expect_s7_class(result, cld_exec)
})

test_that("TTEST formula one-sample type field is 'one sample'", {
    result = sleep |>
        define_model(extra ~ 1) |>
        prepare_test(TTEST) |>
        conclude()

    expect_equal(result@data$type[[1]], "one sample")
})

test_that("TTEST formula one-sample matches base R t.test()", {
    result = sleep |>
        define_model(extra ~ 1) |>
        prepare_test(TTEST) |>
        conclude()

    base = t.test(sleep$extra)

    expect_equal(result@data$ttest[[1]]$statistic, base$statistic, tolerance = 1e-6)
    expect_equal(result@data$ttest[[1]]$p.value, base$p.value, tolerance = 1e-6)
})

test_that("TTEST formula one-sample with .mu = 1 matches base R", {
    result = sleep |>
        define_model(extra ~ 1) |>
        prepare_test(TTEST) |>
        update(.mu = 1) |>
        conclude()

    base = t.test(sleep$extra, mu = 1)

    expect_equal(result@data$ttest[[1]]$statistic, base$statistic, tolerance = 1e-6)
    expect_equal(result@data$ttest[[1]]$p.value,   base$p.value,   tolerance = 1e-6)
})

# TTEST formula — tidy() ---------------------------------------------------

test_that("tidy() on TTEST formula result returns a tibble", {
    result = sleep |>
        define_model(extra ~ group) |>
        prepare_test(TTEST) |>
        conclude() |>
        tidy()

    expect_s3_class(result, "tbl_df")
})

test_that("tidy() on TTEST formula result has expected columns", {
    result = sleep |>
        define_model(extra ~ group) |>
        prepare_test(TTEST) |>
        conclude() |>
        tidy()

    expect_true(all(c("type", "groups", "statistic", "p.value") %in% names(result)))
})

test_that("tidy() on TTEST formula result statistic matches base R", {
    result = sleep |>
        define_model(extra ~ group) |>
        prepare_test(TTEST) |>
        conclude() |>
        tidy()

    base = t.test(extra ~ group, data = sleep)

    expect_equal(result$statistic[[1]], base$statistic[["t"]], tolerance = 1e-6)
    expect_equal(result$p.value[[1]], base$p.value, tolerance = 1e-6)
})

test_that("tidy() on TTEST formula one-sample result has type column", {
    result = sleep |>
        define_model(extra ~ 1) |>
        prepare_test(TTEST) |>
        conclude() |>
        tidy()

    expect_s3_class(result, "tbl_df")
    expect_true("type" %in% names(result))
    expect_equal(result$type[[1]], "one sample")
})
