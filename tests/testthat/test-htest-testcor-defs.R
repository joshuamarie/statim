test_that("CORTEST pipeline works on rel with single x", {
    result = cars |>
        define_model(rel(speed, dist)) |>
        prepare_test(CORTEST) |>
        conclude()

    expect_s3_class(result, "htest_spec")
    expect_s3_class(result, "cortest_rel")
    expect_named(result$data$res, c("x", "resp", "cortest"))
    expect_equal(nrow(result$data$res), 1L)
    expect_s3_class(result$data$res$cortest[[1]], "htest")
})

test_that("CORTEST eager path works", {
    result = CORTEST(rel(speed, dist), cars)
    expect_s3_class(result, "cortest_rel")
    expect_equal(nrow(result$data$res), 1L)
})

test_that("CORTEST returns test_spec when called with no model", {
    spec = CORTEST()
    expect_s3_class(spec, "test_spec")
    expect_equal(spec$cls, "cortest")
})

test_that("cor_test_rel many-to-one produces one row per x variable", {
    result = iris |>
        define_model(rel(starts_with("Se"), Petal.Width)) |>
        prepare_test(CORTEST) |>
        conclude()

    expect_equal(nrow(result$data$res), 2L)
    expect_equal(result$data$res$x, c("Sepal.Length", "Sepal.Width"))
    expect_true(all(result$data$res$resp == "Petal.Width"))
})

test_that("cor_test_rel pair label is resp ~ x", {
    result = cars |>
        define_model(rel(speed, dist)) |>
        prepare_test(CORTEST) |>
        conclude()

    expect_equal(result$data$res$x[[1]], "speed")
    expect_equal(result$data$res$resp[[1]], "dist")
})

test_that("cor_test_rel estimates are numerically correct", {
    result = cars |>
        define_model(rel(speed, dist)) |>
        prepare_test(CORTEST) |>
        conclude()

    expected = cor.test(cars$speed, cars$dist)
    expect_equal(result$data$res$cortest[[1]]$estimate, expected$estimate)
    expect_equal(result$data$res$cortest[[1]]$p.value, expected$p.value)
})

test_that("cor_test_rel respects .alt argument", {
    result = cars |>
        define_model(rel(speed, dist)) |>
        prepare_test(CORTEST) |>
        update(.alt = "greater") |>
        conclude()

    expect_equal(result$data$res$cortest[[1]]$alternative, "greater")
})

test_that("cor_test_rel respects .ci argument", {
    result = cars |>
        define_model(rel(speed, dist)) |>
        prepare_test(CORTEST) |>
        update(.ci = 0.99) |>
        conclude()

    ci = result$data$res$cortest[[1]]$conf.int
    expect_equal(attr(ci, "conf.level"), 0.99)
})

test_that("cor_test_rel spearman has no conf.int", {
    expect_warning(
        {l
            result = cars |>
                define_model(rel(speed, dist)) |>
                prepare_test(CORTEST) |>
                update(.cor_type = "spearman") |>
                conclude()
        },
        regexp = "Cannot compute exact p-value with ties"
    )

    expect_null(result$data$res$cortest[[1]]$conf.int)
})

test_that("cor_test_rel errors when resp has more than one variable", {
    expect_error(
        iris |>
            define_model(rel(Sepal.Length, c(Petal.Width, Petal.Length))) |>
            prepare_test(CORTEST) |>
            conclude(),
        regexp = "resp.*must be a single variable"
    )
})
