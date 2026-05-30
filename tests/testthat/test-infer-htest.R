test_that("prepare_test() returns a test_lazy", {
    tl = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST)

    expect_s7_class(tl, test_lazy)
})

test_that("prepare_test() stores model_id, processed, test_spec", {
    tl = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST)

    expect_s7_class(tl@model_id, x_by)
    expect_s7_class(tl@test_spec, test_spec)
    expect_named(tl@processed, c("x_data", "group_data"))
})

test_that("prepare_test() with a model function errors", {
    dm = define_model(rel(speed, dist), cars)

    expect_error(
        prepare_test(dm, LINEAR_REG),
        class = "rlang_error"
    )
})

test_that("prepare_test() with a non-function errors", {
    dm = define_model(x_by(extra, group), sleep)

    expect_error(prepare_test(dm, "TTEST"))
})

test_that("as_test_spec() rejects a model function", {
    expect_error(
        as_test_spec(LINEAR_REG),
        class = "rlang_error"
    )
})

test_that("test_lazy recalibrate_spec is NULL by default", {
    tl = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST)

    expect_null(tl@recalibrate_spec)
})

test_that("test_lazy claims is NULL by default", {
    tl = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST)

    expect_null(tl@claims)
})

test_that("test_lazy data_name is empty string by default", {
    tl = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST)

    expect_equal(tl@data_name, "")
})

test_that("print.test_lazy() returns invisibly", {
    tl = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST)

    expect_invisible(print(tl))
})

test_that("update() on test_lazy modifies test_spec args when no recalibrate_spec", {
    tl = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST)

    tl2 = update(tl, .paired = TRUE)

    expect_true(tl2@test_spec@args$.paired)
})

test_that("update() on test_lazy modifies recalibrate_spec args when present", {
    tl = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        via("permute", n = 500L)

    tl2 = update(tl, n = 999L)

    expect_equal(tl2@recalibrate_spec$args$n, 999L)
})

test_that("via() sets recalibrate_spec on test_lazy", {
    tl = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        via("permute", n = 500L)

    expect_equal(tl@recalibrate_spec$method_name, "permute")
    expect_equal(tl@recalibrate_spec$args$n, 500L)
})

test_that("via() with an unregistered variant errors", {
    expect_error(
        sleep |>
            define_model(x_by(extra, group)) |>
            prepare_test(TTEST) |>
            via("nonexistent_variant"),
        class = "rlang_error"
    )
})

test_that("via() preserves all other test_lazy fields", {
    tl = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST)

    tl_via = via(tl, "permute", n = 100L)

    expect_equal(tl@processed, tl_via@processed)
    expect_equal(tl@test_spec@cls, tl_via@test_spec@cls)
})

test_that("conclude() on default method returns cld_exec", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        conclude()

    expect_s7_class(result, cld_exec)
})

test_that("conclude() dispatches permute variant", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        via("permute", n = 200L, seed = 1L) |>
        conclude()

    expect_s7_class(result, cld_exec)
    expect_equal(result@cld_meta$method, "permute")
})

test_that("conclude() dispatches boot variant", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        via("boot", n = 200L, seed = 1L) |>
        conclude()

    expect_s7_class(result, cld_exec)
    expect_equal(result@cld_meta$method, "boot")
})
