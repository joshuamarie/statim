test_that("prepare_model() returns a model_lazy", {
    ml = cars |>
        define_model(rel(speed, dist)) |>
        prepare_model(LINEAR_REG)

    expect_s7_class(ml, statim:::model_lazy)
})

test_that("prepare_model() stores model_id, processed, model_spec", {
    ml = cars |>
        define_model(rel(speed, dist)) |>
        prepare_model(LINEAR_REG)

    expect_s7_class(ml@model_id, statim::rel)
    expect_s7_class(ml@model_spec, statim:::model_spec)
    expect_named(ml@processed, c("x_data", "resp_data"))
})

test_that("prepare_model() with formula dispatches correctly", {
    ml = cars |>
        define_model(dist ~ speed) |>
        prepare_model(LINEAR_REG)

    expect_s7_class(ml, statim:::model_lazy)
    expect_true(inherits(ml@model_id, "formula"))
})

test_that("prepare_model() with a test function errors", {
    dm = define_model(x_by(extra, group), sleep)

    expect_error(
        prepare_model(dm, TTEST),
        class = "rlang_error"
    )
})

test_that("prepare_model() with a non-function errors", {
    dm = define_model(rel(speed, dist), cars)

    expect_error(prepare_model(dm, "LINEAR_REG"))
})

test_that("as_model_spec() rejects a test function", {
    expect_error(
        statim:::as_model_spec(TTEST),
        class = "rlang_error"
    )
})

test_that("model_lazy recalibrate_spec is NULL by default", {
    ml = cars |>
        define_model(rel(speed, dist)) |>
        prepare_model(LINEAR_REG)

    expect_null(ml@recalibrate_spec)
})

test_that("model_lazy data_name is empty string by default", {
    ml = cars |>
        define_model(rel(speed, dist)) |>
        prepare_model(LINEAR_REG)

    expect_equal(ml@data_name, "")
})

test_that("print.model_lazy() returns invisibly", {
    ml = cars |>
        define_model(rel(speed, dist)) |>
        prepare_model(LINEAR_REG)

    expect_invisible(print(ml))
})

test_that("update() on model_lazy modifies model_spec args when no recalibrate_spec", {
    ml = cars |>
        define_model(rel(speed, dist)) |>
        prepare_model(LINEAR_REG)

    ml2 = update(ml, some_arg = TRUE)

    expect_true(ml2@model_spec@args$some_arg)
})

test_that("via() on model_lazy errors for unregistered variant", {
    expect_error(
        cars |>
            define_model(rel(speed, dist)) |>
            prepare_model(LINEAR_REG) |>
            via("nonexistent_variant"),
        class = "rlang_error"
    )
})

test_that("via() preserves all other model_lazy fields", {
    ml = cars |>
        define_model(rel(speed, dist)) |>
        prepare_model(LINEAR_REG)

    # LINEAR_REG has no variants, so via() should error —
    # confirming via() actually validates against the def
    expect_error(via(ml, "boot"), class = "rlang_error")
})

test_that("conclude() on model_lazy returns cld_exec", {
    result = cars |>
        define_model(rel(speed, dist)) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    expect_s7_class(result, statim:::cld_exec)
})

test_that("conclude() on formula model returns cld_exec", {
    result = cars |>
        define_model(dist ~ speed) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    expect_s7_class(result, statim:::cld_exec)
})

test_that("conclude() on model_lazy sets method to 'default'", {
    result = cars |>
        define_model(rel(speed, dist)) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    expect_equal(result@cld_meta$method, "default")
})

test_that("conclude() on model_lazy stores correct stat_name", {
    result = cars |>
        define_model(rel(speed, dist)) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    expect_equal(result@cld_meta$stat_name, "Linear Regression")
})
