test_that("tidy() on default x_by result returns a tibble", {
    result = sleep |>
        define_model(extra %by% group) |>
        prepare_test(TTEST) |>
        conclude() |>
        tidy()

    expect_s3_class(result, "tbl_df")
})

test_that("tidy() on default x_by result has group column", {
    result = sleep |>
        define_model(extra %by% group) |>
        prepare_test(TTEST) |>
        conclude() |>
        tidy()

    expect_true("group" %in% names(result))
})

test_that("tidy() on boot variant returns tibble with lower, upper, n_reps", {
    result = sleep |>
        define_model(extra %by% group) |>
        prepare_test(TTEST) |>
        via("boot", n = 200L, seed = 1L) |>
        conclude() |>
        tidy()

    expect_s3_class(result, "tbl_df")
    expect_named(result, c("lower", "upper", "n_reps"), ignore.order = TRUE)
})

test_that("tidy() on boot variant lower is less than upper", {
    result = sleep |>
        define_model(extra %by% group) |>
        prepare_test(TTEST) |>
        via("boot", n = 500L, seed = 1L) |>
        conclude() |>
        tidy()

    expect_lt(result$lower, result$upper)
})

test_that("tidy() on contrast variant returns tibble with expected columns", {
    result = sleep |>
        define_model(extra %by% group) |>
        prepare_test(TTEST) |>
        via("contrast") |>
        conclude() |>
        tidy()

    expect_s3_class(result, "tbl_df")
    expect_named(
        result,
        c("group", "contrast", "tstat", "df", "p_value", "lower", "upper"),
        ignore.order = TRUE
    )
})

test_that("tidy() errors when no method registered for variant", {
    simple_variant = variant(fn = function(x, group_data) list(diff = 1))
    add_variant(TTEST, x_by, "test_no_tidy") %<-% simple_variant
    on.exit(remove_variant(TTEST, x_by, "test_no_tidy"))

    result = sleep |>
        define_model(extra %by% group) |>
        prepare_test(TTEST) |>
        via("test_no_tidy") |>
        conclude()

    expect_error(tidy(result), class = "rlang_error")
})

test_that("making_tidy %<-% registers tidy method for new variant", {
    simple_variant = variant(fn = function(x, group_data) list(diff = mean(x)))
    add_variant(TTEST, x_by, "test_tidy_reg") %<-% simple_variant
    on.exit({
        remove_variant(TTEST, x_by, "test_tidy_reg")
    })

    making_tidy(TTEST, x_by) %<-% method_tidy(
        test_tidy_reg = function(.x, ...) {
            tibble::tibble(diff = .x@data$diff)
        }
    )

    result = sleep |>
        define_model(extra %by% group) |>
        prepare_test(TTEST) |>
        via("test_tidy_reg") |>
        conclude() |>
        tidy()

    expect_s3_class(result, "tbl_df")
    expect_named(result, "diff")
})

test_that("making_tidy %<-% merges variants without overwriting existing ones", {
    key = tidy_registry_key("ttest_x_by")
    existing = register_tidy[[key]]

    making_tidy(TTEST, x_by) %<-% method_tidy(
        test_merge = function(.x, ...) tibble::tibble(merged = TRUE)
    )
    on.exit({ register_tidy[[key]] = existing })

    updated = register_tidy[[key]]
    expect_false(is.null(updated@variants[["test_merge"]]))
    expect_false(is.null(updated@default))
})
