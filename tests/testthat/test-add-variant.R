simple_variant = variant(
    fn = function(x, group_data, .n = 100L) {
        grp = as.character(group_data[[1]])
        lvls = unique(grp)
        x1 = x[grp == lvls[[1]]]
        x2 = x[grp == lvls[[2]]]
        list(diff = mean(x1) - mean(x2), n = .n)
    }
)

test_that("add_variant %<-% registers variant into registry", {
    add_variant(TTEST, x_by, "test_simple") %<-% simple_variant
    on.exit(remove_variant(TTEST, x_by, "test_simple"))

    key = variant_registry_key("ttest", "x_by")
    expect_false(is.null(variant_registry[[key]][["test_simple"]]))
})

test_that("add_variant %<-% registered variant is reachable via via()", {
    add_variant(TTEST, x_by, "test_simple") %<-% simple_variant
    on.exit(remove_variant(TTEST, x_by, "test_simple"))

    expect_no_error(
        sleep |>
            define_model(extra %by% group) |>
            prepare_test(TTEST) |>
            via("test_simple") |>
            conclude()
    )
})

test_that("add_variant %<-% registered variant returns cld_exec", {
    add_variant(TTEST, x_by, "test_simple") %<-% simple_variant
    on.exit(remove_variant(TTEST, x_by, "test_simple"))

    result = sleep |>
        define_model(extra %by% group) |>
        prepare_test(TTEST) |>
        via("test_simple") |>
        conclude()

    expect_s7_class(result, cld_exec)
    expect_equal(result@cld_meta$method, "test_simple")
})

test_that("add_variant %<-% silently replaces existing user variant", {
    add_variant(TTEST, x_by, "test_simple") %<-% simple_variant
    on.exit(remove_variant(TTEST, x_by, "test_simple"))

    replacement = variant(fn = function(x, group_data) list(replaced = TRUE))
    expect_no_error(
        add_variant(TTEST, x_by, "test_simple") %<-% replacement
    )

    key = variant_registry_key("ttest", "x_by")
    entry = variant_registry[[key]][["test_simple"]]
    expect_true(entry$impl@fn(NULL, NULL)$replaced)
})

test_that("add_variant errors on 'default' name", {
    expect_error(
        add_variant(TTEST, x_by, "default") %<-% simple_variant,
        class = "rlang_error"
    )
})

test_that("add_variant errors when obj has no cls attribute", {
    bare_fn = function() {}
    expect_error(
        add_variant(bare_fn, x_by, "test_simple") %<-% simple_variant,
        class = "rlang_error"
    )
})

test_that("add_variant errors when model_type is not a model_id subclass", {
    expect_error(
        add_variant(TTEST, "x_by", "test_simple") %<-% simple_variant,
        class = "rlang_error"
    )
})

test_that("add_variant errors when value is not a variant object", {
    expect_error(
        add_variant(TTEST, x_by, "test_simple") %<-% function() {},
        class = "rlang_error"
    )
})

test_that("remove_variant removes a user-scoped variant", {
    add_variant(TTEST, x_by, "test_simple") %<-% simple_variant
    remove_variant(TTEST, x_by, "test_simple")

    key = variant_registry_key("ttest", "x_by")
    expect_null(variant_registry[[key]][["test_simple"]])
})

test_that("remove_variant errors on unregistered name", {
    expect_error(
        remove_variant(TTEST, x_by, "nonexistent"),
        class = "rlang_error"
    )
})

test_that("remove_variant errors on 'default' name", {
    expect_error(
        remove_variant(TTEST, x_by, "default"),
        class = "rlang_error"
    )
})

test_that("remove_variant errors on package-scoped variant", {
    key = variant_registry_key("ttest", "x_by")
    variant_registry[[key]][["pkg_variant"]] = list(
        impl = simple_variant,
        origin = "package"
    )
    on.exit(variant_registry[[key]][["pkg_variant"]] <- NULL)

    expect_error(
        remove_variant(TTEST, x_by, "pkg_variant"),
        class = "rlang_error"
    )
})

test_that("via() errors on unregistered variant name", {
    expect_error(
        sleep |>
            define_model(extra %by% group) |>
            prepare_test(TTEST) |>
            via("nonexistent") |>
            conclude(),
        class = "rlang_error"
    )
})

test_that("TTEST remains a function after add_variant %<-%", {
    add_variant(TTEST, x_by, "test_simple") %<-% simple_variant
    on.exit(remove_variant(TTEST, x_by, "test_simple"))

    expect_true(is.function(TTEST))
    expect_equal(attr(TTEST, "cls"), "ttest")
})
