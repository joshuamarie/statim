test_that("method_spec creates correct S7 object", {
    ms = method_spec("boot", method_type = "bootstrap", defaults = list(n = 1000L, seed = NULL))
    expect_true(inherits(ms, "statim::method_spec"))
    expect_equal(ms@method_name, "boot")
    expect_equal(ms@method_type, "bootstrap")
    expect_equal(ms@defaults, list(n = 1000L, seed = NULL))
})

test_that("method_spec with empty defaults", {
    ms = method_spec("permute", method_type = "replicate")
    expect_equal(ms@defaults, list())
})

test_that("test_define creates correct S7 object", {
    td = test_define(
        model_type = "x_by",
        impl_class = "mytest_two",
        engine = "default",
        run = function(self) list(result = 42)
    )
    expect_true(inherits(td, "statim::test_define"))
    expect_equal(td@model_type, "x_by")
    expect_equal(td@impl_class, "mytest_two")
    expect_equal(td@engine, "default")
    expect_null(td@method)
    expect_null(td@fun_args)
})

test_that("test_define stores method and fun_args correctly", {
    ms = method_spec("boot", "bootstrap", defaults = list(n = 500L))
    fa = fun_args(.ci = 0.95)

    td = test_define(
        model_type = "x_by",
        impl_class = "mytest_boot",
        method = ms,
        fun_args = fa,
        run = function(self) NULL
    )
    expect_equal(td@method@method_name, "boot")
    expect_equal(td@fun_args$.ci$default, 0.95)
})

test_that("test_define stores vars and print functions", {
    extractor = function(p) p$x
    printer = function(x, ...) invisible(x)

    td = test_define(
        model_type = "x_by",
        impl_class = "mytest_print",
        vars = list(x = extractor),
        run = function(self) NULL,
        print = printer
    )
    expect_identical(td@vars$x, extractor)
    expect_identical(td@print, printer)
})
