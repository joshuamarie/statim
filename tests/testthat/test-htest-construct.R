test_that("fun_args creates correct object with defaults", {
    fa = fun_args(.paired = FALSE, .mu = 0, .alt = "two.sided", .ci = 0.95)
    expect_s3_class(fa, "fun_args")
    expect_equal(fa$.paired$default, FALSE)
    expect_equal(fa$.mu$default, 0)
    expect_equal(fa$.alt$default, "two.sided")
    expect_equal(fa$.ci$default, 0.95)
    expect_false(fa$.paired$required)
})

test_that("fun_args marks required args correctly", {
    fa = fun_args(.ci = 0.95, ~.required)
    expect_s3_class(fa, "fun_args")
    expect_false(fa$.ci$required)
    expect_true(fa$.required$required)
    expect_null(fa$.required$default)
})

test_that("fun_args names are correct", {
    fa = fun_args(.a = 1, .b = "x")
    expect_named(fa, c(".a", ".b"))
})

test_that("build_lookup creates named list with correct keys", {
    td1 = test_define(model_type = "x_by", impl_class = "t1", run = function(self) NULL)
    td2 = test_define(
        model_type = "x_by", impl_class = "t2",
        method = method_spec("boot", "bootstrap"),
        run = function(self) NULL
    )
    lookup = build_lookup(list(td1, td2))
    expect_named(lookup, c("x_by::::default", "x_by::boot::default"))
})

test_that("find_def returns correct definition by key", {
    td = test_define(model_type = "x_by", impl_class = "mydef", run = function(self) NULL)
    lookup = build_lookup(list(td))
    result = find_def(lookup, model_type = "x_by", method_name = "", engine = "default")
    expect_equal(result@impl_class, "mydef")
})

test_that("find_def errors when key not found", {
    lookup = list()
    expect_error(find_def(lookup, "x_by", "", "default"))
})

test_that("new_htest creates htest_spec object", {
    result = new_htest(list(x = 1), impl_cls = "ttest_two", test_cls = "ttest")
    expect_s3_class(result, "htest_spec")
    expect_s3_class(result, "ttest_two")
    expect_s3_class(result, "ttest")
    expect_equal(result$data, list(x = 1))
})

test_that("new_htest stores print function from def", {
    printer = function(x, ...) invisible(x)
    td = test_define(
        model_type = "x_by", impl_class = "pd",
        run = function(self) NULL,
        print = printer
    )
    result = new_htest(list(), impl_cls = "pd", test_cls = "t", def = td)
    expect_identical(attr(result, "print_fn"), printer)
})

test_that("print.htest_spec calls print_fn when set", {
    printed = FALSE
    printer = function(x, ...) { printed <<- TRUE; invisible(x) }
    td = test_define(
        model_type = "x_by", impl_class = "pd2",
        run = function(self) NULL,
        print = printer
    )
    obj = new_htest(list(val = 42), "pd2", "t", def = td)
    print(obj)
    expect_true(printed)
})

test_that("print.htest_spec falls back to print(x$data) when no print_fn", {
    obj = new_htest(42, "cls1", "cls2", def = NULL)
    # Should not error
    expect_invisible(print(obj))
})

test_that("HTEST_FN returns a function", {
    td = test_define(model_type = "x_by", impl_class = "t1", run = function(self) 42)
    fn = HTEST_FN(cls = "mytest", defs = list(td), .name = "My Test")
    expect_type(fn, "closure")
})

test_that("HTEST_FN returns test_spec when called with no model", {
    td = test_define(model_type = "x_by", impl_class = "t1", run = function(self) 42)
    fn = HTEST_FN(cls = "mytest", defs = list(td), .name = "My Test")
    spec = fn()
    expect_s3_class(spec, "test_spec")
    expect_equal(spec$cls, "mytest")
    expect_equal(spec$name, "My Test")
})
