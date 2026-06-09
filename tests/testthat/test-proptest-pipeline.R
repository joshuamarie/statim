test_that("prop() produces a prop/model_id object", {
    m = prop(45, 100)

    expect_s7_class(m, prop)
    expect_s7_class(m, model_id)
})

test_that("prop() stores x and n as numerics", {
    m = prop(45, 100)

    expect_equal(m@x, 45)
    expect_equal(m@n, 100)
})

test_that("prop() errors when x > n", {
    expect_error(prop(101, 100))
})

test_that("prop() errors when x is negative", {
    expect_error(prop(-1, 100))
})

test_that("prop() errors when n is zero", {
    expect_error(prop(0, 0))
})

test_that("prop() errors when x is non-integer", {
    expect_error(prop(1.5, 100))
})

test_that("prop() errors when n is non-integer", {
    expect_error(prop(45, 100.5))
})

test_that("prop() errors when x is not length 1", {
    expect_error(prop(c(1, 2), 100))
})

test_that("prop() errors when n is not length 1", {
    expect_error(prop(45, c(100, 200)))
})

test_that("prop() allows x = 0 (zero successes)", {
    expect_no_error(prop(0, 100))
})

test_that("prop() allows x = n (all successes)", {
    expect_no_error(prop(100, 100))
})

# ---- `define_model()` for `prop` ----

test_that("define_model() with prop() produces a def_model object", {
    dm = define_model(prop(45, 100))

    expect_s7_class(dm, def_model)
    expect_s7_class(dm@model_id, prop)
})

test_that("define_model() with prop() populates processed with x and n", {
    dm = define_model(prop(45, 100))

    expect_named(dm@processed, c("x", "n"))
    expect_equal(dm@processed$x, 45)
    expect_equal(dm@processed$n, 100)
})

test_that("model_id_info() for prop returns correct model_type", {
    info = model_id_info(prop(45, 100))

    expect_equal(info@model_type, "prop")
})

test_that("model_id_info() for prop includes x and n in other_info", {
    info = model_id_info(prop(45, 100))

    expect_equal(info@other_info$x, 45)
    expect_equal(info@other_info$n, 100)
})

test_that("model_id_info() for prop always includes vars", {
    info = model_id_info(prop(45, 100))

    expect_length(info@vars, 2)
    expect_equal(info@vars[[1]]$name, "x")
    expect_equal(info@vars[[2]]$name, "n")
    expect_equal(info@vars[[1]]$preview, "<constant>")
    expect_equal(info@vars[[2]]$preview, "<constant>")
})

# ---- P-TEST ----

test_that("class_p_test() errors when ci_level is not length 1", {
    expect_error(
        class_p_test(
            x = 45, n = 100, estimate = 0.45,
            statistic = 45, p_val = 0.368,
            lower_ci = 0.35, upper_ci = 0.55,
            ci_level = c(0.95, 0.99)
        )
    )
})

test_that("class_p_test() errors when ci_level >= 1", {
    expect_error(
        class_p_test(
            x = 45, n = 100, estimate = 0.45,
            statistic = 45, p_val = 0.368,
            lower_ci = 0.35, upper_ci = 0.55,
            ci_level = 1
        )
    )
})

test_that("class_p_test() errors when ci_level <= 0", {
    expect_error(
        class_p_test(
            x = 45, n = 100, estimate = 0.45,
            statistic = 45, p_val = 0.368,
            lower_ci = 0.35, upper_ci = 0.55,
            ci_level = 0
        )
    )
})

test_that("P_TEST() eager path returns a cld_exec object", {
    test_out = P_TEST(prop(45, 100))

    expect_s7_class(test_out@data, class_p_test)
})

test_that("P_TEST() eager path estimate matches binom.test()", {
    test_out = P_TEST(prop(45, 100))
    ref = stats::binom.test(45, 100)

    expect_equal(test_out@data@estimate, unname(ref$estimate))
})

test_that("P_TEST() eager path p_val matches binom.test()", {
    test_out = P_TEST(prop(45, 100))
    ref = stats::binom.test(45, 100)

    expect_equal(test_out@data@p_val, ref$p.value)
})

test_that("P_TEST() eager path stores x and n", {
    test_out = P_TEST(prop(45, 100))

    expect_equal(test_out@data@x, 45)
    expect_equal(test_out@data@n, 100)
})

test_that("P_TEST() pipeline produces same test_out as eager path", {
    eager = P_TEST(prop(45, 100))
    pipeline = define_model(prop(45, 100)) |>
        prepare_test(P_TEST) |>
        conclude()

    expect_equal(eager@data@estimate, pipeline@data@estimate)
    expect_equal(eager@data@p_val, pipeline@data@p_val)
})

test_that("P_TEST() via('prop') matches prop.test() with correction", {
    test_out = define_model(prop(45, 100)) |>
        prepare_test(P_TEST) |>
        via("prop") |>
        update(correct = TRUE) |>
        conclude()
    ref = stats::prop.test(45, 100, correct = TRUE)

    expect_equal(test_out@data@statistic, unname(ref$statistic))
    expect_equal(test_out@data@p_val, ref$p.value)
})

test_that("P_TEST() respects .p argument", {
    test_out = define_model(prop(45, 100)) |>
        prepare_test(P_TEST, .p = 0.3) |>
        conclude()
    ref = stats::binom.test(45, 100, p = 0.3)

    expect_equal(test_out@data@p_val, ref$p.value)
})

test_that("P_TEST() respects .alt = 'greater'", {
    test_out = define_model(prop(45, 100)) |>
        prepare_test(P_TEST, .alt = "greater") |>
        conclude()
    ref = stats::binom.test(45, 100, alternative = "greater")

    expect_equal(test_out@data@p_val, ref$p.value)
})

test_that("P_TEST() respects .ci argument", {
    test_out = define_model(prop(45, 100)) |>
        prepare_test(P_TEST, .ci = 0.99) |>
        conclude()

    expect_equal(test_out@data@ci_level, 0.99)
})

test_that("state_null(PI() == .p) translates to correct .p and two.sided", {
    test_out = define_model(prop(45, 100)) |>
        prepare_test(P_TEST) |>
        state_null(PI() == 0.3) |>
        conclude()
    ref = stats::binom.test(45, 100, p = 0.3)

    expect_equal(test_out@data@p_val, ref$p.value)
})

test_that("state_null(PI() > .p) translates to .alt = 'less'", {
    test_out = define_model(prop(45, 100)) |>
        prepare_test(P_TEST) |>
        state_null(PI() > 0.5) |>
        conclude()
    ref = stats::binom.test(45, 100, alternative = "less")

    expect_equal(test_out@data@p_val, ref$p.value)
})

test_that("state_null(PI() < .p) translates to .alt = 'greater'", {
    test_out = define_model(prop(45, 100)) |>
        prepare_test(P_TEST) |>
        state_null(PI() < 0.5) |>
        conclude()
    ref = stats::binom.test(45, 100, alternative = "greater")

    expect_equal(test_out@data@p_val, ref$p.value)
})

test_that("state_null(PI()) works with via('prop') variant", {
    test_out = define_model(prop(45, 100)) |>
        prepare_test(P_TEST) |>
        via("prop", correct = FALSE) |>
        state_null(PI() == 0.3) |>
        conclude()
    ref = stats::prop.test(45, 100, p = 0.3, correct = FALSE)

    expect_equal(test_out@data@p_val, ref$p.value, tolerance = 1e-6)
})

# ---- print ----

test_that("print.class_p_test() returns invisibly", {
    test_out = P_TEST(prop(45, 100))

    expect_invisible(print(test_out@data))
})
