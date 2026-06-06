test_that("define_model() dispatches on model-ID first style", {
    dm = define_model(x_by(extra, group), sleep)

    expect_s7_class(dm, def_model)
    expect_s7_class(dm@model_id, x_by)
    expect_named(dm@processed, c("x_data", "group_data"))
})

test_that("define_model() dispatches on data-frame first style", {
    dm = sleep |> define_model(x_by(extra, group))

    expect_s7_class(dm, def_model)
    expect_s7_class(dm@model_id, x_by)
    expect_named(dm@processed, c("x_data", "group_data"))
})

test_that("define_model() data-frame first and model-ID first produce identical results", {
    dm_mf = define_model(x_by(extra, group), sleep)
    dm_df = sleep |> define_model(x_by(extra, group))

    expect_equal(dm_mf@processed, dm_df@processed)
})

test_that("define_model() with rel() populates x_data and resp_data", {
    dm = define_model(rel(speed, dist), cars)

    expect_s7_class(dm@model_id, rel)
    expect_named(dm@processed, c("x_data", "resp_data"))
    expect_equal(dm@processed$x_data$speed, cars$speed)
    expect_equal(dm@processed$resp_data$dist, cars$dist)
})

test_that("define_model() with pairwise() populates var_names, pairs, data", {
    dm = define_model(pairwise(speed, dist), cars)

    expect_s7_class(dm@model_id, pairwise)
    expect_named(dm@processed, c("var_names", "pairs", "data"))
    expect_all_true(dm@processed$var_names == c("speed", "dist"))
    # expect_equal(dm@processed$var_names, c("speed", "dist"))
})

test_that("define_model() with formula dispatches correctly", {
    dm = define_model(extra ~ group, sleep)

    expect_s7_class(dm, def_model)
    expect_true(inherits(dm@model_id, "formula"))
    expect_named(dm@processed, c("data", "vars", "formula"))
})

# ---- x_by ----

test_that("x_by() produces an x_by/model_id object", {
    m = x_by(extra, group)

    expect_s7_class(m, x_by)
    expect_s7_class(m, model_id)
})

test_that("x_by() stores quosures in @x and @group", {
    m = x_by(extra, group)

    expect_true(rlang::is_quosure(m@x))
    expect_true(rlang::is_quosure(m@group))
})

test_that("x_by() with inline data via I()", {
    m = x_by(I(score = rnorm(30)), I(grp = rep(c("a", "b"), each = 15)))
    dm = define_model(m, NULL)

    expect_named(dm@processed$x_data, "score")
    expect_named(dm@processed$group_data, "grp")
    expect_length(dm@processed$x_data$score, 30)
})

test_that("x_by() with c() selects multiple columns", {
    df = data.frame(x1 = 1:10, x2 = 11:20, g = rep(c("a", "b"), 5))
    dm = define_model(x_by(c(x1, x2), g), df)

    expect_equal(ncol(dm@processed$x_data), 2)
    expect_named(dm@processed$x_data, c("x1", "x2"))
})

test_that("x_by() with inlines() produces correctly named columns", {
    dm = define_model(
        x_by(inlines(a = rnorm(20), rnorm(20)), I(rep(c("x", "y"), 10))),
        NULL
    )

    expect_named(dm@processed$x_data, c("a", "xv2"))
})

test_that("%by% is an alias for x_by()", {
    m1 = x_by(extra, group)
    m2 = extra %by% group

    expect_s7_class(m2, x_by)
    expect_equal(
        rlang::as_label(m1@x),
        rlang::as_label(m2@x)
    )
})

# ---- rel ----

test_that("rel() produces a rel/model_id object", {
    m = rel(speed, dist)

    expect_s7_class(m, rel)
    expect_s7_class(m, model_id)
})

test_that("rel() with tidyselect helper requires data — errors without it", {
    # tidyselect helpers are only valid when a data frame is supplied;
    # passing NULL should abort with an rlang error
    expect_error(
        define_model(rel(starts_with("sp"), dist), NULL),
        class = "rlang_error"
    )
})

test_that("rel() resolves columns from data frame", {
    dm = define_model(rel(speed, dist), cars)

    expect_equal(dm@processed$x_data$speed, cars$speed)
    expect_equal(dm@processed$resp_data$dist, cars$dist)
})

# ---- pairwise ----

test_that("pairwise() produces a pairwise/model_id object", {
    m = pairwise(a, b, c)

    expect_s7_class(m, pairwise)
    expect_s7_class(m, model_id)
})

test_that("pairwise() default direction is 'lt'", {
    m = pairwise(a, b, c)

    expect_equal(m@direction, "lt")
})

test_that("pairwise() direction is stored correctly", {
    m = pairwise(a, b, c, direction = "all")

    expect_equal(m@direction, "all")
})

test_that("pairwise() with direction = 'lt' produces strictly lower-triangular pairs", {
    df = data.frame(a = 1:5, b = 1:5, c = 1:5)
    dm = define_model(pairwise(a, b, c), df)
    pairs = dm@processed$pairs

    # with 3 vars and direction "lt": (a,b), (a,c), (b,c) = 3 pairs
    expect_length(pairs, 3)
    # no pair should have identical elements
    expect_true(all(vapply(pairs, \(p) p[[1]] != p[[2]], logical(1))))
})

test_that("pairwise() with direction = 'all' includes self-pairs", {
    df = data.frame(a = 1:5, b = 1:5, c = 1:5)
    dm = define_model(pairwise(a, b, c, direction = "all"), df)

    # 3 vars, direction "all": 3 * 3 = 9 pairs
    # This means "all combinations"
    expect_length(dm@processed$pairs, 9)
})

test_that("pairwise() with direction = 'eq' returns only self-pairs", {
    df = data.frame(a = 1:5, b = 1:5, c = 1:5)
    dm = define_model(pairwise(a, b, c, direction = "eq"), df)

    expect_length(dm@processed$pairs, 3)
    expect_true(all(vapply(dm@processed$pairs, \(p) p[[1]] == p[[2]], logical(1))))
})

# ---- model_id_info ----

test_that("model_id_info() for x_by without processed omits vars and counts", {
    info = model_id_info(x_by(extra, group))

    expect_equal(info$model_type, "x_by")
    expect_match(info$args, "extra")
    expect_match(info$args, "group")
    expect_null(info$vars)
    expect_length(info$other_info, 0)
})

test_that("model_id_info() for x_by with processed includes vars and counts", {
    dm = define_model(x_by(extra, group), sleep)
    info = model_id_info(dm@model_id, dm@processed)

    expect_equal(info$other_info$x_vars, 1)
    expect_equal(info$other_info$by_vars, 1)
    expect_length(info$vars, 2)
})

test_that("model_id_info() for rel without processed omits vars", {
    info = model_id_info(rel(speed, dist))

    expect_equal(info$model_type, "rel")
    expect_null(info$vars)
})

test_that("model_id_info() for rel with processed includes vars and counts", {
    dm = define_model(rel(speed, dist), cars)
    info = model_id_info(dm@model_id, dm@processed)

    expect_equal(info$other_info$x_vars, 1)
    expect_equal(info$other_info$resp_vars, 1)
    expect_length(info$vars, 2)
})

test_that("model_id_info() for pairwise includes direction in other_info", {
    info = model_id_info(pairwise(a, b, c))

    expect_equal(info$model_type, "pairwise")
    expect_equal(info$other_info$direction, "lt")
})

test_that("model_id_info() for pairwise with processed includes n_pairs", {
    df = data.frame(a = 1:5, b = 1:5, c = 1:5)
    dm = define_model(pairwise(a, b, c), df)
    info = model_id_info(dm@model_id, dm@processed)

    expect_equal(info$other_info$n_pairs, 3)
    expect_length(info$vars, 3)
})

test_that("model_id_info() for formula reports left_var and right_var", {
    dm = define_model(extra ~ group, sleep)
    info = model_id_info(dm@model_id, dm@processed)

    expect_equal(info$model_type, "formula")
    expect_equal(info$other_info$left_var, 1)
    expect_equal(info$other_info$right_var, 1)
})

# ---- prop ----

test_that("define_model() with prop() dispatches on model-ID first style", {
    dm = define_model(prop(45, 100))

    expect_s7_class(dm, def_model)
    expect_s7_class(dm@model_id, prop)
    expect_named(dm@processed, c("x", "n"))
})

test_that("model_id_info() for prop without processed still includes vars", {
    info = model_id_info(prop(45, 100))

    expect_equal(info$model_type, "prop")
    expect_length(info$vars, 2)
})

test_that("print.def_model() returns invisibly for prop()", {
    dm = define_model(prop(45, 100))

    expect_invisible(print(dm))
})

test_that("print.model_id() returns invisibly for prop()", {
    expect_invisible(print(prop(45, 100)))
})

# print methods ----------------------------------------------------------------

test_that("print.def_model() returns invisibly", {
    dm = define_model(x_by(extra, group), sleep)

    expect_invisible(print(dm))
})

test_that("print.model_id() returns invisibly", {
    expect_invisible(print(x_by(extra, group)))
    expect_invisible(print(rel(speed, dist)))
    expect_invisible(print(pairwise(a, b, c)))
})
