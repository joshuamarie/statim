test_that("model_id_info.x_by returns correct structure without processed", {
    model_metadata_xby = model_id_info(x_by(extra, group))
    expect_named(model_metadata_xby, c("model_type", "args", "other_info"))
    expect_equal(model_metadata_xby$model_type, "x_by")
    expect_equal(model_metadata_xby$args, "extra | group")
    expect_equal(model_metadata_xby$other_info, list())
})

test_that("model_id_info.rel returns correct structure without processed", {
    model_metadata_rel = model_id_info(rel(speed, dist))
    expect_named(model_metadata_rel, c("model_type", "args", "other_info"))
    expect_equal(model_metadata_rel$model_type, "rel")
    expect_equal(model_metadata_rel$args, "speed ; dist")
    expect_equal(model_metadata_rel$other_info, list())
})

test_that("model_id_info.pairwise returns correct structure without processed", {
    model_metadata_pairwise = model_id_info(pairwise(a, b, c))
    expect_named(model_metadata_pairwise, c("model_type", "args", "other_info"))
    expect_equal(model_metadata_pairwise$model_type, "pairwise")
    expect_equal(model_metadata_pairwise$args, "a, b, c")
    expect_equal(model_metadata_pairwise$other_info$direction, "lt")
    expect_null(model_metadata_pairwise$other_info$n_pairs)
})

test_that("model_id_info.formula returns correct structure without processed", {
    model_metadata_formula = model_id_info(define_model(extra ~ group, sleep)$model_id)
    expect_named(model_metadata_formula, c("model_type", "args", "other_info"))
    expect_equal(model_metadata_formula$model_type, "formula")
    expect_equal(model_metadata_formula$args, "extra ~ group")
    expect_equal(model_metadata_formula$other_info$left_var, 1L)
    expect_equal(model_metadata_formula$other_info$right_var, 1L)
})

test_that("model_id_info.x_by populates other_info and vars with processed", {
    dm = define_model(x_by(extra, group), sleep)
    result = model_id_info(dm$model_id, dm$processed)
    expect_equal(result$other_info$x_vars, 1L)
    expect_equal(result$other_info$by_vars, 1L)
    expect_length(result$vars, 2L)
    expect_equal(result$vars[[1]]$name, "extra")
    expect_equal(result$vars[[2]]$name, "group")
    expect_match(result$vars[[1]]$preview, "^<dbl")
    expect_match(result$vars[[2]]$preview, "^<fct|^<chr")
})

test_that("model_id_info.rel populates other_info and vars with processed", {
    dm = define_model(rel(speed, dist), cars)
    result = model_id_info(dm$model_id, dm$processed)
    expect_equal(result$other_info$x_vars, 1L)
    expect_equal(result$other_info$resp_vars, 1L)
    expect_length(result$vars, 2L)
    expect_equal(result$vars[[1]]$name, "speed")
    expect_equal(result$vars[[2]]$name, "dist")
})

test_that("model_id_info.pairwise populates n_pairs and vars with processed", {
    dm = define_model(pairwise(extra, ID), sleep)
    result = model_id_info(dm$model_id, dm$processed)
    expect_equal(result$other_info$direction, "lt")
    expect_equal(result$other_info$n_pairs, 1L)
    expect_length(result$vars, 2L)
})

test_that("model_id_info.formula populates vars with processed", {
    dm = define_model(extra ~ group, sleep)
    result = model_id_info(dm$model_id, dm$processed)
    expect_length(result$vars, 2L)
    expect_equal(result$vars[[1]]$name, "extra")
    expect_equal(result$vars[[2]]$name, "group")
})

test_that("c() args are stripped of wrapper in args string", {
    extra_c = model_id_info(x_by(c(x1, x2), group))
    expect_equal(extra_c$args, "x1, x2 | group")
})

test_that("resolve_quo errors with check_missing_data for missing symbol in c() call", {
    q = rlang::quo(c(doesnotexist1, doesnotexist2))
    expect_error(
        resolve_quo(q, data = NULL, role = "x"),
        class = "check_missing_data"
    )
})

test_that("resolve_quo resolves c() of names from the environment", {
    x1 = 1:10
    x2 = 11:20
    q = rlang::quo(c(x1, x2))
    result = resolve_quo(q, data = NULL, role = "x")
    expect_named(result, c("x1", "x2"))
    expect_equal(result$x1, 1:10)
    expect_equal(result$x2, 11:20)
})

test_that("I() shows as <inline> in args string", {
    extra_inline = model_id_info(x_by(I(rnorm(30)), group))
    expect_equal(extra_inline$args, "<inline> | group")
})

test_that("resolve_quo resolves unnamed inlines() with auto-names", {
    set.seed(1)
    quo = rlang::quo(inlines(rnorm(10), rnorm(10)))
    extra_inlines = resolve_quo(quo, data = NULL, role = "x")
    expect_s3_class(extra_inlines, "data.frame")
    expect_named(extra_inlines, c("xv1", "xv2"))
    expect_length(extra_inlines[[1]], 10)
    expect_length(extra_inlines[[2]], 10)
})

test_that("inlines() shows as <inlines> in args string", {
    extra_inlines = model_id_info(x_by(inlines(rnorm(30), rnorm(30)), group))
    expect_equal(extra_inlines$args, "<inlines> | group")
})

test_that("tidyselect helper is deparsed in args string", {
    extra_tidyselect = model_id_info(pairwise(starts_with("x")))
    expect_equal(extra_tidyselect$args, 'starts_with("x")')
})

test_that("colon range selector is deparsed in args string", {
    extra_range_selector = model_id_info(pairwise(Sepal.Length:Petal.Width))
    expect_equal(extra_range_selector$args, "Sepal.Length:Petal.Width")
})

test_that("print.model_id snapshot for x_by", {
    expect_snapshot(print(x_by(extra, group)))
})

test_that("print.model_id snapshot for rel", {
    expect_snapshot(print(rel(speed, dist)))
})

test_that("print.model_id snapshot for pairwise", {
    expect_snapshot(print(pairwise(a, b, c)))
})

test_that("print.def_model snapshot for x_by", {
    expect_snapshot(define_model(x_by(extra, group), sleep))
})

test_that("print.def_model snapshot for formula", {
    expect_snapshot(define_model(extra ~ group, sleep))
})

test_that("print.def_model snapshot for pairwise", {
    expect_snapshot(define_model(pairwise(extra, ID), sleep))
})

test_that("print.test_lazy snapshot — default method", {
    expect_snapshot(
        sleep |>
            define_model(x_by(extra, group)) |>
            prepare_test(TTEST)
    )
})

test_that("print.test_lazy snapshot — via boot", {
    expect_snapshot(
        sleep |>
            define_model(x_by(extra, group)) |>
            prepare_test(TTEST) |>
            via("boot", n = 2000L)
    )
})
