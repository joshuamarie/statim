# Single-model path: Type I (sequential) ANOVA — one term per row plus Residuals.
# Multi-model path: incremental F-test across nested models.
#
# Reference values are computed from `stats::anova()` and `stats::lm()` directly.

# ---- helpers ----

pipeline_conclude = function(data, formula) {
    data |>
        define_model(formula) |>
        prepare_model(LINEAR_REG) |>
        conclude()
}

pipeline_lazy = function(data, formula) {
    data |>
        define_model(formula) |>
        prepare_model(LINEAR_REG)
}

ref_anova_single = function(data, formula) {
    stats::anova(stats::lm(formula, data = data))
}

ref_anova_multi = function(data, ...) {
    formulas = list(...)
    fits = lapply(formulas, function(f) stats::lm(f, data = data))
    do.call(stats::anova, fits)
}

is_cld_anova = function(x) S7::S7_inherits(x, cld_anova)

# ---- Single-model: output structure ----

test_that("single-model anova() returns a cld_anova", {
    anova_simple = pipeline_conclude(cars, dist ~ speed) |> anova()
    expect_true(is_cld_anova(anova_simple))
})

test_that("single-model anova() table has correct columns", {
    tbl = pipeline_conclude(cars, dist ~ speed) |> anova() |> (\(x) x@data)()
    expect_named(tbl, c("term", "df", "ss", "ms", "f_value", "p_value"))
})

test_that("single-model anova() has one row per term plus Residuals", {
    tbl = pipeline_conclude(cars, dist ~ speed) |> anova() |> (\(x) x@data)()
    expect_equal(nrow(tbl), 2L)  # It concatenates "residuals" part, i.e. speed + Residuals
    expect_equal(tbl$term, c("speed", "Residuals"))
})

test_that("single-model anova() last row is Residuals with NA f_value and p_value", {
    tbl = pipeline_conclude(cars, dist ~ speed) |> anova() |> (\(x) x@data)()
    expect_equal(tbl$term[nrow(tbl)], "Residuals")
    expect_true(is.na(tbl$f_value[nrow(tbl)]))
    expect_true(is.na(tbl$p_value[nrow(tbl)]))
})

test_that("single-model anova() cld_meta method is 'Type I'", {
    result = pipeline_conclude(cars, dist ~ speed) |> anova()
    expect_equal(result@cld_meta$method, "Type I")
})

# ---- Single-model: Numerical correctness ----

test_that("single-model anova() SS matches stats::anova()", {
    ref = ref_anova_single(cars, dist ~ speed)
    tbl = pipeline_conclude(cars, dist ~ speed) |> anova() |> (\(x) x@data)()

    expect_equal(tbl$ss[1L], ref$`Sum Sq`[1L], tolerance = 1e-6)
    expect_equal(tbl$ss[2L], ref$`Sum Sq`[2L], tolerance = 1e-6)
})

test_that("single-model anova() F value matches stats::anova()", {
    ref = ref_anova_single(cars, dist ~ speed)
    tbl = pipeline_conclude(cars, dist ~ speed) |> anova() |> (\(x) x@data)()

    expect_equal(tbl$f_value[1L], ref$`F value`[1L], tolerance = 1e-6)
})

test_that("single-model anova() p-value matches stats::anova()", {
    ref = ref_anova_single(cars, dist ~ speed)
    tbl = pipeline_conclude(cars, dist ~ speed) |> anova() |> (\(x) x@data)()

    expect_equal(tbl$p_value[1L], ref$`Pr(>F)`[1L], tolerance = 1e-8)
})

test_that("single-model anova() residual df matches stats::anova()", {
    ref = ref_anova_single(cars, dist ~ speed)
    tbl = pipeline_conclude(cars, dist ~ speed) |> anova() |> (\(x) x@data)()

    expect_equal(tbl$df[nrow(tbl)], ref$Df[nrow(ref)])
})

test_that("single-model anova() SS values sum to total SS", {
    tbl = pipeline_conclude(cars, dist ~ speed) |> anova() |> (\(x) x@data)()
    fit = stats::lm(dist ~ speed, data = cars)
    y = fit$fitted.values + fit$residuals
    tss = sum((y - mean(y))^2)

    expect_equal(sum(tbl$ss), tss, tolerance = 1e-6)
})

# ---- Single-model: multiple predictors --------------------------------------

test_that("single-model anova() with dot formula has correct number of rows", {
    tbl = pipeline_conclude(mtcars, mpg ~ .) |> anova() |> (\(x) x@data)()
    # 10 predictors + Residuals
    expect_equal(nrow(tbl), 10L + 1L)
})

test_that("single-model anova() with dot formula SS matches stats::anova()", {
    ref = ref_anova_single(mtcars, mpg ~ .)
    tbl = pipeline_conclude(mtcars, mpg ~ .) |> anova() |> (\(x) x@data)()

    expect_equal(tbl$ss[-nrow(tbl)], ref$`Sum Sq`[-nrow(ref)], tolerance = 1e-6)
})

test_that("single-model anova() with dot formula p-values match stats::anova()", {
    ref = ref_anova_single(mtcars, mpg ~ .)
    tbl = pipeline_conclude(mtcars, mpg ~ .) |> anova() |> (\(x) x@data)()

    expect_equal(tbl$p_value[-nrow(tbl)], ref$`Pr(>F)`[-nrow(ref)], tolerance = 1e-8)
})

# ---- Single-model: pipeline variants ----

test_that("single-model `anova()` works from model_lazy (no conclude)", {
    anova_simple = pipeline_lazy(cars, dist ~ speed) |> anova()

    expect_true(is_cld_anova(anova_simple))
})

test_that("single-model `anova()` lazy and conclude paths produce equal SS", {
    tbl_lazy = pipeline_lazy(cars, dist ~ speed) |> anova() |> (\(x) x@data)()
    tbl_exec = pipeline_conclude(cars, dist ~ speed) |> anova() |> (\(x) x@data)()

    expect_equal(tbl_lazy$ss, tbl_exec$ss, tolerance = 1e-10)
})

# ---- Single-model: error when `x_mat` is missing ----

# test_that("single-model `anova()` errors clearly when `x_mat` is missing", {
#     fit = stats::lm(dist ~ speed, data = cars)
#     coef_tbl = summary(fit)$coefficients
#     rss = sum(fit$residuals^2)
#     df_res = fit$df.residual
#
#     obj = class_lm_object(
#         terms = fit$terms,
#         fitted = unname(fit$fitted.values),
#         residuals = unname(fit$residuals),
#         beta = coef_tbl[, 1],
#         std_beta = coef_tbl[, 2],
#         df_residual = df_res,
#         deviance = rss,
#         dispersion = rss / df_res,
#         family = "gaussian"
#         # x_mat deliberately omitted
#     )
#
#     expect_error(anova(obj))
#     # expect_error(anova(obj), class = "rlang_error")
# })

# ---- Multi-model: output structure ----

test_that("multi-model anova() returns a cld_anova", {
    mod1 = pipeline_conclude(LifeCycleSavings, sr ~ 1)
    mod2 = pipeline_conclude(LifeCycleSavings, sr ~ pop15)
    anova_multiple = anova(mod1, mod2)

    expect_true(is_cld_anova(anova_multiple))
})

test_that("multi-model anova() table has correct columns", {
    mod1 = pipeline_conclude(LifeCycleSavings, sr ~ 1)
    mod2 = pipeline_conclude(LifeCycleSavings, sr ~ pop15)
    tbl = anova(mod1, mod2)@data

    expect_named(tbl, c("model", "res_df", "deviance", "df", "dev_diff", "f_value", "p_value"))
})

test_that("multi-model anova() has one row per model", {
    mod1 = pipeline_conclude(LifeCycleSavings, sr ~ 1)
    mod2 = pipeline_conclude(LifeCycleSavings, sr ~ pop15)
    mod3 = pipeline_conclude(LifeCycleSavings, sr ~ pop15 + pop75)
    tbl = anova(mod1, mod2, mod3)@data

    expect_equal(nrow(tbl), 3L)
})

# ---- Multi-model: numerical correctness ----

test_that("multi-model `anova()` F value matches `stats::anova()`", {
    ref = ref_anova_multi(
        LifeCycleSavings,
        sr ~ 1,
        sr ~ pop15
    )
    mod1 = pipeline_conclude(LifeCycleSavings, sr ~ 1)
    mod2 = pipeline_conclude(LifeCycleSavings, sr ~ pop15)
    tbl = anova(mod1, mod2)@data

    expect_equal(tbl$f_value[2L], ref$F[2L], tolerance = 1e-6)
})

test_that("multi-model anova() p-value matches stats::anova()", {
    ref = ref_anova_multi(
        LifeCycleSavings,
        sr ~ 1,
        sr ~ pop15
    )
    mod1 = pipeline_conclude(LifeCycleSavings, sr ~ 1)
    mod2 = pipeline_conclude(LifeCycleSavings, sr ~ pop15)
    tbl = anova(mod1, mod2)@data

    expect_equal(tbl$p_value[2L], ref$`Pr(>F)`[2L], tolerance = 1e-8)
})

test_that("multi-model anova() first row has NA df, dev_diff, f_value, p_value", {
    mod1 = pipeline_conclude(LifeCycleSavings, sr ~ 1)
    mod2 = pipeline_conclude(LifeCycleSavings, sr ~ pop15)
    tbl = anova(mod1, mod2)@data

    expect_true(is.na(tbl$df[1L]))
    expect_true(is.na(tbl$dev_diff[1L]))
    expect_true(is.na(tbl$f_value[1L]))
    expect_true(is.na(tbl$p_value[1L]))
})

# ---- Multi-model: `write_models()` pipeline ----

test_that("write_models() |> anova() matches direct anova() call", {
    ref_tbl = {
        mod1 = pipeline_conclude(LifeCycleSavings, sr ~ 1)
        mod2 = pipeline_conclude(LifeCycleSavings, sr ~ pop15)
        mod3 = pipeline_conclude(LifeCycleSavings, sr ~ pop15 + pop75)
        anova(mod1, mod2, mod3)@data
    }

    wm_tbl = LifeCycleSavings |>
        write_models(
            f1 = sr ~ 1,
            f2 = sr ~ pop15,
            f3 = sr ~ pop15 + pop75
        ) |>
        prepare_model(LINEAR_REG) |>
        anova() |>
        (\(x) x@data)()

    expect_equal(unname(wm_tbl[-1, ]$f_value), ref_tbl[-1, ]$f_value, tolerance = 1e-10)
    expect_equal(unname(wm_tbl[-1, ]$p_value), ref_tbl[-1, ]$p_value, tolerance = 1e-10)
})

test_that("write_models() anova() uses model labels as row names", {
    tbl = LifeCycleSavings |>
        write_models(
            null = sr ~ 1,
            main = sr ~ pop15
        ) |>
        prepare_model(LINEAR_REG) |>
        anova() |>
        (\(x) x@data)()

    expect_equal(tbl$model, c("null", "main"))
})

# ---- Multi-model: error cases ----

test_that("multi-model anova() errors on mismatched response", {
    mod1 = pipeline_conclude(LifeCycleSavings, sr ~ pop15)
    mod2 = pipeline_conclude(LifeCycleSavings, pop15 ~ sr)

    expect_error(anova(mod1, mod2), class = "rlang_error")
})

test_that("multi-model anova() errors when non-cld_exec passed", {
    mod1 = pipeline_conclude(LifeCycleSavings, sr ~ 1)

    expect_error(anova(mod1, 42), class = "rlang_error")
})

test_that("multi-model anova() errors when non-model_lazy passed to lazy path", {
    mod1 = pipeline_lazy(LifeCycleSavings, sr ~ 1)

    expect_error(anova(mod1, 42), class = "rlang_error")
})
