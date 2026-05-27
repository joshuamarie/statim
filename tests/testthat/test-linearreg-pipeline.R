test_that("lm_to_lm_object() returns an lm_object from a fitted lm", {
    fit = lm(dist ~ speed, data = cars)
    obj = lm_to_lm_object(fit)

    expect_s7_class(obj, statim::lm_object)
})

test_that("lm_to_lm_object() errors on non-lm input", {
    expect_error(
        lm_to_lm_object(list(x = 1)),
        class = "rlang_error"
    )
})

test_that("lm_to_lm_object() coefficients df has correct columns", {
    fit = lm(dist ~ speed, data = cars)
    obj = lm_to_lm_object(fit)

    expect_named(obj@coefficients, c("term", "estimate", "std_error", "statistic", "p_value"))
})

test_that("lm_to_lm_object() fit_summary has correct columns", {
    fit = lm(dist ~ speed, data = cars)
    obj = lm_to_lm_object(fit)

    expect_named(
        obj@fit_summary,
        c("r_squared", "adj_r_squared", "sigma", "df_residual", "n_obs")
    )
})

test_that("lm_to_lm_object() residuals match base lm residuals", {
    fit = lm(dist ~ speed, data = cars)
    obj = lm_to_lm_object(fit)

    expect_equal(unname(obj@residuals), unname(fit$residuals), tolerance = 1e-8)
})

test_that("LINEAR_REG() eager via rel() returns stat_infer_spec", {
    result = LINEAR_REG(rel(speed, dist), cars)

    expect_s7_class(result, stat_infer_spec)
})

test_that("LINEAR_REG() eager result data is an lm_object", {
    result = LINEAR_REG(rel(speed, dist), cars)

    expect_s7_class(result@data, statim::lm_object)
})

test_that("LINEAR_REG() eager via rel() matches base lm() coefficients", {
    result = LINEAR_REG(rel(speed, dist), cars)
    base = lm(dist ~ speed, data = cars)

    expect_equal(
        result@data@coefficients$estimate,
        unname(coef(base)),
        tolerance = 1e-8
    )
})

test_that("LINEAR_REG() eager via formula returns stat_infer_spec", {
    result = LINEAR_REG(dist ~ speed, cars)

    expect_s7_class(result, stat_infer_spec)
})

test_that("LINEAR_REG() eager via formula matches rel() result", {
    rel_result = LINEAR_REG(rel(speed, dist), cars)
    formula_result = LINEAR_REG(dist ~ speed, cars)

    expect_equal(
        rel_result@data@coefficients$estimate,
        formula_result@data@coefficients$estimate,
        tolerance = 1e-8
    )
})

test_that("rel() pipeline returns cld_exec", {
    result = cars |>
        define_model(rel(speed, dist)) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    expect_s7_class(result, cld_exec)
})

test_that("rel() pipeline data is an lm_object", {
    result = cars |>
        define_model(rel(speed, dist)) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    expect_s7_class(result@data, statim::lm_object)
})

test_that("rel() pipeline result matches base lm() r_squared", {
    result = cars |>
        define_model(rel(speed, dist)) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    base = summary(lm(dist ~ speed, data = cars))

    expect_equal(result@data@fit_summary$r_squared, base$r.squared, tolerance = 1e-8)
})

test_that("rel() pipeline stat_name is 'Linear Regression'", {
    result = cars |>
        define_model(rel(speed, dist)) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    expect_equal(result@cld_meta$stat_name, "Linear Regression")
})

test_that("rel() pipeline print returns invisibly", {
    result = cars |>
        define_model(rel(speed, dist)) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    expect_invisible(print(result))
})

test_that("formula pipeline returns cld_exec", {
    result = cars |>
        define_model(dist ~ speed) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    expect_s7_class(result, cld_exec)
})

test_that("formula pipeline data is an lm_object", {
    result = cars |>
        define_model(dist ~ speed) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    expect_s7_class(result@data, statim::lm_object)
})

test_that("formula and rel() pipelines produce identical coefficients", {
    rel_result = cars |>
        define_model(rel(speed, dist)) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    formula_result = cars |>
        define_model(dist ~ speed) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    expect_equal(
        rel_result@data@coefficients$estimate,
        formula_result@data@coefficients$estimate,
        tolerance = 1e-8
    )
})

test_that("rel() pipeline with multiple predictors runs without error", {
    expect_no_error(
        LifeCycleSavings |>
            define_model(rel(c(pop15, pop75, dpi, ddpi), sr)) |>
            prepare_model(LINEAR_REG) |>
            conclude()
    )
})

test_that("formula pipeline with multiple predictors returns correct n coefficients", {
    result = LifeCycleSavings |>
        define_model(sr ~ pop15 + pop75 + dpi + ddpi) |>
        prepare_model(LINEAR_REG) |>
        conclude()

    expect_equal(nrow(result@data@coefficients), 5L)
})
