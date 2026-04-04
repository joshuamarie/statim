test_that("define_model.data.frame returns def_model (pipe-friendly)", {
    result = sleep |> define_model(x_by(extra, group))
    expect_s3_class(result, "def_model")
    expect_named(result, c("model_id", "processed"))
})

test_that("define_model.data.frame stores model_id and processed", {
    result = sleep |> define_model(x_by(extra, group))
    expect_s3_class(result$model_id, "x_by")
    expect_named(result$processed, c("x_data", "group_data"))
})

test_that("define_model.model_id returns def_model (model-ID first)", {
    result = define_model(x_by(extra, group), sleep)
    expect_s3_class(result, "def_model")
    expect_s3_class(result$model_id, "x_by")
})

test_that("define_model works with formula in pipe-friendly style", {
    result = sleep |> define_model(extra ~ group)
    expect_s3_class(result, "def_model")
    expect_equal(class(result$model_id)[[1]], "formula")
})

test_that("define_model works with formula in model-ID first style", {
    result = define_model(extra ~ group, sleep)
    expect_s3_class(result, "def_model")
    expect_equal(class(result$model_id)[[1]], "formula")
})

test_that("define_model.data.frame processes processed field correctly", {
    result = sleep |> define_model(extra ~ group)
    expect_named(result$processed, c("data", "vars", "formula"))
    expect_equal(result$processed$vars, c("extra", "group"))
})
