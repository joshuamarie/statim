test_that("model_processor.x_by returns x_data and group_data", {
    mid = x_by(extra, group)
    result = model_processor(mid, sleep)

    expect_named(result, c("x_data", "group_data"))
    expect_s3_class(result$x_data, "data.frame")
    expect_s3_class(result$group_data, "data.frame")
    expect_named(result$x_data, "extra")
    expect_named(result$group_data, "group")
    expect_equal(nrow(result$x_data), nrow(sleep))
})

test_that("model_processor.rel returns x_data and resp_data", {
    mid = rel(speed, dist)
    result = model_processor(mid, cars)

    expect_named(result, c("x_data", "resp_data"))
    expect_named(result$x_data, "speed")
    expect_named(result$resp_data, "dist")
    expect_equal(nrow(result$x_data), nrow(cars))
})

test_that("model_processor.formula returns data, vars, formula", {
    f = extra ~ group
    result = model_processor(f, sleep)

    expect_named(result, c("data", "vars", "formula"))
    expect_identical(result$data, sleep)
    expect_equal(result$vars, c("extra", "group"))
    expect_equal(result$formula, f)
})

test_that("model_processor.formula with NULL data evaluates in formula env", {
    x = c(1.0, -1.0, 0.5)
    y = c("a", "b", "a")
    f = x ~ y
    result = model_processor(f, NULL)

    expect_s3_class(result$data, "data.frame")
    expect_named(result$data, c("x", "y"))
})

test_that("model_processor.pairwise returns var_names, pairs, data", {
    df = data.frame(a = 1:5, b = 6:10, c = 11:15)
    pw = pairwise(a, b, c)
    result = model_processor(pw, df)

    expect_named(result, c("var_names", "pairs", "data"))
    expect_equal(result$var_names, c("a", "b", "c"))
    expect_length(result$pairs, 3)
})
