test_that("x_by returns correct class", {
    result = x_by(extra, group)
    expect_s3_class(result, "x_by")
    expect_s3_class(result, "model_id")
    expect_equal(class(result), c("x_by", "model_id"))
})

test_that("x_by captures variable names as quosures", {
    result = x_by(extra, group)
    expect_length(result, 2)
})

test_that("rel returns correct class", {
    result = rel(speed, dist)
    expect_s3_class(result, "rel")
    expect_s3_class(result, "model_id")
    expect_equal(class(result), c("rel", "model_id"))
})

test_that("rel has x and resp components", {
    result = rel(speed, dist)
    expect_named(result, c("x", "resp"))
})

test_that("pairwise returns correct class", {
    result = pairwise(a, b, c)
    expect_s3_class(result, "pairwise")
    expect_s3_class(result, "model_id")
    expect_equal(class(result), c("pairwise", "model_id"))
})

test_that("pairwise stores direction parameter", {
    result_lt = pairwise(a, b, c, direction = "lt")
    expect_equal(result_lt$direction, "lt")

    result_all = pairwise(a, b, c, direction = "all")
    expect_equal(result_all$direction, "all")
})

test_that("pairwise default direction is lt", {
    result = pairwise(a, b)
    expect_equal(result$direction, "lt")
})

test_that("model_id_class attaches correct classes", {
    obj = list(x = 1)
    result = model_id_class(obj, "myclass")
    expect_equal(class(result), c("myclass", "model_id"))
    expect_equal(result$x, 1)
})

test_that("model_id_class works with any object", {
    result = model_id_class(list(), "custom_id")
    expect_s3_class(result, "model_id")
    expect_s3_class(result, "custom_id")
})
