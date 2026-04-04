test_that("classify_quo returns ':symbol' for a bare name", {
    q = rlang::quo(myvar)
    result = classify_quo(q)
    expect_equal(result$type, ":symbol")
    expect_true(is.symbol(result$expr))
})

test_that("classify_quo returns ':c_call' for c() of bare names", {
    q = rlang::quo(c(a, b, c))
    result = classify_quo(q)
    expect_equal(result$type, ":c_call")
})

test_that("classify_quo returns ':i_call' for I() expression", {
    q = rlang::quo(I(rnorm(10)))
    result = classify_quo(q)
    expect_equal(result$type, ":i_call")
})

test_that("classify_quo returns ':inlines_call' for inlines() expression", {
    q = rlang::quo(inlines(rnorm(10), rnorm(10)))
    result = classify_quo(q)
    expect_equal(result$type, ":inlines_call")
})

test_that("classify_quo returns ':tidyselect' for a helper call", {
    q = rlang::quo(starts_with("x"))
    result = classify_quo(q)
    expect_equal(result$type, ":tidyselect")
})

test_that("classify_quo returns ':error' for a bare call without I()", {
    q = rlang::quo(rnorm(10))
    result = classify_quo(q)
    expect_equal(result$type, ":error")
})

test_that("classify_quo returns ':error' for c() containing non-symbols", {
    q = rlang::quo(c(a, I(rnorm(10))))
    result = classify_quo(q)
    expect_equal(result$type, ":error")
})

test_that("resolve_quo resolves a bare symbol from a data frame", {
    q = rlang::quo(extra)
    result = resolve_quo(q, data = sleep, role = "x")
    expect_s3_class(result, "data.frame")
    expect_named(result, "extra")
})

test_that("resolve_quo resolves a bare symbol from the environment", {
    myvar = 1:10
    q = rlang::quo(myvar)
    result = resolve_quo(q, data = NULL, role = "x")
    expect_s3_class(result, "data.frame")
    expect_named(result, "myvar")
    expect_equal(result[[1]], 1:10)
})

test_that("resolve_quo resolves c() of names from a data frame", {
    q = rlang::quo(c(extra, group))
    result = resolve_quo(q, data = sleep, role = "x")
    expect_named(result, c("extra", "group"))
})

test_that("resolve_quo resolves a named I() call", {
    q = rlang::quo(I(score = rnorm(10)))
    result = resolve_quo(q, data = NULL, role = "x")
    expect_named(result, "score")
    expect_length(result[[1]], 10)
})

test_that("resolve_quo auto-names an unnamed I() call using role", {
    q = rlang::quo(I(rnorm(10)))
    result = resolve_quo(q, data = NULL, role = "x", idx = 1L)
    expect_named(result, "xv1")
})

test_that("resolve_quo errors on bare call without I()", {
    q = rlang::quo(rnorm(10))
    expect_error(resolve_quo(q, data = NULL, role = "x"), class = "rlang_error")
})

test_that("resolve_quo errors on tidyselect without data", {
    q = rlang::quo(starts_with("x"))
    expect_error(resolve_quo(q, data = NULL, role = "x"), class = "rlang_error")
})

test_that("resolve_quo errors with check_missing_data when symbol not found", {
    q = rlang::quo(doesnotexist)
    expect_error(
        resolve_quo(q, data = NULL, role = "x"),
        class = "check_missing_data"
    )
})

# ---- two_vars_extract --------------------------------------------------------

test_that("two_vars_extract works with a data frame", {
    mid = x_by(extra, group)
    result = two_vars_extract(mid, sleep)
    expect_named(result, c("x1_data", "x2_data"))
    expect_s3_class(result$x1_data, "data.frame")
    expect_s3_class(result$x2_data, "data.frame")
    expect_named(result$x1_data, "extra")
    expect_named(result$x2_data, "group")
})

test_that("two_vars_extract errors when not exactly 2 args", {
    bad = structure(list(rlang::quo(a)), class = c("x_by", "model_id"))
    expect_error(two_vars_extract(bad, sleep))
})

# ---- pairwise_data_extract ---------------------------------------------------

test_that("pairwise_data_extract works with a data frame", {
    df = data.frame(a = 1:5, b = 6:10, c = 11:15)
    pw = pairwise(a, b, c)
    result = pairwise_data_extract(pw, df)
    expect_named(result, c("var_names", "pairs", "data"))
    expect_equal(result$var_names, c("a", "b", "c"))
    expect_length(result$pairs, 3)
    expect_s3_class(result$data, "data.frame")
    expect_named(result$data, c("a", "b", "c"))
})

test_that("pairwise_data_extract produces correct pairs", {
    df = data.frame(a = 1:3, b = 4:6)
    pw = pairwise(a, b)
    result = pairwise_data_extract(pw, df)
    expect_length(result$pairs, 1)
    expect_equal(result$pairs[[1]], c("a", "b"))
})

test_that("pairwise_data_extract works with I() inline data", {
    set.seed(1)
    pw = pairwise(I(rnorm(10)), I(rnorm(10)))
    result = pairwise_data_extract(pw, NULL)
    expect_named(result, c("var_names", "pairs", "data"))
    expect_equal(result$var_names, c("pv1", "pv2"))
    expect_length(result$pairs, 1)
})
