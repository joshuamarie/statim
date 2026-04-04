test_that("c_quo_vars_extract handles a c() call expression", {
    q = rlang::quo(c(a, b, c))
    result = c_quo_vars_extract(q)
    expect_type(result, "list")
    expect_length(result, 3)
})

test_that("c_quo_vars_extract handles a single symbol", {
    q = rlang::quo(myvar)
    result = c_quo_vars_extract(q)
    expect_length(result, 1)
    expect_equal(as.character(result[[1]]), "myvar")
})

test_that("c_quo_vars_extract errors on non-symbol non-c() expression", {
    q = rlang::quo(a + b)
    expect_error(c_quo_vars_extract(q))
})

test_that("c_vars_label converts symbols to character labels", {
    syms = list(quote(alpha), quote(beta), quote(gamma))
    result = c_vars_label(syms)
    expect_equal(result, c("alpha", "beta", "gamma"))
})

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
    # Construct a fake model_id with wrong number of elements
    bad = structure(list(rlang::quo(a)), class = c("x_by", "model_id"))
    expect_error(two_vars_extract(bad, sleep))
})

test_that("pairwise_data_extract works with a data frame", {
    df = data.frame(a = 1:5, b = 6:10, c = 11:15)
    pw = pairwise(a, b, c)
    result = pairwise_data_extract(pw, df)

    expect_named(result, c("var_names", "pairs", "data"))
    expect_equal(result$var_names, c("a", "b", "c"))
    expect_length(result$pairs, 3)  # 3 lt-pairs from 3 vars
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
