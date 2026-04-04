make_ctx = function(
    processed = list(x = c(1, 2, 3), grp = c("a", "b", "a")),
    args = list(.mu = 5),
    extractors = list(
        x = function(p) p$x,
        grp = function(p) p$grp
    ),
    fa = fun_args(.ci = 0.95, .mu = 0),
    method_args = list(n = 100L, seed = 42L),
    claims = NULL
) {
    infer_context(
        processed = processed,
        args = args,
        extractors = extractors,
        fun_args = fa,
        claims = claims,
        method_args = method_args
    )
}

test_that("infer_context constructs S7 object", {
    ctx = make_ctx()
    expect_true(inherits(ctx, "statim::infer_context"))
})

test_that("ic_pull returns the extracted value", {
    ctx = make_ctx()
    expect_equal(ic_pull(ctx, "x"), c(1, 2, 3))
    expect_equal(ic_pull(ctx, "grp"), c("a", "b", "a"))
})

test_that("ic_pull errors when role has no extractor", {
    ctx = make_ctx()
    expect_error(ic_pull(ctx, "missing_role"))
})

test_that("ic_name returns column name for data frame output", {
    ctx = infer_context(
        processed = list(df_var = data.frame(my_col = 1:3)),
        extractors = list(df_role = function(p) p$df_var)
    )
    expect_equal(ic_name(ctx, "df_role"), "my_col")
})

test_that("ic_name returns role string for non-data-frame output", {
    ctx = make_ctx()
    expect_equal(ic_name(ctx, "x"), "x")
})

test_that("ic_arg returns user-supplied arg first", {
    ctx = make_ctx(args = list(.mu = 99))
    # User supplied .mu = 99, fun_args has .mu = 0 — user wins
    expect_equal(ic_arg(ctx, ".mu"), 99)
})

test_that("ic_arg falls back to fun_args default", {
    ctx = make_ctx(args = list())
    # No user arg; fun_args has .ci = 0.95
    expect_equal(ic_arg(ctx, ".ci"), 0.95)
})

test_that("ic_arg falls back to provided default when no fun_args entry", {
    ctx = make_ctx(args = list())
    result = ic_arg(ctx, ".missing", default = "fallback")
    expect_equal(result, "fallback")
})

test_that("ic_arg errors for required arg not supplied", {
    ctx = infer_context(
        processed = list(),
        args = list(),
        fun_args = fun_args(~.required_arg)
    )
    expect_error(ic_arg(ctx, ".required_arg"))
})

test_that("ic_method_arg returns method-level argument", {
    ctx = make_ctx()
    expect_equal(ic_method_arg(ctx, "n"), 100L)
    expect_equal(ic_method_arg(ctx, "seed"), 42L)
})

test_that("ic_method_arg returns default when arg missing", {
    ctx = make_ctx(method_args = list())
    expect_equal(ic_method_arg(ctx, "n", default = 999L), 999L)
    expect_null(ic_method_arg(ctx, "n"))
})

test_that("ic_claim returns NULL when claims is NULL", {
    ctx = make_ctx(claims = NULL)
    expect_null(ic_claim(ctx, "anything"))
})

test_that("ic_claim returns named claim", {
    ctx = infer_context(
        processed = list(),
        claims = list(H0 = "mu = 0")
    )
    expect_equal(ic_claim(ctx, "H0"), "mu = 0")
})

test_that("infer_context uses empty list for method_args when NULL supplied", {
    ctx = infer_context(processed = list(), method_args = NULL)
    expect_equal(ctx@method_args, list())
})
