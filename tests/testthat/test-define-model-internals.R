# ---- Quosure classifier ----

test_that("classify_quo() identifies bare symbol", {
    quo = rlang::quo(extra)
    cl = classify_quo(quo)

    expect_equal(cl$type, ":symbol")
})

test_that("classify_quo() identifies c() of all symbols", {
    quo = rlang::quo(c(x1, x2))
    cl = classify_quo(quo)

    expect_equal(cl$type, ":c_call")
})

test_that("classify_quo() rejects c() with non-symbol args", {
    quo = rlang::quo(c(x1, 1 + 2))
    cl = classify_quo(quo)

    expect_equal(cl$type, ":error")
})

test_that("classify_quo() identifies I() call", {
    quo = rlang::quo(I(rnorm(10)))
    cl = classify_quo(quo)

    expect_equal(cl$type, ":i_call")
})

test_that("classify_quo() identifies inlines() call", {
    quo = rlang::quo(inlines(rnorm(10), rnorm(10)))
    cl = classify_quo(quo)

    expect_equal(cl$type, ":inlines_call")
})

test_that("Do not call `tidyselect::` prefixes, `classify_quo()` will flag this", {
    quo = rlang::quo(tidyselect::starts_with("x"))
    cl = classify_quo(quo)

    expect_equal(cl$type, ":error")
})

test_that("classify_quo() marks arbitrary calls as :error", {
    quo = rlang::quo(sqrt(x))
    cl = classify_quo(quo)

    expect_equal(cl$type, ":error")
})

# ---- Quosure resolver ----

test_that("resolve_quo() resolves a symbol from a data frame", {
    df = data.frame(x = 1:5)
    quo = rlang::quo(x)
    result = resolve_quo(quo, data = df, role = "x")

    expect_equal(result$x, 1:5)
})

test_that("resolve_quo() resolves a symbol from calling environment", {
    x = 1:5
    quo = rlang::quo(x)
    result = resolve_quo(quo, data = NULL, role = "x")

    expect_equal(result$x, 1:5)
})

test_that("resolve_quo() errors with class when symbol missing from env", {
    quo = rlang::quo(nonexistent_var_xyz)
    expect_error(
        resolve_quo(quo, data = NULL, role = "x"),
        class = "check_missing_data"
    )
})

test_that("resolve_quo() resolves c() of symbols from data frame", {
    df = data.frame(x1 = 1:5, x2 = 6:10)
    quo = rlang::quo(c(x1, x2))
    result = resolve_quo(quo, data = df, role = "x")

    expect_equal(ncol(result), 2)
    expect_named(result, c("x1", "x2"))
})

test_that("resolve_quo() resolves c() of symbols from environment", {
    x1 = 1:5
    x2 = 6:10
    quo = rlang::quo(c(x1, x2))
    result = resolve_quo(quo, data = NULL, role = "x")

    expect_equal(ncol(result), 2)
    expect_named(result, c("x1", "x2"))
})

test_that("resolve_quo() resolves I() with user-supplied name", {
    quo = rlang::quo(I(score = rnorm(10)))
    result = resolve_quo(quo, data = NULL, role = "x")

    expect_named(result, "score")
    expect_length(result$score, 10)
})

test_that("resolve_quo() resolves I() with auto-name when unnamed", {
    quo = rlang::quo(I(rnorm(10)))
    result = resolve_quo(quo, data = NULL, role = "x", idx = 1L)

    expect_named(result, "xv1")
})

test_that("resolve_quo() errors on arbitrary call expression", {
    quo = rlang::quo(sqrt(x))
    expect_error(resolve_quo(quo, data = NULL, role = "x"))
})

test_that("resolve_quo() doesn't resolve expression when `data` is a data frame", {
    df = data.frame(xa = 1:5, xb = 6:10, y = 11:15)
    quo = rlang::quo(starts_with("x"))
    expect_error(resolve_quo(quo, data = df, role = "x"), class = "rlang_error")
})

test_that("resolve_quo() errors when tidyselect used without data", {
    quo = rlang::quo(tidyselect::starts_with("x"))
    expect_error(resolve_quo(quo, data = NULL, role = "x"))
})

# ---- Testing hard-coded "inlines" API ----

test_that("resolve_inlines() names columns from user-supplied names", {
    dm = define_model(
        x_by(inlines(a = rnorm(10), b = rnorm(10)), I(rep(c("x", "y"), 5))),
        NULL
    )
    expect_named(dm@processed$x_data, c("a", "b"))
})

test_that("resolve_inlines() auto-names unnamed columns by role and position", {
    dm = define_model(
        x_by(inlines(rnorm(10), rnorm(10)), I(rep(c("x", "y"), 5))),
        NULL
    )
    expect_named(dm@processed$x_data, c("xv1", "xv2"))
})

test_that("resolve_inlines() handles mixed named and unnamed", {
    dm = define_model(
        x_by(inlines(a = rnorm(10), rnorm(10)), I(rep(c("x", "y"), 5))),
        NULL
    )
    expect_named(dm@processed$x_data, c("a", "xv2"))
})

test_that("auto_name() produces role + 'v' + idx", {
    expect_equal(auto_name("x", 1L), "xv1")
    expect_equal(auto_name("group", 3L), "groupv3")
    expect_equal(auto_name("p", 2L), "pv2")
})

# ---- "Relational" function ----
## It has `two_vars_extract()` to automatically extract two arguments

test_that("two_vars_extract() returns x1_data and x2_data", {
    x_quo = rlang::quo(extra)
    g_quo = rlang::quo(group)
    result = two_vars_extract(x_quo, g_quo, data = sleep, role2 = "group")

    expect_named(result, c("x1_data", "x2_data"))
    expect_equal(result$x1_data$extra, sleep$extra)
    expect_equal(result$x2_data$group, sleep$group)
})

# ---- Extraction of "pairwise" data ----

test_that("pairwise_data_extract() returns var_names, pairs, data", {
    m = pairwise(speed, dist)
    result = pairwise_data_extract(m, data = cars)

    expect_named(result, c("var_names", "pairs", "data"))
    expect_all_true(result$var_names == c("speed", "dist"))
    # expect_equal(result$var_names, c("speed", "dist"))
})

test_that("pairwise_data_extract() respects direction", {
    df = data.frame(a = 1:5, b = 1:5, c = 1:5)
    m_lt = pairwise(a, b, c, direction = "lt")
    m_all = pairwise(a, b, c, direction = "all")

    r_lt = pairwise_data_extract(m_lt, data = df)
    r_all = pairwise_data_extract(m_all, data = df)

    expect_lt(length(r_lt$pairs), length(r_all$pairs))
})

# ---- inequality + pairs_generator -------------------------------------------------
## Generation of pairs uses lexicographic ordering

test_that("inequality() applies lt correctly", {
    expect_true(inequality(1, 2, "lt"))
    expect_false(inequality(2, 2, "lt"))
    expect_false(inequality(3, 2, "lt"))
})

test_that("inequality() applies lteq correctly", {
    expect_true(inequality(1, 2, "lteq"))
    expect_true(inequality(2, 2, "lteq"))
    expect_false(inequality(3, 2, "lteq"))
})

test_that("inequality() applies gt correctly", {
    expect_true(inequality(3, 2, "gt"))
    expect_false(inequality(2, 2, "gt"))
})

test_that("inequality() applies gteq correctly", {
    expect_true(inequality(2, 2, "gteq"))
    expect_true(inequality(3, 2, "gteq"))
    expect_false(inequality(1, 2, "gteq"))
})

test_that("inequality() applies eq correctly", {
    expect_true(inequality(2, 2, "eq"))
    expect_false(inequality(1, 2, "eq"))
})

test_that("inequality() applies neq correctly", {
    expect_true(inequality(1, 2, "neq"))
    expect_false(inequality(2, 2, "neq"))
})

test_that("inequality() applies all correctly (always TRUE)", {
    expect_true(inequality(1, 2, "all"))
    expect_true(inequality(2, 2, "all"))
    expect_true(inequality(3, 1, "all"))
})

test_that("inequality() stops on invalid direction", {
    expect_error(inequality(1, 2, "sideways"))
})

test_that("pairs_generator() with direction 'lt' gives n*(n-1)/2 pairs", {
    vars = c("a", "b", "c", "d")
    pairs = pairs_generator(vars, direction = "lt", simplify = TRUE)

    expect_length(pairs, 6) # 4 choose 2
})

test_that("pairs_generator() with direction 'all' gives n^2 pairs", {
    vars = c("a", "b", "c")
    pairs = pairs_generator(vars, direction = "all", simplify = TRUE)

    expect_length(pairs, 9)
})

test_that("pairs_generator() with direction 'eq' gives n self-pairs", {
    vars = c("a", "b", "c")
    pairs = pairs_generator(vars, direction = "eq", simplify = TRUE)

    expect_length(pairs, 3)
    expect_true(all(vapply(pairs, \(p) p[[1]] == p[[2]], logical(1))))
})

test_that("pairs_generator() with simplify = FALSE returns a data frame", {
    vars = c("a", "b")
    result = pairs_generator(vars, direction = "lt", simplify = FALSE)

    expect_s3_class(result, "data.frame")
    expect_named(result, c("x", "y"))
})

# ---- Core `define_model()` processor ----
# Internally, `define_model()` always relies on
# `model_processor()` given the data and the `model_id`

test_that("model_processor() for x_by returns x_data and group_data", {
    result = model_processor(x_by(extra, group), sleep)

    expect_named(result, c("x_data", "group_data"))
    expect_equal(result$x_data$extra, sleep$extra)
    expect_equal(result$group_data$group, sleep$group)
})

test_that("model_processor() for rel returns x_data and resp_data", {
    result = model_processor(rel(speed, dist), cars)

    expect_named(result, c("x_data", "resp_data"))
    expect_equal(result$x_data$speed, cars$speed)
    expect_equal(result$resp_data$dist, cars$dist)
})

test_that("model_processor() for pairwise returns var_names, pairs, data", {
    result = model_processor(pairwise(speed, dist), cars)

    expect_named(result, c("var_names", "pairs", "data"))
})

test_that("model_processor() for formula resolves variables from data", {
    result = model_processor(extra ~ group, sleep)

    expect_named(result, c("data", "vars", "formula"))
    expect_equal(result$vars, c("extra", "group"))
})

test_that("model_processor() for formula resolves variables from environment when data is NULL", {
    extra = sleep$extra
    group = sleep$group
    result = model_processor(extra ~ group, NULL)

    expect_equal(result$vars, c("extra", "group"))
    expect_equal(result$data$extra, extra)
})

# ---- vars_preview + format_quo_label ----
# This is only used to display info from `model_id` objects

test_that("vars_preview() returns a list with name and preview fields", {
    cols = list(x = 1:10, g = letters[1:10])
    result = vars_preview(cols)

    expect_length(result, 2)
    expect_named(result[[1]], c("name", "preview"))
    expect_equal(result[[1]]$name, "x")
    expect_match(result[[2]]$preview, "^<")
})

test_that("format_quo_label() returns symbol name for bare name", {
    quo = rlang::quo(extra)
    expect_equal(format_quo_label(quo), "extra")
})

test_that("format_quo_label() returns comma-joined names for c()", {
    quo = rlang::quo(c(x1, x2))
    expect_equal(format_quo_label(quo), "x1, x2")
})

test_that("format_quo_label() returns '<inline>' for I()", {
    quo = rlang::quo(I(rnorm(10)))
    expect_equal(format_quo_label(quo), "<inline>")
})

test_that("format_quo_label() returns '<inlines>' for inlines()", {
    quo = rlang::quo(inlines(rnorm(10), rnorm(10)))
    expect_equal(format_quo_label(quo), "<inlines>")
})
