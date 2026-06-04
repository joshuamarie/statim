# baseline() ---------------------------------------------------------------

test_that("baseline() errors if fn is not a function", {
    expect_error(
        baseline(fn = "not_a_function"),
        class = "rlang_error"
    )
})

test_that("baseline() errors if fn does not start with .proc", {
    expect_error(
        baseline(fn = function(x) x),
        class = "rlang_error"
    )
})

test_that("baseline() errors if print is not NULL or a function", {
    expect_error(
        baseline(fn = function(.proc) .proc, print = "bad"),
        class = "rlang_error"
    )
})

test_that("baseline() returns a baseline S7 object", {
    b = baseline(fn = function(.proc) .proc)
    expect_s7_class(b, statim:::baseline)
})

test_that("baseline() stores fn and NULL print by default", {
    fn = function(.proc) .proc
    b = baseline(fn = fn)
    expect_identical(b@fn, fn)
    expect_null(b@print)
})

test_that("baseline() stores a custom print function", {
    fn = function(.proc) .proc
    pf = function(x, ...) invisible(x)
    b = baseline(fn = fn, print = pf)
    expect_identical(b@print, pf)
})

# variant() ----------------------------------------------------------------

test_that("variant() errors if fn is not a function", {
    expect_error(
        variant(fn = 42L),
        class = "rlang_error"
    )
})

test_that("variant() errors if fn does not start with .proc", {
    expect_error(
        variant(fn = function(x) x),
        class = "rlang_error"
    )
})

test_that("variant() errors if print is not NULL or a function", {
    expect_error(
        variant(fn = function(.proc) .proc, print = TRUE),
        class = "rlang_error"
    )
})

test_that("variant() returns a variant S7 object", {
    v = variant(fn = function(.proc) .proc)
    expect_s7_class(v, statim:::variant)
})

test_that("variant() stores fn and NULL print by default", {
    fn = function(.proc) .proc
    v = variant(fn = fn)
    expect_identical(v@fn, fn)
    expect_null(v@print)
})

# agendas() ----------------------------------------------------------------

test_that("agendas() errors if base is missing", {
    expect_error(agendas(), class = "rlang_error")
})

test_that("agendas() errors if base is not a baseline object", {
    v = variant(fn = function(.proc) .proc)
    expect_error(
        agendas(base = v),
        class = "rlang_error"
    )
})

test_that("agendas() errors if a variant argument is unnamed (all unnamed)", {
    b = baseline(fn = function(.proc) .proc)
    v = variant(fn = function(.proc) .proc)
    expect_error(
        agendas(base = b, v),
        class = "rlang_error"
    )
})

test_that("agendas() errors if a variant argument is unnamed (mixed named/unnamed)", {
    b = baseline(fn = function(.proc) .proc)
    v1 = variant(fn = function(.proc) .proc)
    v2 = variant(fn = function(.proc) .proc)
    expect_error(
        agendas(base = b, ok = v1, v2),
        class = "rlang_error"
    )
})

test_that("agendas() errors if a named argument is not a variant", {
    b = baseline(fn = function(.proc) .proc)
    expect_error(
        agendas(base = b, bad = "not_a_variant"),
        class = "rlang_error"
    )
})

test_that("agendas() returns an agendas S3 object", {
    b = baseline(fn = function(.proc) .proc)
    ag = agendas(base = b)
    expect_s3_class(ag, "agendas")
})

test_that("agendas() stores base and empty variants list", {
    b = baseline(fn = function(.proc) .proc)
    ag = agendas(base = b)
    expect_s7_class(ag$base, statim:::baseline)
    expect_length(ag$variants, 0L)
})

test_that("agendas() stores a named variant", {
    b = baseline(fn = function(.proc) .proc)
    v = variant(fn = function(.proc) .proc)
    ag = agendas(base = b, alt = v)
    expect_named(ag$variants, "alt")
    expect_s7_class(ag$variants[["alt"]], statim:::variant)
})

test_that("agendas() stores multiple named variants", {
    b = baseline(fn = function(.proc) .proc)
    v1 = variant(fn = function(.proc) .proc)
    v2 = variant(fn = function(.proc, n = 10L) list(n = n))
    ag = agendas(base = b, v_one = v1, v_two = v2)
    expect_named(ag$variants, c("v_one", "v_two"))
})
