test_that("%||% returns left value when non-NULL", {
    expect_equal(1L %||% 2L, 1L)
    expect_equal("a" %||% "b", "a")
    expect_equal(FALSE %||% TRUE, FALSE)
    expect_equal(0 %||% 99, 0)
})

test_that("%||% returns right value when left is NULL", {
    expect_equal(NULL %||% 2L, 2L)
    expect_equal(NULL %||% "fallback", "fallback")
    expect_equal(NULL %||% list(1, 2), list(1, 2))
})
