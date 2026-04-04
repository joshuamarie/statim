test_that("inequality returns correct results for all directions", {
    expect_true(inequality(1, 2, "lt"))
    expect_false(inequality(2, 1, "lt"))
    expect_false(inequality(2, 2, "lt"))

    expect_true(inequality(1, 2, "lteq"))
    expect_true(inequality(2, 2, "lteq"))
    expect_false(inequality(3, 2, "lteq"))

    expect_true(inequality(2, 1, "gt"))
    expect_false(inequality(1, 2, "gt"))
    expect_false(inequality(2, 2, "gt"))

    expect_true(inequality(2, 1, "gteq"))
    expect_true(inequality(2, 2, "gteq"))
    expect_false(inequality(1, 2, "gteq"))

    expect_true(inequality(2, 2, "eq"))
    expect_false(inequality(1, 2, "eq"))

    expect_true(inequality(1, 2, "neq"))
    expect_false(inequality(2, 2, "neq"))

    expect_true(inequality(1, 2, "all"))
    expect_true(inequality(2, 2, "all"))
    expect_true(inequality(2, 1, "all"))
})

test_that("inequality errors on invalid direction", {
    expect_error(inequality(1, 2, "invalid"))
})

test_that("pairs_generator produces correct number of pairs with direction lt", {
    pairs = pairs_generator(c("a", "b", "c"), direction = "lt")
    expect_type(pairs, "list")
    expect_length(pairs, 3)  # (a,b), (a,c), (b,c)
    expect_equal(pairs[[1]], c("a", "b"))
    expect_equal(pairs[[2]], c("a", "c"))
    expect_equal(pairs[[3]], c("b", "c"))
})

test_that("pairs_generator with 2 vars and direction lt gives 1 pair", {
    pairs = pairs_generator(c("a", "b"), direction = "lt")
    expect_length(pairs, 1)
    expect_equal(pairs[[1]], c("a", "b"))
})

test_that("pairs_generator with direction all gives n^2 pairs", {
    pairs = pairs_generator(c("a", "b"), direction = "all")
    expect_length(pairs, 4)  # (a,a), (a,b), (b,a), (b,b)
})

test_that("pairs_generator with simplify = FALSE returns a data frame", {
    result = pairs_generator(c("a", "b", "c"), direction = "lt", simplify = FALSE)
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 3)
})

test_that("pairs_generator with direction eq gives diagonal pairs", {
    pairs = pairs_generator(c("a", "b"), direction = "eq")
    expect_length(pairs, 2)  # (a,a), (b,b)
    expect_equal(pairs[[1]], c("a", "a"))
    expect_equal(pairs[[2]], c("b", "b"))
})
