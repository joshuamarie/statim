test_that("full TTEST pipeline works on sleep with x_by", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        conclude()

    expect_s3_class(result, "htest_spec")
    expect_s3_class(result, "ttest")
    expect_s3_class(result$data, "data.frame")
})

test_that("TTEST eager path with x_by works", {
    result = TTEST(x_by(extra, group), sleep)
    expect_s3_class(result, "htest_spec")
    expect_s3_class(result, "ttest_two")
    expect_equal(result$name, "T-Test")
})

test_that("TTEST returns test_spec when called with no model", {
    spec = TTEST()
    expect_s3_class(spec, "test_spec")
    expect_equal(spec$cls, "ttest")
})

test_that("ttest_def_two produces correct t-test result", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        conclude()

    ttest_obj = result$data$ttest[[1]]
    expect_s3_class(ttest_obj, "htest")
    expect_equal(names(result$data), c("group", "ttest"))
    expect_equal(result$data$group, "group")
})

test_that("ttest_def_two respects .paired argument", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        update(.paired = TRUE) |>
        conclude()

    expect_s3_class(result, "htest_spec")
    ttest_obj = result$data$ttest[[1]]
    expect_true(ttest_obj$parameter["df"] < 18)
})

test_that("ttest_def_two respects .alt argument", {
    result_greater = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        update(.alt = "greater") |>
        conclude()

    ttest_obj = result_greater$data$ttest[[1]]
    expect_equal(ttest_obj$alternative, "greater")
})

test_that("ttest_def_two errors when group has != 2 levels", {
    expect_error(
        iris |>
            define_model(x_by(Sepal.Length, Species)) |>
            prepare_test(TTEST) |>
            conclude()
    )
})

test_that("ttest boot variant pipeline works", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        via("boot", n = 100, seed = 42) |>
        conclude()

    expect_s3_class(result, "htest_spec")
    expect_s3_class(result, "ttest_two")
    expect_equal(result$data$n, 100)
    expect_length(result$data$boot_dist, 100)
    expect_named(result$data$ci, c("2.5%", "97.5%"))
})

test_that("ttest boot variant seed makes results reproducible", {
    run = function() {
        sleep |>
            define_model(x_by(extra, group)) |>
            prepare_test(TTEST) |>
            via("boot", n = 100, seed = 99) |>
            conclude()
    }
    r1 = run()
    r2 = run()
    expect_equal(r1$data$boot_dist, r2$data$boot_dist)
})

test_that("ttest permute variant pipeline works", {
    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        via("permute", n = 100, seed = 1) |>
        conclude()

    expect_s3_class(result, "htest_spec")
    expect_s3_class(result, "ttest_two")
    expect_equal(result$data$n, 100)
    expect_true(is.numeric(result$data$observed))
    expect_true(is.numeric(result$data$p.value))
    expect_gte(result$data$p.value, 0)
    expect_lte(result$data$p.value, 1)
    expect_length(result$data$null_dist, 100)
})

test_that("ttest permute variant seed makes results reproducible", {
    run = function() {
        sleep |>
            define_model(x_by(extra, group)) |>
            prepare_test(TTEST) |>
            via("permute", n = 100, seed = 7) |>
            conclude()
    }
    r1 = run()
    r2 = run()
    expect_equal(r1$data$observed, r2$data$observed)
    expect_equal(r1$data$null_dist, r2$data$null_dist)
})

test_that("ttest permute_rfast variant works when plugged externally", {
    skip_if_not_installed("Rfast2")
    on.exit(clear_htest_defs("ttest"), add = TRUE)

    plug_variant(
        TTEST, "permute_rfast",
        variant(
            fn = function(x, group_data, B = 999L) {
                rlang::check_installed("Rfast2", reason = "to run the Rfast2-backed permutation t-test")
                grp = as.character(group_data[[1]])
                lvls = unique(grp)
                if (length(lvls) != 2L) {
                    cli::cli_abort(c(
                        "Permutation t-test requires exactly 2 groups.",
                        "i" = "Found {length(lvls)} group{{?s}}."
                    ))
                }
                res = Rfast2::perm.ttest(
                    x = x[grp == lvls[[1]]],
                    y = x[grp == lvls[[2]]],
                    B = B
                )
                list(stat = res[["stat"]], p.value = res[["permutation p-value"]], B = B)
            },
            print = function(x, ...) {
                summary_data = tibble::tibble(
                    Statistic = round(x$data$stat, 4),
                    `p-value` = round(x$data$p.value, 4),
                    n_perms = x$data$B
                )
                cli::cat_line(cli::rule(center = "T-test Permutation", line = "="), "\n\n")
                tabstats::table_default(summary_data)
                cat("\n\n")
                invisible(x)
            }
        )
    )

    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        via("permute_rfast", B = 199) |>
        conclude()

    expect_s3_class(result, "htest_spec")
    expect_s3_class(result, "ttest_two")
    expect_true(is.numeric(result$data$stat))
    expect_true(is.numeric(result$data$p.value))
    expect_equal(result$data$B, 199)
})

test_that("ttest_def_formula two-sample pipeline works", {
    result = sleep |>
        define_model(extra ~ group) |>
        prepare_test(TTEST) |>
        conclude()

    expect_s3_class(result, "ttest_formula")
    expect_named(result$data, c("type", "group", "ttest"))
    expect_equal(result$data$type[[1]], "two sample")
})

test_that("ttest_def_formula eager path works", {
    result = TTEST(extra ~ group, sleep)
    expect_s3_class(result, "ttest_formula")
})

test_that("ttest_def_formula one-sample detection works", {
    result = TTEST(extra ~ 1, sleep)
    expect_s3_class(result, "ttest_formula")
    expect_equal(result$data$type[[1]], "one sample")
})

test_that("ttest_def_formula errors when group has != 2 levels", {
    expect_error(
        iris |>
            define_model(Sepal.Length ~ Species) |>
            prepare_test(TTEST) |>
            conclude()
    )
})

# ---- plug_variant / swap_variant ----

test_that("plug_variant adds new variant to TTEST", {
    on.exit(clear_htest_defs("ttest"), add = TRUE)

    plug_variant(
        TTEST, "custom_boot",
        variant(
            fn = function(x, group_data, n = 50L) {
                list(n = n, result = "custom")
            }
        )
    )

    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        via("custom_boot") |>
        conclude()

    expect_s3_class(result, "htest_spec")
    expect_equal(result$data$result, "custom")
})

test_that("plug_variant errors if name already exists", {
    on.exit(clear_htest_defs("ttest"), add = TRUE)

    plug_variant(TTEST, "custom_v", variant(fn = function(x) x))
    expect_error(
        plug_variant(TTEST, "custom_v", variant(fn = function(x) x)),
        regexp = "already exists"
    )
})

test_that("swap_variant replaces existing variant", {
    on.exit(clear_htest_defs("ttest"), add = TRUE)

    plug_variant(TTEST, "swap_me", variant(fn = function(x, group_data) list(v = 1L)))
    swap_variant(TTEST, "swap_me", variant(fn = function(x, group_data) list(v = 2L)))

    result = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        via("swap_me") |>
        conclude()

    expect_equal(result$data$v, 2L)
})

test_that("plug_variant and swap_variant error on default", {
    expect_error(
        plug_variant(TTEST, "default", variant(fn = function(x) x)),
        regexp = "frozen"
    )
    expect_error(
        swap_variant(TTEST, "default", variant(fn = function(x) x)),
        regexp = "frozen"
    )
})

test_that("clear_htest_defs removes user variants", {
    plug_variant(TTEST, "temp_v", variant(fn = function(x) x))
    clear_htest_defs("ttest")

    lazy = sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST)

    expect_error(via(lazy, "temp_v"))
})

# ---- Pairwise T-test ----

test_that("ttest_def_pairwise basic pipeline works on iris", {
    result = iris |>
        define_model(pairwise(Sepal.Length, Sepal.Width, Petal.Length)) |>
        prepare_test(TTEST) |>
        conclude()

    expect_s3_class(result, "htest_spec")
    expect_s3_class(result, "ttest_pairwise")
    expect_named(result$data, c("a", "b", "ttest"))
    expect_equal(nrow(result$data), 3L)
    expect_s3_class(result$data$ttest[[1]], "htest")
})

test_that("ttest_def_pairwise produces correct pair labels", {
    result = iris |>
        define_model(pairwise(Sepal.Length, Sepal.Width, Petal.Length)) |>
        prepare_test(TTEST) |>
        conclude()

    expect_equal(result$data$a, c("Sepal.Length", "Petal.Length", "Petal.Length"))
    expect_equal(result$data$b, c("Sepal.Width", "Sepal.Length", "Sepal.Width"))
})

test_that("ttest_def_pairwise works with tidyselect helpers", {
    result = iris |>
        define_model(pairwise(starts_with(c("Se", "Pe")))) |>
        prepare_test(TTEST) |>
        conclude()

    expect_s3_class(result, "ttest_pairwise")
    expect_equal(nrow(result$data), 6L)
})

test_that("ttest_def_pairwise scalar .mu is recycled across all pairs", {
    result = iris |>
        define_model(pairwise(Sepal.Length, Sepal.Width, Petal.Length)) |>
        prepare_test(TTEST) |>
        update(.mu = 1) |>
        conclude()

    result_default = iris |>
        define_model(pairwise(Sepal.Length, Sepal.Width, Petal.Length)) |>
        prepare_test(TTEST) |>
        conclude()

    expect_equal(result$data$ttest, result_default$data$ttest)
})

test_that("ttest_def_pairwise per-variable .mu shifts null hypothesis correctly", {
    result = iris |>
        define_model(pairwise(Sepal.Length, Sepal.Width)) |>
        prepare_test(TTEST) |>
        update(.mu = c(5, 3)) |>
        conclude()

    expect_equal(result$data$ttest[[1]]$null.value[["difference in means"]], 2)
})

test_that("ttest_def_pairwise .mu of wrong length errors clearly", {
    expect_error(
        iris |>
            define_model(pairwise(Sepal.Length, Sepal.Width, Petal.Length)) |>
            prepare_test(TTEST) |>
            update(.mu = c(1, 2)) |>
            conclude(),
        regexp = "\\.mu.*must be length 1 or length 3"
    )
})

test_that("ttest_def_pairwise length-4 .mu works without error", {
    expect_no_error(
        iris |>
            define_model(pairwise(starts_with(c("Se", "Pe")))) |>
            prepare_test(TTEST) |>
            update(.mu = c(5, 3, 4, 2)) |>
            conclude()
    )
})

test_that("ttest_def_pairwise respects .alt argument", {
    result = iris |>
        define_model(pairwise(Sepal.Length, Sepal.Width)) |>
        prepare_test(TTEST) |>
        update(.alt = "greater") |>
        conclude()

    expect_equal(result$data$ttest[[1]]$alternative, "greater")
})

test_that("ttest_def_pairwise respects .ci argument", {
    result = iris |>
        define_model(pairwise(Sepal.Length, Sepal.Width)) |>
        prepare_test(TTEST) |>
        update(.ci = 0.99) |>
        conclude()

    ci = result$data$ttest[[1]]$conf.int
    expect_equal(attr(ci, "conf.level"), 0.99)
})

test_that("ttest_def_pairwise eager path works", {
    result = TTEST(pairwise(Sepal.Length, Sepal.Width, Petal.Length), iris)
    expect_s3_class(result, "ttest_pairwise")
    expect_equal(nrow(result$data), 3L)
})
