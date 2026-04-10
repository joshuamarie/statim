# ---- t-test for `x_by()` argument ----
ttest_def_two = test_define(
    model_type = "x_by",
    impl_class = "ttest_two",
    fun_args = fun_args(
        .paired = FALSE,
        .mu = 0,
        .alt = "two.sided",
        .ci = 0.95
    ),
    vars = list(
        x = function(p) p$x_data[[1]],
        group = function(p) p$group_data
    ),
    run = function(self) {
        resp = ic_pull(self, "x")
        group_df = ic_pull(self, "group")
        ci_level = ic_arg(self, ".ci")
        paired = ic_arg(self, ".paired")
        mu = ic_arg(self, ".mu")
        alt = ic_arg(self, ".alt")

        tests = lapply(names(group_df), function(grp_name) {
            grp = as.character(group_df[[grp_name]])
            lvls = unique(grp)

            if (length(lvls) != 2L) {
                cli::cli_abort(c(
                    "Two-sample t-test requires exactly 2 groups.",
                    "i" = "Found {length(lvls)} group{{?s}} in {.val {grp_name}}."
                ))
            }

            res = stats::t.test(
                x = resp[grp == lvls[[1]]],
                y = resp[grp == lvls[[2]]],
                paired = paired,
                mu = mu,
                alternative = alt,
                conf.level = ci_level
            )

            list(group = grp_name, ttest = res)
        })

        tibble::tibble(
            group = vapply(tests, `[[`, character(1), "group"),
            ttest = lapply(tests, `[[`, "ttest")
        )
    },
    print = function(x, ...) {
        rlang::check_installed(
            c("broom", "purrr"),
            reason = "to retrieve t-test results and re-store it in a data frame"
        )

        dat = x$data

        tidy_rows = lapply(seq_len(nrow(dat)), function(i) {
            td = broom::tidy(dat$ttest[[i]])
            ci_level = attr(purrr::pluck(dat$ttest[[i]], "conf.int"), "conf.level")
            lo_name = paste0("lower_", ci_level * 100)
            up_name = paste0("upper_", ci_level * 100)

            stat_row = dplyr::transmute(
                td,
                groups = dat$group[[i]],
                diff = estimate,
                `t-stat` = statistic,
                pval = p.value
            )
            ci_row = dplyr::transmute(
                td,
                groups = dat$group[[i]],
                !!lo_name := conf.low,
                !!up_name := conf.high
            )
            list(stat = stat_row, ci = ci_row)
        })

        stat_out = dplyr::bind_rows(lapply(tidy_rows, `[[`, "stat"))
        ci_out = dplyr::bind_rows(lapply(tidy_rows, `[[`, "ci"))

        pval_styler = function(x) {
            x_num = suppressWarnings(as.numeric(x$value))
            if (is.na(x_num) || x_num > 0.05) {
                cli::style_italic(x$value)
            } else if (x_num > 0.01) {
                cli::col_red(x$value)
            } else {
                cli::style_bold("<0.001")
            }
        }

        cli::cat_line(cli::rule(left = "Summary", line = "-"), "\n")
        tabstats::table_default(
            stat_out,
            style_columns = tabstats::td_style(pval = pval_styler)
        )
        cat("\n\n")

        cli::cat_line(cli::rule(left = "Confidence Interval", line = "-"), "\n")
        tabstats::table_default(ci_out)
        cat("\n\n")

        invisible(x)
    }
)

# ---- Bootstrapped t-test for `x_by()` args ----
# ---- for `x_by()` ----
ttest_def_boot = test_define(
    model_type = "x_by",
    impl_class = "ttest_boot",
    method = method_spec(
        "boot",
        method_type = "bootstrap",
        defaults = list(n = 1000L, seed = NULL)
    ),
    vars = list(
        x = function(p) p$x_data[[1]],
        group = function(p) p$group_data[[1]]
    ),
    run = function(self) {
        n = ic_method_arg(self, "n")
        seed = ic_method_arg(self, "seed")

        if (!is.null(seed)) set.seed(seed)

        grp = as.character(ic_pull(self, "group"))
        resp = ic_pull(self, "x")
        lvls = unique(grp)

        idx1 = which(grp == lvls[[1]])
        idx2 = which(grp == lvls[[2]])

        boot_dist = replicate(n, {
            b1 = resp[sample(idx1, replace = TRUE)]
            b2 = resp[sample(idx2, replace = TRUE)]
            mean(b1) - mean(b2)
        })

        ci = quantile(
            boot_dist,
            c(
                (1 - ic_arg(self, ".ci", 0.95)) / 2,
                1 - (1 - ic_arg(self, ".ci", 0.95)) / 2
            )
        )

        list(
            boot_dist = boot_dist,
            ci = ci,
            n = n
        )
    },
    print = function(x, ...) {
        ci = round(x$data$ci, 4)
        summary_data = tibble::tibble(
            names = c("CI", "n_reps"),
            vals = c(paste0("[", ci[[1]], ", ", ci[[2]], "]"), x$data$n)
        )

        cli::cat_line(cli::rule(center = "Bootstrapped T-test", line = "="), "\n\n")
        cli::cat_line(cli::rule(left = "Summary", line = "-"), "\n")
        tabstats::table_summary(
            summary_data,
            style = tabstats::sm_style(
                sep = ":  "
            ),
            center_table = TRUE
        )
        cat("\n\n")
        invisible(x)
    }
)

# ---- Default permutation t-test for `x_by()` ----
# ---- for `x_by()` ----
ttest_def_permute = test_define(
    model_type = "x_by",
    impl_class = "ttest_permute",
    engine = "default",
    method = method_spec(
        "permute",
        method_type = "replicate",
        defaults = list(n = 1000L, seed = NULL)
    ),
    vars = list(
        x = function(p) p$x_data[[1]],
        group = function(p) p$group_data[[1]]
    ),
    run = function(self) {
        n = ic_method_arg(self, "n")
        seed = ic_method_arg(self, "seed")

        if (!is.null(seed)) set.seed(seed)

        grp = as.character(ic_pull(self, "group"))
        resp = ic_pull(self, "x")
        lvls = unique(grp)

        obs = mean(resp[grp == lvls[[1]]]) -
            mean(resp[grp == lvls[[2]]])

        null_dist = replicate(n, {
            perm = sample(resp)
            mean(perm[grp == lvls[[1]]]) -
                mean(perm[grp == lvls[[2]]])
        })

        list(
            observed  = obs,
            null_dist = null_dist,
            p.value = mean(abs(null_dist) >= abs(obs)),
            n = n
        )
    },
    print = function(x, ...) {
        summary_data = tibble::tibble(
            Statistic = round(x$data$observed, 4),
            `p-value` = round(x$data$p.value, 4),
            n_perms = x$data$n
        )

        pval_styler = function(x) {
            x_num = suppressWarnings(as.numeric(x$value))
            if (is.na(x_num) || x_num > 0.05) {
                cli::style_italic(x$value)
            } else if (x_num > 0.01) {
                cli::col_red(x$value)
            } else {
                cli::style_bold("<0.001")
            }
        }

        cli::cat_line(cli::rule(center = "T-test Permutation", line = "="), "\n\n")
        cli::cat_line(cli::rule(left = "Summary", line = "-"), "\n")
        tabstats::table_default(
            summary_data,
            style_columns = tabstats::td_style(`p-value` = pval_styler)
        )
        cat("\n\n")
        invisible(x)
    }
)

# ---- {Rfast}'s permutation t-test for `x_by()` ----
# ---- for `x_by()` ----
ttest_def_permute_rfast = test_define(
    model_type = "x_by",
    impl_class = "ttest_permute_rfast",
    engine = "rfast",
    method = method_spec(
        "permute",
        method_type = "replicate",
        defaults = list(B = 999L)
    ),
    vars = list(
        x = function(p) p$x_data[[1]],
        group = function(p) p$group_data[[1]]
    ),
    run = function(self) {
        rlang::check_installed(
            "Rfast2",
            reason = "to run the Rfast2-backed permutation t-test engine"
        )

        B = ic_method_arg(self, "B")
        grp = as.character(ic_pull(self, "group"))
        resp = ic_pull(self, "x")
        lvls = unique(grp)

        if (length(lvls) != 2L) {
            cli::cli_abort(c(
                "Permutation t-test requires exactly 2 groups.",
                "i" = "Found {length(lvls)} group{{?s}}."
            ))
        }

        x = resp[grp == lvls[[1]]]
        y = resp[grp == lvls[[2]]]

        # Rfast2 requires numeric vectors, no NAs
        res = Rfast2::perm.ttest(x = x, y = y, B = B)

        list(
            stat = res[["stat"]],
            p.value = res[["permutation p-value"]],
            B = B
        )
    },
    print = function(x, ...) {
        summary_data = tibble::tibble(
            Statistic = round(x$data$stat, 4),
            `p-value` = round(x$data$p.value, 4),
            n_perms = x$data$B
        )

        pval_styler = function(x) {
            x_num = suppressWarnings(as.numeric(x$value))
            if (is.na(x_num) || x_num > 0.05) {
                cli::style_italic(x$value)
            } else if (x_num > 0.01) {
                cli::col_red(x$value)
            } else {
                cli::style_bold("<0.001")
            }
        }

        cli::cat_line(cli::rule(center = "T-test Permutation", line = "="), "\n\n")
        cli::cat_line(cli::rule(left = "Summary", line = "-"), "\n")
        tabstats::table_default(
            summary_data,
            style_columns = tabstats::td_style(`p-value` = pval_styler)
        )
        cat("\n\n")

        # cli::cli_text("{.field Statistic}            : {round(x$data$stat, 4)}")
        # cli::cli_text("{.field p-value (permutation)}: {round(x$data$p.value, 4)}")
        # cli::cli_text("{.field Permutations}         : {x$data$B}")
        invisible(x)
    }
)
