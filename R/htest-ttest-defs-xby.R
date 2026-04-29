ttest_def_two = test_define(
    model_type = "x_by",
    impl_class = "ttest_two",
    impl = agendas(
        base = baseline(
            # ---- Default implementation ----
            fn = function(x, group_data, .paired = FALSE, .mu = 0, .alt = "two.sided", .ci = 0.95) {
                tests = lapply(names(group_data), function(grp_name) {
                    grp = as.character(group_data[[grp_name]])
                    lvls = unique(grp)

                    if (length(lvls) != 2L) {
                        cli::cli_abort(c(
                            "Two-sample t-test requires exactly 2 groups.",
                            "i" = "Found {length(lvls)} group{{?s}} in {.val {grp_name}}."
                        ))
                    }

                    res = stats::t.test(
                        x = x[grp == lvls[[1]]],
                        y = x[grp == lvls[[2]]],
                        paired = .paired,
                        mu = .mu,
                        alternative = .alt,
                        conf.level = .ci
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
        ),
        boot = variant(
            # ---- Bootstrapping ----
            # ---- variant: boot ----
            fn = function(x, group_data, .ci = 0.95, n = 1000L, seed = NULL) {
                if (!is.null(seed)) set.seed(seed)

                grp = as.character(group_data[[1]])
                lvls = unique(grp)

                idx1 = which(grp == lvls[[1]])
                idx2 = which(grp == lvls[[2]])

                boot_dist = replicate(n, {
                    b1 = x[sample(idx1, replace = TRUE)]
                    b2 = x[sample(idx2, replace = TRUE)]
                    mean(b1) - mean(b2)
                })

                ci = quantile(
                    boot_dist,
                    c((1 - .ci) / 2, 1 - (1 - .ci) / 2)
                )

                list(boot_dist = boot_dist, ci = ci, n = n)
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
                    style = tabstats::sm_style(sep = ":  "),
                    center_table = TRUE
                )
                cat("\n\n")
                invisible(x)
            }
        ),
        permute = variant(
            # ---- Permutation test ----
            # ---- variant: permute ----
            fn = function(x, group_data, n = 1000L, seed = NULL) {
                if (!is.null(seed)) set.seed(seed)

                grp = as.character(group_data[[1]])
                lvls = unique(grp)

                obs = mean(x[grp == lvls[[1]]]) -
                    mean(x[grp == lvls[[2]]])

                null_dist = replicate(n, {
                    perm = sample(x)
                    mean(perm[grp == lvls[[1]]]) -
                        mean(perm[grp == lvls[[2]]])
                })

                list(
                    observed = obs,
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
    )
)
