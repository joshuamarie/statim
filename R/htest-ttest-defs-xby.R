ttest_def_two = test_define(
    model_type = x_by,
    # impl_class = "ttest_two",
    impl = agendas(
        base = baseline(
            # ---- Default implementation ----
            fn = function(.proc, .paired = FALSE, .mu = 0, .alt = "two.sided", .ci = 0.95) {
                x = .proc$x_data[[1]]
                group_data = .proc$group_data

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
                    c("broom", "purrr", "dplyr"),
                    reason = "to retrieve t-test results and re-store it in a data frame"
                )

                dat = x@data

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
                        !!lo_name := ifelse(is.infinite(conf.low), "-Inf", conf.low),
                        !!up_name := ifelse(is.infinite(conf.high), "Inf", conf.high)
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
            fn = function(.proc, .ci = 0.95, n = 1000L, seed = NULL) {
                x = .proc$x_data[[1]]
                group_data = .proc$group_data

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
                ci = round(x@data$ci, 4)
                summary_data = tibble::tibble(
                    names = c("CI", "n_reps"),
                    vals = c(paste0("[", ci[[1]], ", ", ci[[2]], "]"), x@data$n)
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
            fn = function(.proc, n = 1000L, seed = NULL) {
                x = .proc$x_data[[1]]
                group_data = .proc$group_data

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
                    Statistic = round(x@data$observed, 4),
                    `p-value` = round(x@data$p.value, 4),
                    n_perms = x@data$n
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
        ),
        weighted = variant(
            # ---- Weighted t-test ----
            # ---- variant: "weighted" ----
            fn = function(.proc, .mu = 0, .ci = 0.95, .w = NULL, .op = "==") {
                x = .proc$x_data[[1]]
                group_data = .proc$group_data

                grp_name = names(group_data)[[1]]
                grp = as.character(group_data[[grp_name]])
                lvls = unique(grp)

                if (length(lvls) != 2L) {
                    cli::cli_abort(c(
                        "Contrast t-test requires exactly 2 groups.",
                        "i" = "Found {length(lvls)} group{{?s}} in {.val {grp_name}}."
                    ))
                }

                x1 = x[grp == lvls[[1]]]
                x2 = x[grp == lvls[[2]]]

                n1 = length(x1)
                n2 = length(x2)
                xbar1 = mean(x1)
                xbar2 = mean(x2)
                s1 = stats::var(x1)
                s2 = stats::var(x2)

                coefs = if (is.null(.w)) {
                    c(1, -1)
                } else {
                    coef_nms = names(.w)
                    c(
                        .w[coef_nms == lvls[[1]]],
                        .w[coef_nms == lvls[[2]]]
                    )
                }

                c1 = coefs[[1]]
                c2 = coefs[[2]]

                est_val = c1 * xbar1 + c2 * xbar2
                se = sqrt(c1^2 * s1 / n1 + c2^2 * s2 / n2)
                tstat = (est_val - .mu) / se
                df = (c1^2 * s1 / n1 + c2^2 * s2 / n2)^2 /
                    ((c1^2 * s1 / n1)^2 / (n1 - 1) + (c2^2 * s2 / n2)^2 / (n2 - 1))

                p.value = switch(
                    .op,
                    "==" = 2 * stats::pt(-abs(tstat), df = df),
                    ">=" = stats::pt(-tstat, df = df),
                    "<=" = stats::pt(tstat, df = df),
                    ">" = stats::pt(-tstat, df = df),
                    "<" = stats::pt(tstat, df = df),
                    "!=" = 2 * stats::pt(-abs(tstat), df = df)
                )

                alpha = 1 - .ci
                ci = switch(
                    .op,
                    "==" = ,
                    "!=" = {
                        t_crit = stats::qt(1 - alpha / 2, df = df)
                        c(.mu - t_crit * se, .mu + t_crit * se)
                    },
                    ">=" = ,
                    ">" = {
                        t_crit = stats::qt(1 - alpha, df = df)
                        c(.mu - t_crit * se, Inf)
                    },
                    "<=" = ,
                    "<" = {
                        t_crit = stats::qt(1 - alpha, df = df)
                        c(-Inf, .mu + t_crit * se)
                    }
                )
                names(ci) = c("lower", "upper")

                list(
                    group = grp_name,
                    est = est_val,
                    coefs = coefs,
                    tstat = tstat,
                    df = df,
                    p.value = p.value,
                    ci = ci,
                    ci_level = .ci,
                    mu = .mu
                )
            },
            print = function(x, ...) {
                dat = x@data
                ci_level = dat$ci_level * 100
                lo_name = paste0("lower_", ci_level)
                up_name = paste0("upper_", ci_level)

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

                stat_out = tibble::tibble(
                    groups = dat$group,
                    est = round(dat$est, 4),
                    `t-stat` = round(dat$tstat, 4),
                    df = round(dat$df, 2),
                    pval = round(dat$p.value, 4)
                )

                fmt_ci = function(val) {
                    if (is.infinite(val)) ifelse(val > 0, "Inf", "-Inf") else round(val, 4)
                }

                ci_out = tibble::tibble(
                    groups = dat$group,
                    !!lo_name := fmt_ci(dat$ci[["lower"]]),
                    !!up_name := fmt_ci(dat$ci[["upper"]])
                )

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
    ),
    # ---- Modify Modelled Hypothesis ----
    compatible_params = list(MU),
    claim_translator = claim_translate(
        default = map_claim(
            .mu = function(claim, processed) {
                resolved = claim_contrast_coefs(claim)
                coefs = resolved$coefs

                valid_two_sample = length(coefs) == 2L && identical(sort(unname(coefs)), c(-1, 1))
                valid_one_sample = length(coefs) == 1L && coefs == 1

                if (!valid_two_sample && !valid_one_sample) {
                    cli::cli_abort(c(
                        "T-test only supports simple mean differences.",
                        "i" = "Found weighted contrast: {.val {coefs}}.",
                        "i" = "Use {.code via(\"contrast\")} for weighted hypotheses."
                    ))
                }

                resolved$scalar
            },
            .alt = function(claim, processed = NULL) {
                switch(claim@op,
                       "==" = , "!=" = "two.sided",
                       ">=" = , ">" = "greater",
                       "<=" = , "<" = "less"
                )
            }
        ),
        weighted = map_claim(
            .mu = function(claim, processed) claim_contrast_coefs(claim)$scalar,
            .op = function(claim, processed) claim_contrast_coefs(claim)$op,
            .w = function(claim, processed) claim_contrast_coefs(claim)$coefs
        )
    )
)
