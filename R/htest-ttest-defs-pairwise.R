# ---- Pairwise T-test ----
ttest_def_pairwise = test_define(
    model_type = "pairwise",
    impl_class = "ttest_pairwise",
    fun_args = fun_args(
        .paired = FALSE,
        .mu = 0,
        .alt = "two.sided",
        .ci = 0.95
    ),
    vars = list(
        pairs = function(p) p$pairs,
        data = function(p) p$data,
        var_names = function(p) p$var_names
    ),
    run = function(self) {
        pairs = ic_pull(self, "pairs")
        data = ic_pull(self, "data")
        var_names = ic_pull(self, "var_names")
        n_vars = length(var_names)
        n_pairs = length(pairs)

        paired = ic_arg(self, ".paired")
        mu = ic_arg(self, ".mu")
        alt = ic_arg(self, ".alt")
        ci_level = ic_arg(self, ".ci")

        if (length(mu) == 1L) {
            mu = rep(mu, n_vars)
        } else if (length(mu) != n_vars) {
            cli::cli_abort(c(
                "{.arg .mu} must be length 1 or length {n_vars} (one per variable).",
                "i" = "Variables: {.val {var_names}}.",
                "x" = "Got length {length(mu)}."
            ))
        }
        names(mu) = var_names

        tests = lapply(seq_along(pairs), function(i) {
            a = pairs[[i]][[1]]
            b = pairs[[i]][[2]]

            res = stats::t.test(
                x = data[[a]],
                y = data[[b]],
                paired = paired,
                mu = mu[[a]] - mu[[b]],
                alternative = alt,
                conf.level = ci_level
            )

            list(a = a, b = b, ttest = res)
        })

        tibble::tibble(
            a = vapply(tests, `[[`, character(1), "a"),
            b = vapply(tests, `[[`, character(1), "b"),
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
            pair_lbl = paste0(dat$a[[i]], " vs ", dat$b[[i]])

            stat_row = dplyr::transmute(
                td,
                pair = pair_lbl,
                diff = estimate,
                `t-stat` = statistic,
                pval = p.value
            )
            ci_row = dplyr::transmute(
                td,
                pair = pair_lbl,
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
