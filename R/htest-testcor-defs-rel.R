cor_test_rel = test_define(
    model_type = "rel",
    impl_class = "cortest_rel",
    fun_args = fun_args(
        .cor_type = "pearson",
        .alt = "two.sided",
        .ci = 0.95
    ),
    vars = list(
        x_data = function(m) m$x_data,
        resp_data = function(m) m$resp_data
    ),
    run = function(self) {
        x_data = ic_pull(self, "x_data")
        resp_data = ic_pull(self, "resp_data")

        if (length(resp_data) != 1L) {
            cli::cli_abort(c(
                "{.arg resp} must be a single variable.",
                "i" = "Got {length(resp_data)} variable{?s}: {.val {names(resp_data)}}.",
                "i" = "Use a bare name or {.fn I} for a single response variable."
            ))
        }

        resp_name = names(resp_data)

        method = ic_arg(self, ".cor_type")
        alt = ic_arg(self, ".alt")
        ci_level = ic_arg(self, ".ci")

        tests = lapply(names(x_data), function(x_name) {
            res = stats::cor.test(
                x = x_data[[x_name]],
                y = resp_data[[1]],
                method = method,
                alternative = alt,
                conf.level = ci_level
            )
            list(x = x_name, resp = resp_name, cortest = res)
        })

        list(
            res = tibble::tibble(
                x = vapply(tests, `[[`, character(1), "x"),
                resp = vapply(tests, `[[`, character(1), "resp"),
                cortest = lapply(tests, `[[`, "cortest")
            ),
            ci_level = ci_level
        )
    },
    print = function(x, ...) {
        rlang::check_installed(
            c("broom", "purrr"),
            reason = "to retrieve correlation test results and re-store in a data frame"
        )

        dat = x$data$res

        tidy_rows = lapply(seq_len(nrow(dat)), function(i) {
            td = broom::tidy(dat$cortest[[i]])
            ct = dat$cortest[[i]]
            ci = ct$conf.int
            pair_lbl = paste0(dat$resp[[i]], " ~ ", dat$x[[i]])

            has_ci = !is.null(ci)
            ci_level = if (has_ci) attr(ci, "conf.level") else x$data$ci_level
            lo_name = paste0("lower_", ci_level * 100)
            up_name = paste0("upper_", ci_level * 100)

            stat_row = dplyr::transmute(
                td,
                pair = pair_lbl,
                estimate = estimate,
                stat = statistic,
                pval = p.value
            )
            ci_row = dplyr::transmute(
                td,
                pair = pair_lbl,
                !!lo_name := if (has_ci) conf.low else NA_real_,
                !!up_name := if (has_ci) conf.high else NA_real_
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

        has_any_ci = any(vapply(dat$cortest, function(ct) !is.null(ct$conf.int), logical(1)))
        if (has_any_ci) {
            cli::cat_line(cli::rule(left = "Confidence Interval", line = "-"), "\n")
            tabstats::table_default(ci_out)
            cat("\n\n")
        }

        invisible(x)
    }
)
