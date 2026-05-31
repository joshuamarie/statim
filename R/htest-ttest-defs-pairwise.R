ttest_def_pairwise = test_define(
    model_type = pairwise,
    impl = agendas(
        base = baseline(
            fn = function(.proc, .paired = FALSE, .mu = 0, .alt = "two.sided", .ci = 0.95) {
                var_names = .proc$var_names
                pairs = .proc$pairs
                data = .proc$data
                direction = .proc$direction %||% "lt"
                is_one_sample = direction == "eq"

                n_vars = length(var_names)

                if (length(.mu) == 1L) {
                    .mu = rep(.mu, n_vars)
                } else if (length(.mu) != n_vars) {
                    cli::cli_abort(c(
                        "{.arg .mu} must be length 1 or length {n_vars} (one per variable).",
                        "i" = "Variables: {.val {var_names}}.",
                        "x" = "Got length {length(.mu)}."
                    ))
                }
                names(.mu) = var_names

                tests = lapply(seq_along(pairs), function(i) {
                    a = pairs[[i]][[1]]
                    b = pairs[[i]][[2]]

                    res = if (is_one_sample) {
                        stats::t.test(
                            x = data[[a]],
                            mu = .mu[[a]],
                            alternative = .alt,
                            conf.level = .ci
                        )
                    } else {
                        stats::t.test(
                            x = data[[a]],
                            y = data[[b]],
                            paired = .paired,
                            mu = .mu[[a]] - .mu[[b]],
                            alternative = .alt,
                            conf.level = .ci
                        )
                    }

                    list(a = a, b = b, ttest = res)
                })

                class_ttest_pairwise(
                    var1 = vapply(tests, function(x) x[["a"]], character(1)),
                    var2 = vapply(tests, function(x) x[["b"]], character(1)),
                    est = vapply(tests, function(t) {
                        est = t$ttest$estimate
                        if (length(est) == 2L) est[[1L]] - est[[2L]] else est[[1L]]
                    }, numeric(1)),
                    df = vapply(tests, function(t) t$ttest$parameter[["df"]], numeric(1)),
                    t_stat = vapply(tests, function(t) t$ttest$statistic[["t"]], numeric(1)),
                    p_value = vapply(tests, function(t) t$ttest$p.value, numeric(1)),
                    method_name = unique(
                        vapply(tests, function(t) t$ttest$method, character(1))
                    )
                )
            }
        )
    )
)

class_ttest_pairwise = S7::new_class(
    "class_ttest_pairwise",
    properties = list(
        var1 = S7::class_character,
        var2 = S7::class_character,
        est = S7::class_numeric,
        df = S7::class_numeric,
        t_stat = S7::class_numeric,
        p_value = S7::class_numeric,
        method_name = S7::new_property(
            class = S7::class_character,
            default = "",
            validator = function(value) {
                if (length(value) != 1L)
                    paste0("`method_name` must be length 1, not ", length(value), ".")
            }
        )
    )
)

S7::method(print, class_ttest_pairwise) = function(x, ...) {
    is_one_sample = all(x@var1 == x@var2)

    if (is_one_sample) {
        vars = x@var1
        grid = expand.grid(var1 = vars, var2 = vars, stringsAsFactors = FALSE)
        diag_idx = match(vars, vars)

        lookup = stats::setNames(seq_along(vars), vars)
        diff_vec = rep("", nrow(grid))
        t_vec = rep("", nrow(grid))
        pval_vec = rep("", nrow(grid))

        for (k in seq_along(vars)) {
            idx = which(grid$var1 == vars[[k]] & grid$var2 == vars[[k]])
            diff_vec[[idx]] = formatC(x@est[[k]], digits = 3, format = "f")
            t_vec[[idx]] = formatC(x@t_stat[[k]], digits = 3, format = "f")
            pval_vec[[idx]] = formatC(x@p_value[[k]], digits = 3, format = "f")
        }

        spec = tabstats::new_pairwise_data(
            var1 = grid$var1,
            var2 = grid$var2,
            diff = diff_vec,
            t_stat = t_vec,
            pval = pval_vec
        )
    } else {
        spec = tabstats::new_pairwise_data(
            var1 = x@var1,
            var2 = x@var2,
            diff = formatC(x@est, digits = 3, format = "f"),
            t_stat = formatC(x@t_stat, digits = 3, format = "f"),
            pval = formatC(x@p_value, digits = 3, format = "f")
        )
    }

    tabstats::pairwise_matrix(
        spec,
        title = if (nzchar(x@method_name)) x@method_name else "Pairwise t-Tests",
        layout_view = TRUE,
        diag_1 = FALSE,
        style = tabstats::cm_style(
            pval = function(x) {
                x_num = suppressWarnings(as.numeric(x))
                if (is.na(x_num) || x_num > 0.05) {
                    cli::style_italic(x)
                } else if (x_num > 0.01) {
                    cli::col_red(x)
                } else {
                    cli::style_bold("<0.001")
                }
            }
        )
    )

    invisible(x)
}
