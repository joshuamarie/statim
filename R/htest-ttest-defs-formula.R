#' @title T-Test: Formula interface
#'
#' @description
#' The formula implementation performs one-sample or two-sample t-tests
#' specified via a standard R formula. The response variable is taken from
#' the left-hand side; the right-hand side determines the test type:
#'
#' 1. `y ~ group`: two-sample t-test, one test per grouping variable.
#' 2. `y ~ 1`: one-sample t-test against `.mu`.
#' 3. `y ~ group + 1`" both tests in a single call.
#'
#' Use a formula directly as the model ID to select this implementation.
#'
#' @section Arguments:
#' The following arguments are passed via `...` in [TTEST()]:
#'
#' \describe{
#'   \item{`.mu`}{Numeric. Hypothesized mean or mean difference. Default `0`.}
#'   \item{`.alt`}{String. One of `"two.sided"`, `"greater"`, or `"less"`.
#'     Default `"two.sided"`.}
#'   \item{`.ci`}{Numeric. Confidence level. Default `0.95`.}
#' }
#'
#' @section Variants:
#' No variants are currently registered for the formula path. Use
#' [add_variant()] to register custom variants at the user or package level.
#'
#' @section Result class:
#' Returns a tibble with columns `type`, `group`, and `ttest` (a list-column
#' of [stats::t.test()] results). This path does not currently return a
#' [class_stat_infer] subclass — use [making_tidy()] to register a tidy
#' method if needed.
#'
#' @examples
#' sleep |>
#'     define_model(extra ~ group) |>
#'     prepare_test(TTEST) |>
#'     conclude()
#'
#' # one-sample
#' sleep |>
#'     define_model(extra ~ 1) |>
#'     prepare_test(TTEST) |>
#'     conclude()
#'
#' # both in one call
#' sleep |>
#'     define_model(extra ~ group + 1) |>
#'     prepare_test(TTEST) |>
#'     conclude()
#'
#' @keywords internal
#' @name ttest-formula
#' @family ttest-implementations
NULL

ttest_def_formula = test_define(
    model_type = S7::class_formula,
    impl = agendas(
        base = baseline(
            fn = function(.proc, .mu = 0, .alt = "two.sided", .ci = 0.95) {
                formula = .proc$formula
                data = .proc$data

                trms = terms(formula)
                response = all.vars(formula)[1]
                rhs_labels = attr(trms, "term.labels")
                has_one_samp = grepl("\\b1\\b", deparse(formula[[3]]))

                two_samp = lapply(rhs_labels, function(grp_name) {
                    f = stats::as.formula(paste(response, "~", grp_name))
                    grp = as.character(data[[grp_name]])
                    lvls = unique(grp)

                    if (length(lvls) != 2L) {
                        cli::cli_abort(c(
                            "Two-sample t-test requires exactly 2 groups.",
                            "i" = "Found {length(lvls)} group{{?s}} in {.val {grp_name}}."
                        ))
                    }

                    res = stats::t.test(
                        formula = f,
                        data = data,
                        mu = .mu,
                        alternative = .alt,
                        conf.level = .ci
                    )

                    make_test("two sample", grp_name, res)
                })

                one_samp = if (has_one_samp) {
                    f_one = stats::as.formula(paste(response, "~ 1"))
                    res = stats::t.test(
                        formula = f_one,
                        data = data,
                        mu = .mu,
                        alternative = .alt,
                        conf.level = .ci
                    )
                    list(make_test("one sample", "1", res))
                } else {
                    list()
                }

                tests = c(two_samp, one_samp)

                tibble::tibble(
                    type = vapply(tests, `[[`, character(1), "type"),
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

                    stat_row = dplyr::mutate(
                        td,
                        groups = dat$group[[i]],
                        type = dat$type[[i]],
                        est_type = switch(
                            type,
                            `two sample` = "mu_diff",
                            `one sample` = "mu"
                        ),
                        est = estimate,
                        `t-stat` = statistic,
                        pval = p.value,
                        .keep = "none"
                    )
                    ci_row = dplyr::mutate(
                        td,
                        groups = dat$group[[i]],
                        type = dat$type[[i]],
                        !!lo_name := conf.low,
                        !!up_name := conf.high,
                        .keep = "none"
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
    )
)

make_test = function(type, group, ttest) {
    list(type = type, group = group, ttest = ttest)
}

