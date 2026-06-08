#' T-Test
#'
#' `TTEST()` performs a t-test for one-sample, two-sample, paired, pairwise,
#' or formula-based comparisons.
#'
#' @param .model A model ID from `x_by()`, `pairwise()`, or a formula.
#'   When supplied, the test executes immediately. When `NULL` (default),
#'   returns a `test_spec` for use in the pipeline via [prepare_test()].
#' @param .data A data frame. Only used on the standalone path.
#' @param ... Additional arguments passed to the implementation. See the
#'   **Arguments by model ID** section for the full list per path.
#'
#' @return A `cld_exec` object (in [conclude()]), a `stat_infer_spec` object, or a
#'   `test_spec` when `.model = NULL`. Depending on the implementation you wrote, it returns
#'   any class. However, by default, some implementations use base `{statim}` S7 classes.
#'   For instance:
#'   - `ttest_x_by`, by default, returns a [class_ttest_two] object
#'   - `ttest_pairwise`, by default, returns a [class_ttest_pairwise] object
#'
#' @section Supported model IDs:
#' Each model ID routes to a separate implementation. See the linked pages
#' for full argument lists, variants, and result class details:
#'
#' - `x_by()`: two-sample or paired t-test. See [ttest-xby].
#' - `pairwise()`: pairwise t-tests across variables. See [ttest-pairwise].
#' - `<formula>`: one-sample and/or two-sample t-test. See [ttest-formula].
#'
#' @inheritSection ttest-xby Arguments
#' @inheritSection ttest-xby Variants
#' @inheritSection ttest-xby Result class
#' @inheritSection ttest-xby Hypothesis claims
#'
#' @examples
#' # eager
#' TTEST(x_by(extra, group), sleep)
#'
#' # pipeline
#' sleep |>
#'     define_model(x_by(extra, group)) |>
#'     prepare_test(TTEST) |>
#'     conclude()
#'
#' # bootstrap
#' sleep |>
#'     define_model(x_by(extra, group)) |>
#'     prepare_test(TTEST) |>
#'     via("boot", n = 2000) |>
#'     conclude()
#'
#' # permutation
#' sleep |>
#'     define_model(x_by(extra, group)) |>
#'     prepare_test(TTEST) |>
#'     via("permute", n = 2000) |>
#'     conclude()
#'
#' # weighted t-test
#' sleep |>
#'     define_model(x_by(extra, group)) |>
#'     prepare_test(TTEST) |>
#'     state_null(
#'         2 * MU(extra, group == "1") <= MU(extra, group == "2")
#'     ) |>
#'     # Try to obtain 90% of the confidence interval
#'     via("weighted", .ci = 0.9) |>
#'     conclude()
#'
#' # pairwise
#' iris |>
#'     define_model(pairwise(Sepal.Length, Sepal.Width, Petal.Length)) |>
#'     prepare_test(TTEST) |>
#'     conclude()
#'
#' # hypothesis claim
#' sleep |>
#'     define_model(x_by(extra, group)) |>
#'     prepare_test(TTEST) |>
#'     state_null(MU(extra) == 0) |>
#'     conclude()
#'
#' @seealso
#' [ttest-xby], [ttest-pairwise], [ttest-formula] for per-implementation
#' details. [class_ttest_two], [class_ttest_pairwise] for result class
#' slots. [via()], [state_null()], [conclude()], [auto_tidy()].
#'
#' @export
TTEST = HTEST_FN(
    cls = "ttest",
    defs = list(
        ttest_def_two,
        ttest_def_formula,
        ttest_def_pairwise
    ),
    .name = "T-Test"
)

#' Structured result container for two-sample t-tests
#'
#' @description
#' An S7 class produced by [TTEST] pipelines using [x_by()] as the model ID.
#' Not constructed manually — use the pipeline instead.
#'
#' Inherits from [class_stat_infer], so [auto_tidy()] dispatches on it
#' automatically. Downstream packages can use it as a `parent` in
#' `S7::new_class()`.
#'
#' @usage NULL
#'
#' @details
#' Slots (populated automatically by [TTEST]):
#'
#' - `group`: name of the grouping variable.
#' - `estimate`: mean difference (or weighted contrast estimate).
#' - `t_stat`: t-statistic.
#' - `df`: degrees of freedom.
#' - `p_val`: p-value.
#' - `lower_ci`: lower confidence bound.
#' - `upper_ci`: upper confidence bound.
#' - `ci_level`: confidence level, e.g. `0.95`.
#'
#' @section Shared by variants:
#' Both `default` and `weighted` return a `class_ttest_two`, so
#' [auto_tidy()] and [print()] are inherited by `weighted` for free.
#'
#' @seealso [TTEST], [auto_tidy()], [class_stat_infer]
#'
#' @export
class_ttest_two = S7::new_class(
    "class_ttest_two",
    parent = class_stat_infer,
    properties = list(
        group = S7::class_character,
        estimate = S7::class_numeric,
        t_stat = S7::class_numeric,
        df = S7::class_numeric,
        p_val = S7::class_numeric,
        lower_ci = S7::class_numeric,
        upper_ci = S7::class_numeric,
        ci_level = S7::new_property(
            class = S7::class_numeric,
            default = 0.95,
            validator = function(value) {
                if (length(value) != 1L)
                    return(paste0("`ci_level` must be length 1, not ", length(value), "."))
                if (value <= 0 || value >= 1)
                    "`ci_level` must be between 0 and 1 (exclusive)."
            }
        )
    )
)

S7::method(print, class_ttest_two) = function(x, ...) {
    ci_level = x@ci_level * 100
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

    fmt_ci = function(val) {
        ifelse(is.infinite(val), ifelse(val > 0, "Inf", "-Inf"), round(val, 4))
    }

    stat_out = tibble::tibble(
        group = x@group,
        estimate = round(x@estimate, 4),
        t_stat = round(x@t_stat, 4),
        df = round(x@df, 2),
        p_val = round(x@p_val, 4)
    )

    ci_out = tibble::tibble(
        group = x@group,
        !!lo_name := fmt_ci(x@lower_ci),
        !!up_name := fmt_ci(x@upper_ci)
    )

    cli::cat_line(cli::rule(left = "Summary", line = "-"), "\n")
    tabstats::table_default(
        stat_out,
        style_columns = tabstats::td_style(p_val = pval_styler)
    )
    cat("\n\n")

    cli::cat_line(cli::rule(left = "Confidence Interval", line = "-"), "\n")
    tabstats::table_default(ci_out)
    cat("\n\n")

    invisible(x)
}
