#' Proportion Test
#'
#' `P_TEST()` performs a one-sample proportion test using either an exact
#' binomial test or a normal approximation. If `P_TEST` is supplied within the lazy-loaded pipeline,
#' supply `P_TEST` as a function within i.e. `prepare_test(.test = P_TEST)` call.
#'
#' @param .model A registered model ID, e.g. [prop()]. When supplied, the test executes
#'   immediately.
#' @param .data Unused. Accepted for pipeline consistency.
#' @param ... Additional arguments passed to the implementation. See the
#'   **Arguments** and **Variants** sections below.
#'
#' @return A `cld_exec` object, or a `test_spec` when `.model = NULL`.
#'   The object stored in `cld_exec@data` is a [class_p_test] object.
#'
#' @section Arguments:
#' The following arguments are passed via `...` in [P_TEST()] or [via()]:
#'
#' \describe{
#'   \item{`.p`}{Numeric. Hypothesized proportion under H\eqn{_0}. Default `0.5`.}
#'   \item{`.alt`}{Direction: `"two.sided"`, `"greater"`, or `"less"`.
#'     Default `"two.sided"`.}
#'   \item{`.ci`}{Confidence level. Default `0.95`.}
#' }
#'
#' @section Variants:
#' \describe{
#'   \item{`"prop"`}{Normal approximation via [stats::prop.test()] without
#'     continuity correction. Accepts the same `.p`, `.alt`, `.ci` arguments
#'     as the default, except with `correct` addition to indicate whether Yates'
#'     continuity correction should be applied or not.}
#' }
#'
#' @section Hypothesis claims:
#' Supports [PI()] via [state_null()]:
#'
#' ```r
#' define_model(prop(45, 100)) |>
#'     prepare_test(P_TEST) |>
#'     state_null(PI() == 0.5) |>
#'     conclude()
#' ```
#'
#' @examples
#' P_TEST(prop(45, 100))
#'
#' # piped syntax
#' define_model(prop(45, 100)) |>
#'     prepare_test(P_TEST) |>
#'     conclude()
#'
#' # normal approximation
#' define_model(prop(45, 100)) |>
#'     prepare_test(P_TEST) |>
#'     via("prop") |>
#'     conclude()
#'
#' # hypothesis claim
#' define_model(prop(45, 100)) |>
#'     prepare_test(P_TEST) |>
#'     state_null(PI() == 0.3) |>
#'     conclude()
#'
#' @seealso [prop()], [class_p_test], [PI()], [state_null()], [via()],
#'   [conclude()]
#'
#' @export
P_TEST = HTEST_FN(
    cls = "p_test",
    defs = list(ptest_def),
    .name = "Proportion Test"
)

#' Structured result container for proportion tests
#'
#' @description
#' An S7 class produced by [P_TEST] pipelines using [prop()] as the model
#' ID. Not constructed manually — use the pipeline instead.
#'
#' Inherits from [class_stat_infer], so [auto_tidy()] dispatches on it
#' automatically. Downstream packages can use it as a `parent` in
#' `S7::new_class()`.
#'
#' @usage NULL
#'
#' @details
#' Slots (populated automatically by [P_TEST]):
#'
#' - `x`: number of successes (input).
#' - `n`: number of trials (input).
#' - `estimate`: observed proportion (`x / n`).
#' - `statistic`: test statistic. The count `x` for the default binomial;
#'   the chi-squared value for `"prop"`.
#' - `p_val`: p-value.
#' - `lower_ci`: lower confidence bound.
#' - `upper_ci`: upper confidence bound.
#' - `ci_level`: confidence level, e.g. `0.95`.
#'
#' @section Shared by variants:
#' Both `default` and `prop` return a `class_p_test`, so [auto_tidy()] and
#' [print()] are inherited by `prop` for free.
#'
#' @seealso [P_TEST], [auto_tidy()], [class_stat_infer]
#'
#' @export
class_p_test = S7::new_class(
    "class_p_test",
    parent = class_stat_infer,
    properties = list(
        x = S7::class_numeric,
        n = S7::class_numeric,
        estimate = S7::class_numeric,
        statistic = S7::class_numeric,
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

S7::method(print, class_p_test) = function(x, ...) {
    ci_level = x@ci_level * 100
    lo_name = paste0("lower_", ci_level)
    up_name = paste0("upper_", ci_level)

    .x = x@x
    .n = x@n
    .estimate = x@estimate
    .statistic = x@statistic
    .p_val = x@p_val
    .lower_ci = x@lower_ci
    .upper_ci = x@upper_ci

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
        x = .x,
        n = .n,
        estimate = round(.estimate, 4),
        statistic = round(.statistic, 4),
        p_val = round(.p_val, 4)
    )

    ci_out = tibble::tibble(
        !!lo_name := fmt_ci(.lower_ci),
        !!up_name := fmt_ci(.upper_ci)
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

ptest_build = function(res, .proc, .ci) {
    class_p_test(
        x = .proc$x,
        n = .proc$n,
        estimate = unname(res$estimate),
        statistic = unname(res$statistic),
        p_val = res$p.value,
        lower_ci = res$conf.int[[1]],
        upper_ci = res$conf.int[[2]],
        ci_level = .ci
    )
}
