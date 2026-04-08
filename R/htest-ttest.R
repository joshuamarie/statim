#' T-Test
#'
#' `TTEST()` performs a t-test for one-sample, two-sample, paired, pairwise,
#' or formula-based comparisons.
#'
#' @param .model A model ID from `x_by()`, `pairwise()`, or a formula.
#'   When supplied, the test executes immediately. When `NULL` (default),
#'   returns a `test_spec` for use in the pipeline via [prepare_test()].
#' @param .data A data frame. Only used on the standalone path.
#' @param ... Additional arguments passed to the implementation:
#'   `.paired`, `.mu`, `.alt`, `.ci` for the classical path.
#' @param .extra_defs A list of additional `test_define` objects supplied
#'   by the user. These extend the available implementations and engines.
#'
#' @return An `htest_spec` object (standalone or eager), or a `test_spec`
#'   object (pipeline).
#'
#' @section Supported model IDs:
#' - `x_by()` — two-sample or paired t-test
#'
#' @section Method variants:
#' - `"boot"` — bootstrap confidence interval via [via()]
#'
#' @examples
#' # standalone
#' TTEST(x_by(extra, group), sleep)
#'
#' # Main pipeline
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
#' @export
TTEST = HTEST_FN(
    # ---- t-test API ----
    cls = "ttest",
    defs = list(
        ttest_def_two,
        ttest_def_boot,
        ttest_def_permute,
        ttest_def_permute_rfast,
        ttest_def_formula,
        ttest_def_pairwise
    ),
    .name = "T-Test"
)
