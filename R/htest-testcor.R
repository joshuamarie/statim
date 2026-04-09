#' Correlation test
#'
#' `CORTEST()` performs a t-test for one-sample, two-sample, paired, pairwise,
#' or formula-based comparisons.
#'
#' @param .model A model ID from `rel()`, `pairwise()`, or a formula.
#'   When supplied, the test executes immediately. When `NULL` (default),
#'   returns a `test_spec` for use in the pipeline via [prepare_test()].
#' @param .data A data frame. Only used on the standalone path.
#' @param ... Additional arguments passed to the implementation:
#'   `.method`, `.ci` for the classical path.
#' @param .extra_defs A list of additional `test_define` objects supplied
#'   by the user. These extend the available implementations and engines.
#'
#' @return An `htest_spec` object (standalone or eager), or a `test_spec`
#'   object (pipeline).
#'
#' @section Supported model IDs:
#' - `rel()`: Many-to-one correlation test
#' - `pairwise()`: Pairwise correlation test
#'
#' @examples
#' # standalone
#' CORTEST(rel(speed, dist), cars)
#'
#' # Main pipeline
#' cars |>
#'     define_model(rel(speed, dist)) |>
#'     prepare_test(CORTEST) |>
#'     conclude()
#'
#' @export
CORTEST = HTEST_FN(
    # ---- correlation test API ----
    cls = "cortest",
    defs = list(
        cor_test_rel
    ),
    .name = "Correlation Test"
)
