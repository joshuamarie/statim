#' Build a hypothesis test function
#'
#' `HTEST_FN()` is a developer-interface constructor for user-facing test
#' functions like [TTEST()]. It returns a function with a consistent
#' signature that routes to the correct implementation based on the model
#' ID and method variant.
#'
#' @param cls A string naming the test class, e.g. `"ttest"`.
#' @param defs A list of [stat_define()] objects.
#' @param .name A string used as the test title in output.
#'
#' @return A function with signature `function(.model, .data, ...)`.
#'
#' @seealso [MODEL_FN()], [stat_define()], [prepare_test()], [via()], [conclude()]
#'
#' @export
HTEST_FN = function(cls, defs, .name) {
    STAT_CONSTRUCTOR(cls, defs, .name, spec_class = test_spec)
}
