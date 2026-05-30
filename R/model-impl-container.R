#' Build a model inference function
#'
#' `MODEL_FN()` is a developer-interface constructor for user-facing model
#' functions like `LINEAR_REG()`. It returns a function that routes to the
#' correct implementation based on the model ID and method variant.
#'
#' @param cls A string naming the model class, e.g. `"linear_reg"`.
#' @param defs A list of [stat_define()] objects.
#' @param .name A string used as the model title in output.
#'
#' @return A function with signature `function(.model, .data, ...)`.
#'
#' @seealso [HTEST_FN()], [stat_define()], [prepare_model()], [via()], [conclude()]
#'
#' @export
MODEL_FN = function(cls, defs, .name) {
    STAT_CONSTRUCTOR(cls, defs, .name, spec_class = model_spec)
}
