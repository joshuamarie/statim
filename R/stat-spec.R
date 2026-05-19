#' Define a statistical procedure implementation
#'
#' `stat_define()` declares a single implementation of a statistical procedure.
#' Multiple `stat_define` objects are passed to [HTEST_FN()] or [MODEL_FN()]
#' via `defs`. This is the main extension point for adding new tests or models.
#'
#' @param model_type A string matching the primary class of the model ID
#'   this implementation handles. E.g. `"x_by"`, `"pairwise"`, `"rel"`.
#' @param impl_class A string naming the implementation class.
#'   E.g. `"ttest_two"`, `"linear_reg_rel"`. Used in the S3 class vector
#'   of the result.
#' @param impl An [agendas()] object declaring all implementations.
#' @param compatible_params A character vector of `ParamDef` types this
#'   implementation can interpret via `write_claim()`. E.g. `c("MU")`.
#' @param eval_claim A function with signature `function(self, claim)`
#'   that interprets a `ClaimDef` for this implementation. `NULL` if
#'   `write_claim()` is not supported.
#'
#' @return A `stat_define` S7 object.
#'
#' @seealso [agendas()], [baseline()], [variant()], [HTEST_FN()], [MODEL_FN()]
#'
#' @name stat-infer-definer
#' @export
stat_define = S7::new_class(
    "stat_define",
    properties = list(
        model_type = S7::new_property(
            class = S7::class_any,
            validator = function(value) {
                is_model_id_class = inherits(value, "S7_class") && identical(value@parent, model_id)
                is_formula_class = identical(value, S7::class_formula)
                if (!is_model_id_class && !is_formula_class)
                    "must be a class that inherits from `model_id`, or `S7::class_formula`"
            }
        ),
        impl_class = S7::class_character,
        impl = S7::new_property(
            class = S7::class_any,
            validator = function(value) {
                if (!inherits(value, "agendas"))
                    "must be an `agendas` object"
            }
        ),
        compatible_params = S7::new_property(
            class = S7::class_character,
            default = character(0)
        ),
        eval_claim = S7::new_property(default = NULL)
    )
)

#' @rdname stat-infer-definer
#' @export
test_define = stat_define

#' @rdname stat-infer-definer
#' @export
model_infer_define = stat_define
