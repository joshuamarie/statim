#' Define a statistical procedure implementation
#'
#' `stat_define()` declares a single implementation of a statistical procedure
#' for a given model type. Multiple `stat_define` objects are passed to
#' [HTEST_FN()] or [MODEL_FN()] via `defs`. This is the main extension point
#' for adding new tests or models.
#'
#' @param model_type A model ID class this implementation handles (e.g. `x_by`,
#'   `S7::class_formula`).
#' @param impl An [agendas()] object collecting all implementations. The `fn`
#'   of each [baseline()] and [variant()] inside receives `.proc` as its first
#'   argument. See [baseline()] for the expected signature and [model_processor()]
#'   for the keys available on `.proc` per model type.
#' @param compatible_params A list of S7 param classes (e.g. `list(MU, PI)`)
#'   this implementation accepts in hypothesis claims. An empty list (the
#'   default) disables the check entirely. Useful when a test is param-agnostic
#'   or the restriction has not yet been declared.
#' @param claim_translator A `claim_translate` object or function that maps a
#'   `ClaimDef` to named arguments injected into the implementation alongside
#'   `.proc`. `NULL` if `write_claim()` is not supported.
#'
#' @return A `stat_define` S7 object.
#'
#' @seealso [agendas()], [baseline()], [variant()], [model_processor()],
#'   [HTEST_FN()], [MODEL_FN()]
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
        # impl_class = S7::class_character,
        impl = S7::new_property(
            class = S7::class_any,
            validator = function(value) {
                if (!inherits(value, "agendas"))
                    "must be an `agendas` object"
            }
        ),
        compatible_params = S7::new_property(
            class = S7::class_list,
            default = list(),
            validator = function(value) {
                if (length(value) == 0L) return(NULL)
                bad = !vapply(value, function(cl) {
                    inherits(cl, "S7_class") && is_param_class(cl)
                }, logical(1))
                if (any(bad)) {
                    nms = vapply(value[bad], function(cl) {
                        if (inherits(cl, "S7_class")) cl@name else class(cl)[[1]]
                    }, character(1))
                    paste0(
                        "compatible_params must be a list of param_obj subclasses ",
                        "(e.g. list(MU, PI)). Invalid: ",
                        paste(nms, collapse = ", ")
                    )
                }
            }
        ),
        claim_translator = S7::new_property(default = NULL)
    )
)

#' @rdname stat-infer-definer
#' @export
test_define = stat_define

#' @rdname stat-infer-definer
#' @export
model_infer_define = stat_define
