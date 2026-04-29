#' Define a test implementation
#'
#' `test_define()` declares a single implementation of a hypothesis test.
#' Multiple `test_define` objects are passed to [HTEST_FN()] via `defs`.
#' This is the main extension point for adding new tests or engines.
#'
#' @param model_type A string matching the primary class of the model ID
#'   this implementation handles. E.g. `"x_by"`, `"pairwise"`.
#' @param impl_class A string naming the implementation class.
#'   E.g. `"ttest_two"`. Used in the S3 class vector of the result.
#' @param impl An [agendas()] object declaring all implementations.
#' @param compatible_params A character vector of `ParamDef` types this
#'   implementation can interpret via `write_claim()`. E.g. `c("MU")`.
#' @param eval_claim A function with signature `function(self, claim)`
#'   that interprets a `ClaimDef` for this implementation. `NULL` if
#'   `write_claim()` is not supported.
#'
#' @return A `test_define` S7 object.
#'
#' @seealso [agendas()], [baseline()], [variant()], [HTEST_FN()]
#'
#' @export
test_define = S7::new_class(
    "test_define",
    properties = list(
        model_type = S7::class_character,
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
