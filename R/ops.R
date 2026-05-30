#' Apply a method_tidy to a making_tidy target
#'
#' `%<-%` registers a [method_tidy()] into the tidy registry. The
#' left-hand side must be a [making_tidy()] call.
#'
#' @param lhs A `making_tidy_call` object from [making_tidy()].
#' @param rhs A [method_tidy()] object.
#'
#' @return `NULL` invisibly, called for its side effects.
#'
#' @examples
#' making_tidy(TTEST, x_by) %<-% method_tidy(
#'     default = function(.x, ...) { ... },
#'     boot = function(.x, ...) { ... }
#' )
#'
#' @name modifying-assignment
#' @export
`%<-%` = function(lhs, rhs) {
    if (inherits(lhs, "add_variant_call")) {
        add_variant_register(lhs, rhs)
    } else if (inherits(lhs, "making_tidy_call")) {
        making_tidy_register(lhs, rhs)
    } else {
        cli::cli_abort(
            "Left-hand side of {.code %<-%} must be an {.fn add_variant} or {.fn making_tidy} call."
        )
    }
}

#' Chained equality operator for null hypotheses
#'
#' `%=%` declares that all chained population parameters are hypothesized
#' to be equal. Used inside [state_null()] only — it is a syntactic macro
#' and will error if called outside that context.
#'
#' @param lhs The left-hand side population parameter.
#' @param rhs The right-hand side population parameter.
#'
#' @return Does not return. Always throws an error when called outside
#'   [state_null()].
#'
#' @examples
#' sleep |>
#'     define_model(x_by(extra, group)) |>
#'     prepare_test(ANOVA) |>
#'     state_null(
#'         MU(extra, group == "1") %=%
#'         MU(extra, group == "2")
#'     ) |>
#'     conclude()
#'
#' @name equal-op
#' @export
`%=%` = function(lhs, rhs) {
    cli::cli_abort(
        "{.code %=%} must be used inside {.fn state_null}."
    )
}

# `%<-%` = function(lhs, rhs) {
#     if (!inherits(lhs, "making_tidy_call")) {
#         cli::cli_abort(
#             "Left-hand side of {.code %<-%} must be a {.fn making_tidy} call."
#         )
#     }
#     if (!S7::S7_inherits(rhs, method_tidy)) {
#         cli::cli_abort(
#             "Right-hand side of {.code %<-%} must be a {.cls method_tidy} object."
#         )
#     }
#
#     obj = lhs$obj
#     model_type = lhs$model_type
#
#     stat_cls = attr(obj, "cls") %||% cli::cli_abort(
#         "{.arg obj} must be a function built with {.fn HTEST_FN} or {.fn MODEL_FN}."
#     )
#     is_model_id_class = inherits(model_type, "S7_class") && identical(model_type@parent, model_id)
#     is_formula_class = identical(model_type, S7::class_formula)
#     if (!is_model_id_class && !is_formula_class) {
#         cli::cli_abort(
#             "{.arg model_type} must be a class inheriting from {.cls model_id}, or {.code S7::class_formula}."
#         )
#     }
#
#     key = tidy_registry_key(paste0(stat_cls, "_", model_type_name(model_type)))
#     register_tidy[[key]] = rhs
#     invisible(NULL)
# }
