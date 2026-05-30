#' Tidy a concluded statistical result
#'
#' @param .x A `cld_exec` object produced by [conclude()].
#' @param ... Passed to the registered tidy function.
#'
#' @return A tibble.
#'
#' @export
tidy = S7::new_generic("tidy", ".x")

S7::method(tidy, cld_exec) = function(.x, ...) {
    key = tidy_registry_key(.x@impl_cls)

    mt = register_tidy[[key]]
    if (is.null(mt)) {
        cli::cli_abort(c(
            "No tidy method registered for {.val {(.x@impl_cls)}}.",
            "i" = "Register one with {.code making_tidy(<stat_fn>, <model_type>) %<-% method_tidy(...)}."
        ))
    }

    method_nm = .x@cld_meta$method
    tidy_fn = if (identical(method_nm, "default")) {
        mt@default
    } else {
        mt@variants[[method_nm]]
    }

    if (is.null(tidy_fn)) {
        if (identical(method_nm, "default")) {
            cli::cli_abort(c(
                "No {.arg default} tidy function registered for {.val {(.x@impl_cls)}}.",
                "i" = "Supply {.code default =} in {.fn method_tidy}."
            ))
        } else {
            cli::cli_abort(c(
                "No tidy entry for variant {.val {(method_nm)}} in {.val {(.x@impl_cls)}}.",
                "i" = "Add {.code {method_nm} =} to {.fn method_tidy} for this stat."
            ))
        }
    }

    tidy_fn(.x, ...)
}

#' Declare tidy methods for a stat and model type
#'
#' `making_tidy()` is used as the left-hand side of the `%<-%` operator
#' to register a [method_tidy()] for a stat function and model type.
#'
#' @param obj A stat function built with [HTEST_FN()] or [MODEL_FN()]
#'   (e.g. `TTEST`). Used to scope the registry key.
#' @param model_type An S7 model ID class (e.g. `x_by`, `S7::class_formula`).
#'
#' @return A `making_tidy_call` object, consumed by `%<-%`.
#'
#' @examples
#' making_tidy(TTEST, x_by) %<-% method_tidy(
#'     default = function(.x, ...) { ... },
#'     boot = function(.x, ...) { ... }
#' )
#'
#' @export
making_tidy = function(obj, model_type) {
    structure(
        list(obj = obj, model_type = model_type),
        class = "making_tidy_call"
    )
}

#' @keywords internal
making_tidy_register = function(lhs, rhs) {
    obj = lhs$obj
    model_type = lhs$model_type

    stat_cls = attr(obj, "cls") %||% cli::cli_abort(
        "{.arg obj} must be a function built with {.fn HTEST_FN} or {.fn MODEL_FN}."
    )
    is_model_id_class = inherits(model_type, "S7_class") && identical(model_type@parent, model_id)
    is_formula_class = identical(model_type, S7::class_formula)
    if (!is_model_id_class && !is_formula_class) {
        cli::cli_abort(
            "{.arg model_type} must be a class inheriting from {.cls model_id}, or {.code S7::class_formula}."
        )
    }
    if (!S7::S7_inherits(rhs, method_tidy)) {
        cli::cli_abort(
            "Right-hand side of {.code %<-%} must be a {.cls method_tidy} object."
        )
    }

    key = tidy_registry_key(paste0(stat_cls, "_", model_type_name(model_type)))
    existing = register_tidy[[key]]
    if (is.null(existing)) {
        register_tidy[[key]] = rhs
    } else {
        merged_default = rhs@default %||% existing@default
        merged_variants = utils::modifyList(existing@variants, rhs@variants)
        register_tidy[[key]] = do.call(method_tidy, c(list(merged_default), merged_variants))
    }
    invisible(NULL)
}

#' Declare tidy methods for a stat result
#'
#' `method_tidy()` collects tidy functions for the base implementation and any
#' named variants. The `default` function handles results from the base
#' implementation; additional named arguments handle variant results.
#'
#' @param default A function with signature `function(.x, ...)` for the base
#'   implementation. Required.
#' @param ... Named functions, one per variant (e.g. `boot =`, `contrast =`).
#'   Names must match the variant names registered in [agendas()].
#'
#' @return A `method_tidy` S7 object.
#'
#' @export
method_tidy = S7::new_class(
    "method_tidy",
    properties = list(
        default = S7::new_property(default = NULL),
        variants = S7::new_property(class = S7::class_list, default = list())
    ),
    constructor = function(default = NULL, ...) {
        variants = list(...)

        if (!is.null(default) && !is.function(default)) {
            cli::cli_abort("{.arg default} must be a function or {.val NULL}.")
        }
        bad = !vapply(variants, is.function, logical(1))
        if (any(bad)) {
            cli::cli_abort(
                "All variant entries must be functions. Non-function: {.arg {names(variants)[bad]}}."
            )
        }
        if (!is.null(names(variants)) && any(!nzchar(names(variants)))) {
            cli::cli_abort("All variant entries in {.fn method_tidy} must be named.")
        }

        S7::new_object(S7::S7_object(), default = default, variants = variants)
    }
)

register_tidy = new.env(parent = emptyenv())
tidy_registry_key = function(impl_cls) impl_cls
