#' Tidy a concluded statistical result
#'
#' `tidy()` extracts a tibble of primary results from a `cld_exec` object
#' produced by [conclude()].
#'
#' @section Dispatch:
#' Two paths are tried in order:
#'
#' **Path 1: `auto_tidy()` (preferred).**
#' When `cld_exec@data` is a [class_stat_infer] subclass, [auto_tidy()] is
#' called directly on it. You need no registry, and S7 automatically dispatches on the
#' particular output class, and variants that return the same class inherit the method
#' automatically via the parent chain.
#'
#' **Path 2: `making_tidy()` registry (escape hatch).**
#' When `cld_exec@data` is not a [class_stat_infer] subclass, for example, when a variant
#' intentionally returns any data structure e.g. just a plain list, S3, S4, or R6 object
#' (`check_sic_s7 = FALSE`), [tidy()] falls back to the registry populated
#' by [making_tidy()]. If no entry exists there either, an informative error
#' is raised.
#'
#' @param .x A `cld_exec` object produced by [conclude()].
#' @param ... Passed to the dispatched method.
#'
#' @return The statistical output in a `tibble` data frame format.
#'
#' @seealso [auto_tidy()], [making_tidy()], [method_tidy()], [class_stat_infer]
#'
#' @examples
#' mtcars |>
#'     define_model(mpg ~ .) |>
#'     prepare_model(LINEAR_REG) |>
#'     conclude() |>
#'     tidy()
#'
#' @export
tidy = S7::new_generic("tidy", ".x")

S7::method(tidy, cld_exec) = function(.x, ...) {
    if (S7::S7_inherits(.x@data, class_stat_infer)) {
        return(auto_tidy(.x@data, ...))
    }

    key = tidy_registry_key(.x@impl_cls)
    mt = register_tidy[[key]]

    impl_cls = .x@impl_cls

    if (is.null(mt)) {
        cli::cli_abort(c(
            "No tidy method found for {.val {impl_cls}}.",
            "i" = "Either return a {.cls class_stat_infer} subclass from {.fn fn},",
            "i" = "or register a tidy method via {.fn making_tidy}."
        ))
    }

    method_nm = .x@cld_meta$method
    tidy_fn = if (identical(method_nm, "default")) {
        mt@default
    } else if (!is.null(mt@variants[[method_nm]])) {
        mt@variants[[method_nm]]
    } else {
        cli::cli_abort(c(
            "No tidy entry for variant {.val {method_nm}} in {.val {impl_cls}}.",
            "i" = "Add {.code {method_nm} =} to {.fn method_tidy}, or return a",
            "i" = "{.cls class_stat_infer} subclass from {.fn fn} to use {.fn auto_tidy}."
        ))
    }

    if (is.null(tidy_fn)) {
        cli::cli_abort(c(
            "No {.arg default} tidy function registered for {.val {impl_cls}}.",
            "i" = "Supply {.code default =} in {.fn method_tidy}."
        ))
    }

    out = tidy_fn(.x, ...)

    if (!inherits(out, "tbl_df"))
        cli::cli_abort(
            "The output is preferrably in a tibble format, not {.obj_type_friendly {out}}."
        )

    out
}

#' Declare tidy methods for a stat and model type
#'
#' `making_tidy()` is the escape hatch for registering tidy methods when a
#' variant's `fn` intentionally returns a non-[class_stat_infer] object
#' (plain list, S3, S4, or R6). When `fn` returns a [class_stat_infer]
#' subclass, implement [auto_tidy()] on the result class instead — no
#' registration needed.
#'
#' @param obj A stat function built with [HTEST_FN()] or [MODEL_FN()]
#'   (e.g. `TTEST`).
#' @param model_type An S7 model ID class (e.g. `x_by`, `S7::class_formula`).
#'
#' @return A `making_tidy_call` object, consumed by `%<-%`.
#'
#' @seealso [auto_tidy()], [method_tidy()], [class_stat_infer]
#'
#' @examples
#' # Only needed when fn returns a non-class_stat_infer object.
#' # Prefer implementing auto_tidy() on your result class instead.
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
    is_model_id_class = inherits(model_type, "S7_class") &&
        identical(model_type@parent, model_id)
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
#' `method_tidy()` is the companion to [making_tidy()]. It collects tidy
#' functions for the base implementation and named variants, used only when
#' `fn` returns a non-[class_stat_infer] object.
#'
#' @param default A function with signature `function(.x, ...)`. Required.
#' @param ... Named functions, one per variant. Names must match variant
#'   names registered in [agendas()]. Omitted variants fall back to
#'   `default` automatically.
#'
#' @return A `method_tidy` S7 object.
#'
#' @seealso [making_tidy()], [auto_tidy()], [class_stat_infer]
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
