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
            "i" = "Register one with {.code making_tidy(<stat_define>) <- method_tidy(...)}."
        ))
    }

    method_nm = .x@cld_meta$method
    tidy_fn = if (identical(method_nm, "default")) {
        mt@default
    } else {
        mt@variants[[method_nm]]
    }

    if (is.null(tidy_fn)) {
        cli::cli_abort(c(
            "No tidy entry for variant {.val {(method_nm)}} in {.val {(.x@impl_cls)}}.",
            "i" = "Add {.code {method_nm} =} to {.fn method_tidy} for this {.cls stat_define}."
        ))
    }

    tidy_fn(.x, ...)
}

#' Declare tidy methods for a stat_define
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
        default = S7::new_property(class = S7::class_function),
        variants = S7::new_property(class = S7::class_list, default = list())
    ),
    constructor = function(default, ...) {
        variants = list(...)

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

#' Register tidy methods onto a stat_define object
#'
#' @param x A `stat_define` object.
#' @param value A `method_tidy` S7 object.
#'
#' @return `x`, invisibly.
#'
#' @export
`making_tidy<-` = S7::new_generic("making_tidy<-", c("x", "class", "value"))

S7::method(`making_tidy<-`, list(stat_define, S7::class_function, method_tidy)) = function(x, class, value) {
    stat_cls = attr(class, "cls")
    if (is.null(stat_cls)) {
        cli::cli_abort(c(
            "{.arg class} must be a function built by {.fn HTEST_FN} or {.fn MODEL_FN}.",
            "i" = "E.g. {.code making_tidy(ttest_def_two, TTEST) <- method_tidy(...)}."
        ))
    }
    key = tidy_registry_key(paste0(stat_cls, "_", model_type_name(x@model_type)))
    register_tidy[[key]] = value
    invisible(x)
}

register_tidy = new.env(parent = emptyenv())
tidy_registry_key = function(impl_cls) impl_cls
