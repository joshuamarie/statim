#' Declare the canonical implementation of a test or model
#'
#' `baseline()` declares the default implementation of a statistical procedure,
#' which is the only implementation reachable on the eager path. It is always the
#' default.
#'
#' @param fn A function with named arguments. The framework injects
#'   data and arguments by matching formals to the processed model output.
#' @param print A function with signature `function(x, ...)` for formatting
#'   the result. `NULL` falls back to `print(x@data)`.
#'
#' @return A `baseline` S7 object.
#'
#' @seealso [variant()], [agendas()], [stat_define()]
#'
#' @export
baseline = S7::new_class(
    "baseline",
    properties = list(
        fn = S7::new_property(class = S7::class_function),
        print = S7::new_property(default = NULL)
    ),
    constructor = function(fn, print = NULL) {
        if (!is.function(fn)) {
            cli::cli_abort("{.arg fn} must be a function.")
        }
        first_arg = names(formals(fn))[[1]]
        if (!identical(first_arg, ".proc")) {
            cli::cli_abort(c(
                "{.arg fn} must have {.arg .proc} as its first argument.",
                "i" = "Found {.arg {first_arg}} instead.",
                "i" = "See {.fn baseline} for the expected signature."
            ))
        }
        if (!is.null(print) && !is.function(print)) {
            cli::cli_abort("{.arg print} must be a function or {.val NULL}.")
        }
        S7::new_object(
            S7::S7_object(),
            fn = fn,
            print = print
        )
    }
)

#' Declare an alternative implementation of a test or model
#'
#' `variant()` declares a named method variant reachable only via [via()].
#' Never runs on the eager path.
#'
#' @param fn A function with named arguments. The framework injects
#'   data and arguments by matching formals to the processed model output.
#' @param print A function with signature `function(x, ...)` for formatting
#'   the result. `NULL` falls back to `print(x@data)`.
#'
#' @return A `variant` S7 object.
#'
#' @seealso [baseline()], [agendas()], [via()]
#'
#' @export
variant = S7::new_class(
    "variant",
    properties = list(
        fn = S7::new_property(class = S7::class_function),
        print = S7::new_property(default = NULL)
    ),
    constructor = function(fn, print = NULL) {
        if (!is.function(fn)) {
            cli::cli_abort("{.arg fn} must be a function.")
        }
        first_arg = names(formals(fn))[[1]]
        if (!identical(first_arg, ".proc")) {
            cli::cli_abort(c(
                "{.arg fn} must have {.arg .proc} as its first argument.",
                "i" = "Found {.arg {first_arg}} instead.",
                "i" = "See {.fn baseline} for the expected signature."
            ))
        }
        if (!is.null(print) && !is.function(print)) {
            cli::cli_abort("{.arg print} must be a function or {.val NULL}.")
        }
        S7::new_object(
            S7::S7_object(),
            fn = fn,
            print = print
        )
    }
)

#' Collect implementations for a statistical procedure
#'
#' `agendas()` is the container for all implementations of a procedure.
#' It requires exactly one [baseline()] as its first argument, and accepts
#' any number of named [variant()] objects.
#'
#' @param base A [baseline()] object. Required.
#' @param ... Named [variant()] objects.
#'
#' @return An `agendas` S3 object.
#'
#' @seealso [baseline()], [variant()], [stat_define()]
#'
#' @export
agendas = function(base, ...) {
    if (missing(base)) {
        cli::cli_abort("{.arg base} is required in {.fn agendas}.")
    }
    if (!S7::S7_inherits(base, baseline)) {
        cli::cli_abort("{.arg base} must be a {.cls baseline} object.")
    }

    variants = list(...)
    if (length(variants) > 0) {
        unnamed = which(names(variants) == "" | is.null(names(variants)))
        if (length(unnamed) > 0) {
            cli::cli_abort("All variants in {.fn agendas} must be named.")
        }

        bad = Filter(function(nm) !S7::S7_inherits(variants[[nm]], variant), names(variants))
        if (length(bad) > 0) {
            cli::cli_abort(c(
                "All additional arguments to {.fn agendas} must be {.cls variant} objects.",
                "x" = "Not a variant: {.val {bad}}."
            ))
        }
    }

    out = list(base = base, variants = variants)
    class(out) = "agendas"
    out
}
