#' Declare the canonical implementation of a test
#'
#' `baseline()` declares the default implementation of a test, which is
#' the only implementation reachable on the eager path. It is frozen —
#' no user or package can swap it out via [swap_variant()].
#'
#' @param fn A function with named arguments. The framework injects
#'   data and arguments by matching formals to the processed model output.
#' @param print A function with signature `function(x, ...)` for formatting
#'   the result. `NULL` falls back to `print(x$data)`.
#'
#' @return A `baseline` S7 object.
#'
#' @seealso [variant()], [agendas()], [test_define()]
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

#' Declare an alternative implementation of a test
#'
#' `variant()` declares a named method variant reachable only via [via()].
#' Never runs on the eager path.
#'
#' @param fn A function with named arguments. The framework injects
#'   data and arguments by matching formals to the processed model output.
#' @param print A function with signature `function(x, ...)` for formatting
#'   the result. `NULL` falls back to `print(x$data)`.
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

#' Collect implementations for a test definition
#'
#' `agendas()` is the container for all implementations of a test.
#' It requires exactly one [baseline()] as its first argument, and accepts
#' any number of named [variant()] objects.
#'
#' @param base A [baseline()] object. Required.
#' @param ... Named [variant()] objects.
#'
#' @return An `agendas` S3 object.
#'
#' @seealso [baseline()], [variant()], [test_define()]
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
