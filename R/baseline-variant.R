#' Declare the canonical implementation of a test or model
#'
#' `baseline()` declares the default implementation of a statistical procedure.
#' It is always the default and is the only implementation reachable on the
#' eager path.
#'
#' @param fn A function whose first argument must be `.proc`, the processed
#'   model output from [model_processor()]. The keys available on `.proc`
#'   depend on the model ID used:
#'   - `x_by`: `$x_data`, `$group_data`
#'   - `rel`: `$x_data`, `$resp_data`
#'   - `pairwise`: `$var_names`, `$pairs`, `$data`
#'   - `formula`: `$data`, `$vars`, `$formula`
#'
#'   Try run this to explore the structure: `names(model_processor(<model_id>, <data>))`.
#'
#'   \cr
#'
#'   Additional named arguments are user-supplied statistical parameters
#'   (e.g. `.mu`, `.ci`). See [model_processor()] for the full `.proc`
#'   schema per model type.
#'
#'   ```r
#'   baseline(
#'       fn = function(.proc, .mu = 0, .ci = 0.95) {
#'           # ...
#'           my_result(...)   # return a class_stat_infer subclass
#'       }
#'   )
#'   ```
#'
#' @param print A function with signature `function(x, ...)` for formatting
#'   the result. `x` is a `cld_exec` object — read your result from `x@data`.
#'   `NULL` falls back to `print(x@data)`.
#'
#' @param check_sic_s7 A single logical. When `TRUE`, [conclude()] verifies
#'   at runtime that `fn` returns a [class_stat_infer] subclass. A warning
#'   is issued if it does not, reminding you that [auto_tidy()] and future
#'   `auto_*()` generics will not dispatch automatically.
#'
#'   Set to `FALSE` (the default) when `fn` intentionally returns anything.
#'   In that case, register a tidy method via [making_tidy()] if [tidy()] support
#'   is needed.
#'
#' @return A `baseline` S7 object.
#'
#' @seealso [variant()], [agendas()], [stat_define()], [model_processor()],
#'   [class_stat_infer], [auto_tidy()]
#'
#' @export
baseline = S7::new_class(
    "baseline",
    properties = list(
        fn = S7::new_property(class = S7::class_function),
        print = S7::new_property(default = NULL),
        check_sic_s7 = S7::new_property(class = S7::class_logical, default = FALSE)
    ),
    constructor = function(fn, print = NULL, check_sic_s7 = FALSE) {
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
        if (!is.logical(check_sic_s7) || length(check_sic_s7) != 1L || is.na(check_sic_s7)) {
            cli::cli_abort("{.arg check_sic_s7} must be a single {.cls logical} value.")
        }
        S7::new_object(
            S7::S7_object(),
            fn = fn,
            print = print,
            check_sic_s7 = check_sic_s7
        )
    }
)

#' Declare an alternative implementation of a test or model
#'
#' `variant()` declares a named alternative implementation reachable only via
#' [via()]. Never runs on the eager path.
#'
#' @param fn A function whose first argument must be `.proc`, the processed
#'   model output from [model_processor()]. The keys available on `.proc`
#'   depend on the model ID used:
#'   - `x_by`: `$x_data`, `$group_data`
#'   - `rel`: `$x_data`, `$resp_data`
#'   - `pairwise`: `$var_names`, `$pairs`, `$data`
#'   - `formula`: `$data`, `$vars`, `$formula`
#'
#'   Try run this to explore the structure: `names(model_processor(<model_id>, <data>))`.
#'
#'   \cr
#'
#'   Additional named arguments are user-supplied statistical parameters
#'   (e.g. `.mu`, `.ci`). See [model_processor()] for the full `.proc`
#'   schema per model type.
#'
#'   ```r
#'   variant(
#'       fn = function(.proc, n = 1000L, seed = NULL) {
#'           x = .proc$x_data[[1]]
#'           group_data = .proc$group_data
#'           # ...
#'       }
#'   )
#'   ```
#'
#'   A variant whose `fn` returns the same [class_stat_infer] subclass as
#'   `baseline` inherits [auto_tidy()] and all future `auto_*()` methods
#'   automatically. A variant returning a subclass can override selectively:
#'
#'   ```r
#'   # inherits auto_tidy() from my_result
#'   variant(
#'       fn = function(.proc, ...) { my_result(...) },
#'       check_sic_s7 = TRUE
#'   )
#'
#'   # overrides auto_tidy() via subclass
#'   variant(
#'       fn = function(.proc, ...) { my_result_boot(...) },
#'       check_sic_s7 = TRUE
#'   )
#'
#'   # intentionally plain, no internal `auto_*()` dispatch
#'   variant(
#'       fn = function(.proc, ...) { list(...) },
#'       check_sic_s7 = FALSE
#'   )
#'   ```
#'
#' @param print A function with signature `function(x, ...)`. `x` is a
#'   `cld_exec` object. `NULL` falls back to `print(x@data)`.
#'
#' @param check_sic_s7 A single logical. When `TRUE`, [conclude()] verifies
#'   that `fn` returns a [class_stat_infer] subclass, warning if it does not.
#'   Default `FALSE` preserves full freedom over the return type.
#'
#' @return A `variant` S7 object.
#'
#' @seealso [baseline()], [agendas()], [via()], [model_processor()],
#'   [class_stat_infer], [auto_tidy()]
#'
#' @export
variant = S7::new_class(
    "variant",
    properties = list(
        fn = S7::new_property(class = S7::class_function),
        print = S7::new_property(default = NULL),
        check_sic_s7 = S7::new_property(class = S7::class_logical, default = FALSE)
    ),
    constructor = function(fn, print = NULL, check_sic_s7 = FALSE) {
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
        if (!is.logical(check_sic_s7) || length(check_sic_s7) != 1L || is.na(check_sic_s7)) {
            cli::cli_abort("{.arg check_sic_s7} must be a single {.cls logical} value.")
        }
        S7::new_object(
            S7::S7_object(),
            fn = fn,
            print = print,
            check_sic_s7 = check_sic_s7
        )
    }
)

#' Collect implementations for a statistical procedure
#'
#' `agendas()` is the container for all implementations of a procedure.
#' Requires exactly one [baseline()] and accepts any number of named
#' [variant()] objects.
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
