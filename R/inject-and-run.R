#' Resolve and call the right fn from a baseline or variant,
#' injecting the processed model output and user-supplied args.
#'
#' `.proc` is always injected as the first argument. Every other formal
#' is resolved from `args`, falling back to the declared default in `fn`.
#'
#' @param impl A `baseline` or `variant` S7 object.
#' @param processed Output of `model_processor()`. Passed directly as `.proc`.
#' @param args A named list of user-supplied arguments.
#' @param claims A named list of resolved `ClaimDef` objects. `NULL` if none.
#'
#' @return The raw output of the `fn` call.
#'
#' @keywords internal
#' @noRd
inject_and_run = function(impl, processed, args, claims = NULL) {
    fn = impl@fn
    fn_formals = formals(fn)
    fn_args = names(fn_formals)
    fn_args = fn_args[fn_args != "..."]

    injected = vector("list", length(fn_args))
    names(injected) = fn_args
    missing_args = character(0)

    for (arg in fn_args) {
        if (arg == ".proc") {
            injected[[arg]] = processed
            next
        }
        if (!is.null(args[[arg]])) {
            injected[[arg]] = args[[arg]]
            next
        }
        if (rlang::is_missing(formals(fn)[[arg]])) {
            missing_args = c(missing_args, arg)
            next
        }
        val = formals(fn)[[arg]]
        injected[[arg]] = if (is.call(val)) {
            eval(val, envir = environment(fn) %||% baseenv())
        } else {
            val
        }
    }

    if (length(missing_args) > 0L) {
        cli::cli_abort(c(
            "{length(missing_args)} required argument{?s} not supplied: {.arg {missing_args}}.",
            "i" = "Supply via {.code ...} in the test call."
        ))
    }

    extra = args[!names(args) %in% fn_args]
    has_dots = "..." %in% names(formals(fn))
    if (has_dots) {
        rlang::exec(fn, !!!injected, !!!extra)
    } else {
        rlang::exec(fn, !!!injected)
    }
}
# inject_and_run = function(impl, processed, args, claims = NULL) {
#     fn = impl@fn
#     fn_formals = formals(fn)
#     fn_args = names(fn_formals)
#     fn_args = fn_args[fn_args != "..."]
#
#     injected = lapply(fn_args, function(arg) {
#         if (arg == ".proc") return(processed)
#         if (!is.null(args[[arg]])) return(args[[arg]])
#         # fn_formals[[arg]]
#         default = fn_formals[[arg]]
#         if (is.call(default)) {
#             return(eval(default, envir = environment(fn) %||% baseenv()))
#         }
#         default
#     })
#     names(injected) = fn_args
#
#     missing_args = vapply(injected, function(x) {
#         is.symbol(x) && identical(as.character(x), "")
#     }, logical(1))
#
#     if (any(missing_args)) {
#         cli::cli_abort(c(
#             "{sum(missing_args)} required argument{?s} not supplied: {.arg {fn_args[missing_args]}}.",
#             "i" = "Supply via {.code ...} in the test call."
#         ))
#     }
#
#     extra = args[!names(args) %in% fn_args]
#     has_dots = "..." %in% names(formals(fn))
#     if (has_dots) {
#         rlang::exec(fn, !!!injected, !!!extra)
#     } else {
#         rlang::exec(fn, !!!injected)
#     }
#     # rlang::exec(fn, !!!injected, !!!extra)
# }
