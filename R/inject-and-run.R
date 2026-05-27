#' Resolve and call the right fn from a baseline or variant,
#' injecting processed model output and user-supplied args.
#'
#' @param impl A `baseline` or `variant` S7 object.
#' @param processed Output of `model_processor()`.
#' @param args A named list of user-supplied arguments. They are the extra arguments.
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

    injected = lapply(
        fn_args,
        function(arg) {
            if (!is.null(args[[arg]])) return(args[[arg]])

            from_processed = resolve_from_processed(arg, processed)
            if (!is.null(from_processed)) return(from_processed)

            fn_formals[[arg]]
        }
    )
    names(injected) = fn_args

    missing_args = vapply(injected, function(x) {
        is.symbol(x) && identical(as.character(x), "")
    }, logical(1))

    if (any(missing_args)) {
        n_missing = sum(missing_args)
        cli::cli_abort(c(
            "{n_missing} required argument{?s} not supplied: {.arg {fn_args[missing_args]}}.",
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
    # rlang::exec(fn, !!!injected, !!!extra)
}

#' Map a single fn argument name to processed model output.
#' Tries direct match first, then appends `_data` suffix.
#'
#' @keywords internal
#' @noRd
resolve_from_processed = function(arg, processed) {
    if (!is.null(processed[[arg]])) return(processed[[arg]])

    with_suffix = paste0(arg, "_data")
    val = processed[[with_suffix]]
    if (!is.null(val)) {
        if ((is.data.frame(val) || is.list(val)) && length(val) == 1L) {
            return(val[[1]])
        }
        return(val)
    }

    NULL
}
