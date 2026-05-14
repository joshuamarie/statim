#' Add or replace variants on a test function
#'
#' A family of functions for managing globally registered [variant()] objects
#' across all [HTEST_FN]-based functions for the duration of the session.
#'
#' @param test A test function built with [HTEST_FN()], e.g. [TTEST].
#' @param name A string naming the variant to add or replace.
#' @param impl A [variant()] object.
#' @param origin One of `"user"` (default, session-scoped) or `"package"`
#'   (permanent, intended for `.onLoad()`).
#' @param cls A string naming the test class to clear (e.g. `"ttest"`).
#'   When `NULL` (default), all globally registered variants are cleared.
#'
#' @section Precedence:
#' Globally registered variants sit between built-in definitions and the
#' pipeline. The full priority order, from lowest to highest, is:
#'
#' \enumerate{
#'   \item Built-in variants (declared inside [HTEST_FN])
#'   \item Global variants registered via `plug_variant()` or `swap_variant()`
#' }
#'
#' @return
#' `plug_variant()`, `swap_variant()`, and `clear_htest_defs()` return
#' `NULL` invisibly, called for their side effects.
#' `get_htest_defs()` returns a list of [test_define()] objects, or an
#' empty list if none have been registered for the given `cls`.
#'
#' @seealso [HTEST_FN()], [test_define()], [variant()]
#'
#' @name htest-defs-modifiers
NULL

#' @describeIn htest-defs-modifiers Adds a new named [variant()] to an
#'   existing test function. Hard-errors if the name already exists or
#'   if name is `"default"`.
#' @export
plug_variant = function(test, name, impl, origin = c("user", "package")) {
    origin = match.arg(origin)

    if (identical(name, "default")) {
        cli::cli_abort(
            "{.val default} is frozen and cannot be added via {.fn plug_variant}."
        )
    }
    if (!S7::S7_inherits(impl, variant)) {
        cli::cli_abort("{.arg impl} must be a {.cls variant} object.")
    }

    cls = attr(test, "cls") %||% cli::cli_abort(
        "{.arg test} must be a function built with {.fn HTEST_FN}."
    )

    entries = htest_opts_global$variants[[cls]] %||% list()
    already_exists = any(vapply(entries, function(e) identical(e$name, name), logical(1)))
    if (already_exists) {
        if (origin == "package") {
            # silent replace: .onLoad() hook re-registration on reload is expected
            # Once 'statim' is extended with different package
            # The registered different variation won't simply vanish
            # (Not yet tested)
            htest_opts_global$variants[[cls]] = lapply(entries, function(e) {
                if (identical(e$name, name)) {
                    list(name = name, impl = impl, origin = origin)
                } else
                    e
            })
            return(invisible(NULL))
        }

        cli::cli_abort(c(
            "Variant {.val {name}} already exists.",
            "i" = "Use {.fn swap_variant} to replace it."
        ))
    }

    entry = list(name = name, impl = impl, origin = origin)
    htest_opts_global$variants[[cls]] = c(entries, list(entry))
    invisible(NULL)
}

#' @describeIn htest-defs-modifiers Replaces an existing named [variant()].
#'   Hard-errors if the name does not exist or if name is `"default"`.
#' @export
swap_variant = function(test, name, impl, origin = c("user", "package")) {
    origin = match.arg(origin)

    if (identical(name, "default")) {
        cli::cli_abort(
            "{.val default} is frozen and cannot be swapped via {.fn swap_variant}."
        )
    }
    if (!S7::S7_inherits(impl, variant)) {
        cli::cli_abort("{.arg impl} must be a {.cls variant} object.")
    }

    cls = attr(test, "cls") %||% cli::cli_abort(
        "{.arg test} must be a function built with {.fn HTEST_FN}."
    )

    entries = htest_opts_global$variants[[cls]] %||% list()
    found = any(vapply(entries, function(e) identical(e$name, name), logical(1)))
    if (!found) {
        cli::cli_abort(c(
            "Variant {.val {name}} does not exist.",
            "i" = "Use {.fn plug_variant} to add it."
        ))
    }

    htest_opts_global$variants[[cls]] = lapply(entries, function(e) {
        if (identical(e$name, name)) list(name = name, impl = impl, origin = origin)
        else e
    })
    invisible(NULL)
}

#' #' @describeIn htest-defs-modifiers Returns registered [test_define()] objects
#' #'   for the given `cls`. Used internally by [HTEST_FN()].
#' #' @keywords internal
#' #' @export
#' get_htest_defs = function(cls = NULL) {
#'     if (is.null(cls)) return(list())
#'     entries = htest_opts_global$defs[[cls]] %||% list()
#'     lapply(entries, function(e) e$def)
#' }

#' @describeIn htest-defs-modifiers Resets globally registered variants,
#'   either fully or scoped to a specific `cls`. Only `"user"`-originated
#'   entries are removed — `"package"` entries are always preserved.
#' @keywords internal
#' @export
clear_htest_defs = function(cls = NULL) {
    keys = if (is.null(cls)) names(htest_opts_global$variants) else cls
    for (key in keys) {
        entries = htest_opts_global$variants[[key]] %||% list()
        htest_opts_global$variants[[key]] = Filter(
            function(e) e$origin == "package",
            entries
        )
    }
    invisible(NULL)
}

#' @keywords internal
#' @noRd
htest_opts_global = new.env(parent = emptyenv())
# htest_opts_global$defs = list()
htest_opts_global$variants = list()
