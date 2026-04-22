#' H-test definition modifiers
#'
#' A family of functions for managing globally registered [test_define] objects
#' across all [HTEST_FN]-based functions (e.g. [TTEST]) for the duration of
#' the session.
#'
#' @param defs A [test_define] object or a list of [test_define] objects to be
#'   referenced globally. Each element must be a valid S7 `test_define` instance —
#'   passing anything else raises an error.
#' @param cls A string naming the test class to clear (e.g. `"ttest"`). When
#'   `NULL` (default), all globally registered definitions are cleared.
#'
#' @section Precedence:
#' Globally registered defs sit between built-in definitions and per-call
#' overrides. The full priority order, from lowest to highest, is:
#'
#' \enumerate{
#'   \item Built-in defs (declared inside [HTEST_FN])
#'   \item Global defs registered via `add_htest_defs()`
#'   \item Per-call defs passed via `.extra_defs`
#' }
#'
#' When two defs share the same key (`model_type::method::engine`), the
#' higher-priority entry wins at lookup time.
#'
#' @return
#' - `add_htest_defs()` and `clear_htest_defs()` return `NULL` invisibly,
#'   called for their side effects on `htest_opts_global$defs`.
#' - `get_htest_defs()` returns a list of [test_define] objects, or an empty
#'   list if none have been registered for the given `cls`.
#'
#' @seealso [HTEST_FN()], [test_define()]
#'
#' @examples
#' \donttest{
#' \dontrun{
#' my_def = test_define(
#'     model_type = "x_by",
#'     engine = "custom",
#'     # ...
#' )
#'
#' add_htest_defs(my_def)
#'
#' # my_def is now available in a current environment
#' # no `.extra_defs` needed
#' TTEST(x_by(extra, group), sleep)
#'
#' # Inspect what is registered under "ttest"
#' get_htest_defs("ttest")
#'
#' # Clear only ttest-scoped defs
#' clear_htest_defs("ttest")
#'
#' # Clear everything
#' clear_htest_defs()
#' }
#' }
#'
#' @name htest-defs-modifiers
NULL

#' @describeIn htest-defs-modifiers Registers one or more [test_define] objects
#'   into the global H-test store. The `cls` key is derived automatically from
#'   each def's `impl_class` prefix (e.g. `"ttest_permute_rfast"` routes into
#'   `"ttest"`), scoping it to the correct [HTEST_FN]-based function.
#' @export
add_htest_defs = function(defs) {
    defs = standardize_extra_defs(defs)
    lapply(defs, function(def) {
        key = strsplit(def@impl_class, "_")[[1]][[1]]
        htest_opts_global$defs[[key]] = c(htest_opts_global$defs[[key]], list(def))
    })
    invisible(NULL)
}

#' @describeIn htest-defs-modifiers Returns the list of [test_define] objects
#'   currently registered under the given `cls` key. Primarily used internally
#'   by [HTEST_FN()] but exported for inspection and testing.
#' @keywords internal
#' @export
get_htest_defs = function(cls = NULL) {
    if (is.null(cls)) return(list())
    htest_opts_global$defs[[cls]] %||% list()
}

#' @describeIn htest-defs-modifiers Resets the global H-test store, either
#'   fully or scoped to a specific `cls`. Subsequent calls to [HTEST_FN]-based
#'   functions will fall back to their built-in definitions only.
#' @keywords internal
#' @export
clear_htest_defs = function(cls = NULL) {
    if (is.null(cls)) {
        htest_opts_global$defs = list()
    } else {
        htest_opts_global$defs[[cls]] = list()
    }
    invisible(NULL)
}

#' H-test global definitions store
#'
#' A private, package-level environment that acts as a mutable global store
#' for [test_define] objects across all [HTEST_FN]-based functions.
#'
#' @details
#' `htest_opts_global` is intentionally unexported. Users interact with it
#' only through [add_htest_defs()], [get_htest_defs()], and
#' [clear_htest_defs()]. It holds a single field, `defs`, which is a named
#' list of [test_define] objects keyed by their `cls` prefix, accumulated
#' over the session.
#'
#' @keywords internal
#' @noRd
htest_opts_global = new.env(parent = emptyenv())
htest_opts_global$defs = list()

