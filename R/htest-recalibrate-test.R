#' Recalibrate the test method variant
#'
#' `via()` switches a lazy pipeline to an alternative method variant (e.g.
#' bootstrap, permutation) and merges user-supplied arguments with the
#' variant's declared defaults.
#'
#' @param .x A `test_lazy` or `engine_set` object.
#' @param .method A string naming the method variant. Must match the
#'   `name` passed to [method_spec()] in one of the registered
#'   [test_define()] objects. E.g. `"boot"`, `"permute"`.
#' @param ... Named arguments forwarded to the method (override defaults).
#' @param engine A string naming the engine to use. Defaults to the engine
#'   already set by [through()], or `"default"` if none was set.
#'
#' @return The modified `test_lazy` object with `recalibrate_spec` populated.
#'
#' @seealso [through()], [conclude()], [method_spec()], [test_define()]
#'
#' @examples
#' sleep |>
#'     define_model(x_by(extra, group)) |>
#'     prepare_test(TTEST) |>
#'     via("boot", n = 2000) |>
#'     conclude()
#'
#' @export
via = function(.x, .method, ...) {
    UseMethod("via")
}

#' @rdname via
#' @export
via.test_lazy = function(.x, .method, ..., engine = NULL) {
    dots = list(...)
    engine = engine %||% .x$engine %||% "default"

    key = paste0(class(.x$model_id)[[1]], "::", .method, "::", engine)
    def = .x$test_spec$lookup[[key]] %||% cli::cli_abort(
        "No implementation for method {.val {(.method)}}."
    )

    method_args = utils::modifyList(
        def@method@defaults,
        dots
    )

    .x$recalibrate_spec = list(
        method_name = .method,
        engine = engine,
        args = method_args
    )
    .x
}

#' @rdname via
#' @export
via.engine_set = function(.x, .method, ..., engine = NULL) {
    engine = engine %||% .x$engine %||% "default"
    class(.x) = class(.x)[class(.x) != "engine_set"]
    via.test_lazy(.x, .method, ..., engine = engine)
}
