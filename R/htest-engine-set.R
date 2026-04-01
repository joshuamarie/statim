#' Set the computational engine for a test pipeline
#'
#' `through()` sets the engine without changing the method variant.
#' The engine is inherited by [via()] if called afterwards.
#'
#' @param .x A `test_lazy` object.
#' @param engine A string naming the engine. E.g. `"cpp"`, `"rust"`.
#' @param ... Additional engine-level arguments.
#'
#' @return A `test_lazy` object.
#'
#' @seealso [via()], [conclude()], [update()]
#'
#' @examples
#' # The "cpp" engine is hypothetical; replace with a registered engine name.
#' \dontrun{
#' sleep |>
#'     define_model(x_by(extra, group)) |>
#'     prepare_test(TTEST) |>
#'     through("cpp") |>
#'     conclude()
#'
#' sleep |>
#'     define_model(x_by(extra, group)) |>
#'     prepare_test(TTEST) |>
#'     through("cpp") |>
#'     via("boot", n = 2000) |>
#'     conclude()
#' }
#'
#' @export
through = function(.x, engine, ...) {
    UseMethod("through")
}

#' @rdname through
#' @export
through.test_lazy = function(.x, engine, ...) {
    .x$engine = engine
    .x$engine_args = list(...)
    class(.x) = c("engine_set", class(.x))
    .x
}
