#' Recalibrate the test method variant
#'
#' `via()` switches a lazy pipeline to an alternative method variant and
#' merges user-supplied arguments with the variant's declared defaults.
#'
#' @param .x A `test_lazy` object.
#' @param .method A string naming the method variant. Must match a named
#'   [variant()] in the [agendas()] of the matched [test_define()].
#'   E.g. `"boot"`, `"permute"`, `"permute_rfast"`.
#' @param ... Named arguments forwarded to the variant.
#'
#' @return The modified `test_lazy` object with `recalibrate_spec` populated.
#'
#' @seealso [conclude()], [test_define()]
#'
#' @examples
#' sleep |>
#'     define_model(x_by(extra, group)) |>
#'     prepare_test(TTEST) |>
#'     via("boot", n = 2000) |>
#'     conclude()
#'
#' sleep |>
#'     define_model(x_by(extra, group)) |>
#'     prepare_test(TTEST) |>
#'     via("permute_rfast", B = 999) |>
#'     conclude()
#'
#' @export
via = function(.x, .method, ...) {
    UseMethod("via")
}

#' @rdname via
#' @export
via.test_lazy = function(.x, .method, ...) {
    model_type = class(.x$model_id)[[1]]
    def = find_def(.x$test_spec$lookup, model_type = model_type)

    cls = .x$test_spec$cls
    global_names = vapply(
        htest_opts_global$variants[[cls]] %||% list(),
        function(e) e$name,
        character(1)
    )
    available = c(names(def@impl$variants), global_names)

    if (!.method %in% available) {
        cli::cli_abort(c(
            "No variant {.val {(.method)}} registered for model type {.val {model_type}}.",
            "i" = "Available variant{?s}: {.val {names(def@impl$variants)}}."
        ))
    }

    .x$recalibrate_spec = list(
        method_name = .method,
        args = list(...)
    )
    .x
}
