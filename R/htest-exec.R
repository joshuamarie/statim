#' Execute a lazy test pipeline
#'
#' `conclude()` is the terminal step of the pipeline. It resolves the
#' method variant, runs the implementation, and returns an `htest_spec`
#' object.
#'
#' @param .x A `test_lazy` object produced by [prepare_test()] (optionally
#'   followed by [via()]).
#' @param ... Currently unused.
#'
#' @return An `htest_spec` S3 object.
#'
#' @seealso [prepare_test()], [via()], [HTEST_FN()]
#'
#' @examples
#' sleep |>
#'     define_model(x_by(extra, group)) |>
#'     prepare_test(TTEST) |>
#'     conclude()
#'
#' sleep |>
#'     define_model(x_by(extra, group)) |>
#'     prepare_test(TTEST) |>
#'     via("boot", n = 2000) |>
#'     conclude()
#'
#' @export
conclude = function(.x, ...) {
    UseMethod("conclude")
}

#' @rdname conclude
#' @export
conclude.test_lazy = function(.x, ...) {
    model_type = class(.x$model_id)[[1]]
    def = find_def(.x$test_spec$lookup, model_type = model_type)

    method_name = .x$recalibrate_spec$method_name

    impl = if (!is.null(method_name)) {
        cls = .x$test_spec$cls
        global_variants = htest_opts_global$variants[[cls]] %||% list()
        global_impl = Filter(function(e) identical(e$name, method_name), global_variants)

        def@impl$variants[[method_name]] %||%
            global_impl[[1]]$impl %||%
            cli::cli_abort(c(
                "No variant {.val {method_name}} registered for model type {.val {model_type}}.",
                "i" = "Available variant{?s}: {.val {names(def@impl$variants)}}."
            ))
    } else {
        def@impl$base
    }

    all_args = utils::modifyList(
        .x$test_spec$args,
        .x$recalibrate_spec$args %||% list()
    )

    out_raw = inject_and_run(
        impl = impl,
        processed = .x$processed,
        args = all_args,
        claims = .x$claims
    )

    out = new_htest(
        out_raw,
        impl_cls = def@impl_class,
        test_cls = .x$test_spec$cls,
        print_fn = impl@print
    )
    out$name = .x$test_spec$name
    out
}
