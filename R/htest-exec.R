#' Execute a lazy test pipeline
#'
#' `conclude()` is the terminal step of the pipeline. It resolves the
#' method variant and engine, builds the execution context, runs the
#' implementation, and returns an `htest_spec` object.
#'
#' @param .x A `test_lazy` or `engine_set` object produced by
#'   [prepare_test()] (optionally followed by [through()] and/or [via()]).
#' @param ... Currently unused.
#'
#' @return An `htest_spec` S3 object.
#'
#' @details
#' The engine and method variant are resolved in this order:
#' 1. If [via()] was called, its `method_name` and `engine` win.
#' 2. If [through()] was called (producing an `engine_set`), its engine is
#'    used with method `""` (the classical path).
#' 3. Otherwise `engine = "default"` and `method = ""` are used.
#'
#' @seealso [prepare_test()], [through()], [via()], [HTEST_FN()]
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
    method_name = .x$recalibrate_spec$method_name %||% ""
    engine = .x$recalibrate_spec$engine %||%
        .x$engine %||%
        "default"
    model_type = class(.x$model_id)[[1]]

    def = find_def(
        .x$test_spec$lookup,
        model_type = model_type,
        method_name = method_name,
        engine = engine
    )

    method_args = if (!is.null(.x$recalibrate_spec)) {
        .x$recalibrate_spec$args
    } else {
        list()
    }

    context = infer_context(
        processed = .x$processed,
        args = .x$test_spec$args,
        extractors = def@vars,
        fun_args = def@fun_args,
        claims = .x$claims,
        method_args = method_args
    )

    out_raw = def@run(context)
    out = new_htest(
        out_raw,
        impl_cls = def@impl_class,
        test_cls = .x$test_spec$cls,
        def = def
    )
    out$name = .x$test_spec$name
    out
}

#' @rdname conclude
#' @export
conclude.engine_set = function(.x, ...) {
    if (!is.null(.x$recalibrate_spec)) {
        .x$recalibrate_spec$engine = .x$engine

        method_name = .x$recalibrate_spec$method_name
        engine = .x$engine
        model_type  = class(.x$model_id)[[1]]

        def = find_def(
            .x$test_spec$lookup,
            model_type = model_type,
            method_name = method_name,
            engine = engine
        )

        method_args = utils::modifyList(
            def@method@defaults,
            .x$recalibrate_spec$args
        )

        context = infer_context(
            processed = .x$processed,
            args = .x$test_spec$args,
            extractors = def@vars,
            claims = .x$claims,
            fun_args = def@fun_args,
            method_args = method_args
        )

        out_raw = def@run(context)
        out = new_htest(
            out_raw,
            impl_cls = def@impl_class,
            test_cls = .x$test_spec$cls,
            def = def
        )
        out$name = .x$test_spec$name
        return(out)
    }

    NextMethod()
}
