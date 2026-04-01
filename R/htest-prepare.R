#' Lazily prepare a single test
#'
#' `prepare_test()` attaches a test specification to a `def_model` object,
#' producing a `test_lazy` ready for optional recalibration with [via()] or
#' [through()] before being executed with [conclude()].
#'
#' @param .x A `def_model` object from [define_model()].
#' @param .test A test function such as [TTEST], or a `test_spec` object
#'   returned by calling such a function with no arguments.
#' @param ... Additional arguments passed to methods.
#'
#' @return A `test_lazy` S3 object.
#'
#' @seealso [define_model()], [via()], [through()], [conclude()]
#'
#' @examples
#' sleep |>
#'     define_model(x_by(extra, group)) |>
#'     prepare_test(TTEST) |>
#'     conclude()
#'
#' @name prepare-test
#' @export
prepare_test = function(.x, .test, ...) {
    UseMethod("prepare_test")
}

#' @rdname prepare-test
#' @export
prepare_test.def_model = function(.x, .test, ...) {
    spec = as_test_spec(.test)

    out = list(
        model_id = .x$model_id,
        processed = .x$processed,
        test_spec = spec,
        recalibrate_spec = NULL,
        claims = NULL
    )
    class(out) = "test_lazy"
    out
}

#' @importFrom stats update
#' @export
update.test_lazy = function(object, ...) {
    dots = list(...)
    if (!is.null(object$recalibrate_spec)) {
        object$recalibrate_spec$args = utils::modifyList(
            object$recalibrate_spec$args, dots
        )
    } else {
        object$test_spec$args = utils::modifyList(
            object$test_spec$args, dots
        )
    }
    object
}

as_test_spec = function(.test) {
    if (inherits(.test, "test_spec")) return(.test)

    if (is.function(.test)) {
        spec = .test(.model = NULL)
        if (!inherits(spec, "test_spec"))
            cli::cli_abort("{.arg .test} must return a {.cls test_spec}.")
        return(spec)
    }

    cli::cli_abort("{.arg .test} must be a function or {.cls test_spec}.")
}

