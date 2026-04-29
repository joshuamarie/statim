#' Lazily prepare a single test
#'
#' `prepare_test()` attaches a test specification to a `def_model` object,
#' producing a `test_lazy` ready for optional recalibration with [via()]
#' before being executed with [conclude()].
#'
#' @param .x A `def_model` object from [define_model()].
#' @param .test A test function such as [TTEST], or a `test_spec` object
#'   returned by calling such a function with no arguments.
#' @param ... Additional arguments passed to methods.
#'
#' @return A `test_lazy` S3 object.
#'
#' @seealso [define_model()], [via()], [conclude()]
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

#' @keywords internal
#' @export
print.test_lazy = function(x, ...) {
    cat("\n")
    print(x$model_id)

    cat("\n")
    cat(cli::rule(left = "Test Specification", line = "-"), "\n\n")
    cat("Test :", x$test_spec$name, "\n")

    method = x$recalibrate_spec$method_name %||% "default"
    cat("Method :", method)

    if (!is.null(x$recalibrate_spec)) {
        method_args = Filter(Negate(is.null), x$recalibrate_spec$args)
        if (length(method_args) > 0L) {
            args_str = paste(
                names(method_args),
                vapply(method_args, as.character, character(1)),
                sep = " = ",
                collapse = ", "
            )
            cat(" (", args_str, ")", sep = "")
        }
    }
    cat("\n")

    cat("\n")
    invisible(x)
}

#' Recalibrate arguments from the main pipeline
#'
#' `update()` modifies the arguments of a lazy test pipeline without
#' changing the method variant or engine.
#'
#' @param object A `test_lazy` object.
#' @param ... Named arguments to update.
#'
#' @return The modified `test_lazy` object.
#'
#' @examples
#' sleep |>
#'     define_model(extra ~ group) |>
#'     prepare_test(TTEST) |>
#'     update(.paired = TRUE) |>
#'     conclude()
#'
#' @importFrom stats update
#' @export
update.test_lazy = function(object, ...) {
    dots = list(...)
    if (!is.null(object$recalibrate_spec)) {
        object$recalibrate_spec$args = utils::modifyList(
            object$recalibrate_spec$args,
            dots
        )
    } else {
        object$test_spec$args = utils::modifyList(
            object$test_spec$args,
            dots
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
