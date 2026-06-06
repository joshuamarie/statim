test_lazy = S7::new_class(
    "test_lazy",
    properties = list(
        model_id = S7::new_property(
            class = S7::new_union(model_id, S7::class_formula)
        ),
        processed = S7::new_property(class = S7::class_list),
        test_spec = S7::new_property(class = test_spec),
        recalibrate_spec = S7::new_property(default = NULL),
        claims = S7::new_property(default = NULL),
        data_name = S7::new_property(class = S7::class_character, default = "")
    )
)

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
prepare_test = S7::new_generic("prepare_test", dispatch_args = c(".x", ".test"))
# prepare_test = function(.x, .test, ...) {
#     UseMethod("prepare_test")
# }

S7::method(prepare_test, list(def_model, S7::class_function)) = function(.x, .test, ...) {
    spec = as_test_spec(.test)
    dots = list(...)
    test_lazy(
        model_id = .x@model_id,
        processed = .x@processed,
        test_spec = spec,
        recalibrate_spec = if (length(dots) > 0L) list(args = dots) else NULL
    )
}

S7::method(print, test_lazy) = function(x, ...) {
    cat("\n")
    print(x@model_id)

    cat("\n")
    cat(cli::rule(left = "Test Specification", line = "-"), "\n\n")
    cat("Test   :", x@test_spec@name, "\n")

    method = x@recalibrate_spec$method_name %||% "default"
    cat("Method :", method, "\n")

    if (!is.null(x@recalibrate_spec$args)) {
        method_args = Filter(Negate(is.null), x@recalibrate_spec$args)
        if (length(method_args) > 0L) {
            args_str = paste(
                names(method_args),
                vapply(method_args, as.character, character(1)),
                sep = " = ",
                collapse = ", "
            )
            cat("Args   :", args_str, "\n")
        }
    }

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
#' @name update-test-lazy
#' @keywords internal
NULL

S7::method(update, test_lazy) = function(object, ...) {
    dots = list(...)
    if (!is.null(object@recalibrate_spec)) {
        object@recalibrate_spec$args = utils::modifyList(
            object@recalibrate_spec$args,
            dots
        )
    } else {
        object@test_spec@args = utils::modifyList(
            object@test_spec@args,
            dots
        )
    }
    object
}

as_test_spec = function(.test) {
    if (!is.function(.test)) {
        cli::cli_abort("{.arg .test} must be a function.")
    }

    spec = .test(.model = NULL)

    if (S7::S7_inherits(spec, model_spec)) {
        cli::cli_abort(c(
            "{.arg .test} is a model function, not a test function.",
            "i" = "Did you mean {.fn prepare_model}?"
        ))
    }

    if (!S7::S7_inherits(spec, test_spec)) {
        cli::cli_abort("{.arg .test} must return a {.cls test_spec}.")
    }

    spec
}

# as_test_spec = function(.test) {
#     if (inherits(.test, "test_spec")) return(.test)
#
#     if (is.function(.test)) {
#         spec = .test(.model = NULL)
#         if (!inherits(spec, "test_spec"))
#             cli::cli_abort("{.arg .test} must return a {.cls test_spec}.")
#         return(spec)
#     }
#
#     cli::cli_abort("{.arg .test} must be a function or {.cls test_spec}.")
# }
