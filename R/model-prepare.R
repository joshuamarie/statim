model_lazy = S7::new_class(
    "model_lazy",
    properties = list(
        model_id = S7::new_property(
            class = S7::new_union(model_id, S7::class_formula)
        ),
        processed = S7::new_property(class = S7::class_list),
        model_spec = S7::new_property(class = model_spec),
        recalibrate_spec = S7::new_property(default = NULL),
        data_name = S7::new_property(class = S7::class_character, default = "")
    )
)

#' Lazily prepare a model inference
#'
#' `prepare_model()` attaches a model specification to a `def_model` object,
#' producing a `model_lazy` ready for optional recalibration with [via()]
#' before being executed with [conclude()].
#'
#' @param .x A `def_model` object from [define_model()].
#' @param .model_fn A model function such as `LINEAR_REG()`.
#' @param ... Additional arguments passed to methods.
#'
#' @return A `model_lazy` S3 object.
#'
#' @seealso [prepare_test()], [define_model()], [via()], [conclude()]
#'
#' @examples
#' mtcars |>
#'     define_model(rel(mpg, wt)) |>
#'     prepare_model(LINEAR_REG) |>
#'     conclude()
#'
#' @name prepare-model
#' @export
prepare_model = S7::new_generic("prepare_model", dispatch_args = c(".x", ".model_fn"))
# prepare_model = function(.x, .model_fn, ...) {
#     UseMethod("prepare_model")
# }

S7::method(prepare_model, list(def_model, S7::class_function)) = function(.x, .model_fn, ...) {
    spec = as_model_spec(.model_fn)
    model_lazy(
        model_id = .x@model_id,
        processed = .x@processed,
        model_spec = spec
    )
}

S7::method(print, model_lazy) = function(x, ...) {
    cat("\n")
    print(x@model_id)

    cat("\n")
    cat(cli::rule(left = "Model Specification", line = "-"), "\n\n")
    cat("Model  :", x@model_spec@name, "\n")

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

S7::method(update, model_lazy) = function(object, ...) {
    dots = list(...)
    if (!is.null(object@recalibrate_spec)) {
        object@recalibrate_spec$args = utils::modifyList(
            object@recalibrate_spec$args,
            dots
        )
    } else {
        object@model_spec@args = utils::modifyList(
            object@model_spec@args,
            dots
        )
    }
    object
}

as_model_spec = function(.model_fn) {
    if (!is.function(.model_fn)) {
        cli::cli_abort("{.arg .model_fn} must be a function.")
    }

    spec = .model_fn(.model = NULL)

    if (S7::S7_inherits(spec, test_spec)) {
        cli::cli_abort(c(
            "{.arg .model_fn} is a test function, not a model function.",
            "i" = "Did you mean {.fn prepare_test}?"
        ))
    }

    if (!S7::S7_inherits(spec, model_spec)) {
        cli::cli_abort("{.arg .model_fn} must return a {.cls model_spec}.")
    }

    spec
}
