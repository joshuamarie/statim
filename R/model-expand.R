expanded_model = S7::new_class(
    "expanded_model",
    properties = list(
        models = S7::new_property(class = S7::class_list),
        labels = S7::new_property(class = S7::class_character)
    )
)

#' Write multiple model definitions from a data frame
#'
#' `write_models()` evaluates named model expressions sequentially against
#' `.data`, so each name is available to subsequent expressions via
#' [stats::update()]. Accepts any valid model ID: formulas, [rel()],
#' [x_by()], or any registered `model_id` type.
#'
#' Sits between a data frame and [prepare_model()] or [prepare_test()]
#' in the pipeline.
#'
#' @param .data A data frame.
#' @param ... Named model expressions. Each must evaluate to a formula or
#'   a `model_id` object. Names are used as row labels in [anova()]
#'   output.
#'
#' @return An `expanded_model` object.
#'
#' @seealso [prepare_model()], [anova()]
#'
#' @examples
#' # explicit formulas
#' LifeCycleSavings |>
#'     write_models(
#'         f1 = sr ~ 1,
#'         f2 = sr ~ pop15,
#'         f3 = sr ~ pop15 + pop75,
#'         f4 = sr ~ pop15 + pop75 + dpi,
#'         f5 = sr ~ pop15 + pop75 + dpi + ddpi
#'     ) |>
#'     prepare_model(LINEAR_REG) |>
#'     anova()
#'
#' # update() chain
#' LifeCycleSavings |>
#'     write_models(
#'         f1 = sr ~ 1,
#'         f2 = update(f1, ~. + pop15),
#'         f3 = update(f2, ~. + pop75),
#'         f4 = update(f3, ~. + dpi),
#'         f5 = update(f4, ~. + ddpi)
#'     ) |>
#'     prepare_model(LINEAR_REG) |>
#'     anova()
#'
#' # mixed model_id types
#' LifeCycleSavings |>
#'     write_models(
#'         mod0 = rel(pop15, sr),
#'         f1 = sr ~ 1,
#'         f2 = update(f1, ~. + pop15)
#'     ) |>
#'     prepare_model(LINEAR_REG) |>
#'     anova()
#'
#' @export
write_models = S7::new_generic("write_models", ".data")

S7::method(write_models, S7::class_data.frame) = function(.data, ...) {
    quos = rlang::enquos(...)
    nms = names(quos)

    if (is.null(nms) || any(!nzchar(nms))) {
        cli::cli_abort("All arguments to {.fn write_models} must be named.")
    }

    env = rlang::new_data_mask(rlang::new_environment(parent = rlang::caller_env()))

    models = vector("list", length(quos))
    names(models) = nms

    for (i in seq_along(quos)) {
        val = rlang::eval_tidy(quos[[i]], data = env)
        env[[nms[[i]]]] = val
        models[[i]] = def_model(
            model_id = val,
            processed = model_processor(val, .data)
        )
    }

    expanded_model(models = models, labels = nms)
}

S7::method(prepare_model, list(expanded_model, S7::class_function)) = function(.x, .model_fn, ...) {
    spec = as_model_spec(.model_fn)
    models = lapply(.x@models, function(dm) {
        model_lazy(
            model_id = dm@model_id,
            processed = dm@processed,
            model_spec = spec
        )
    })
    anova_lazy(models = models, labels = .x@labels, args = list())
}

S7::method(prepare_test, list(expanded_model, S7::class_function)) = function(.x, .test, ...) {
    spec = as_test_spec(.test)
    models = lapply(.x@models, function(dm) {
        test_lazy(
            model_id = dm@model_id,
            processed = dm@processed,
            test_spec = spec
        )
    })
    anova_lazy(models = models, labels = .x@labels, args = list())
}
