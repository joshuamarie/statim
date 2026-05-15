#' Model define constructor
#'
#' `define_model()` captures a model ID and optional data into a `def_model`
#' object that can be passed into [prepare_test()].
#'
#' @param .x A model ID object from [x_by()], [rel()], [pairwise()], or a
#'   formula — **or** a data frame when using the data-first pipe style.
#' @param data A data frame. When called on a model-ID object this defaults to
#'   `parent.frame()`, resolving bare variable names against the calling
#'   environment. When calling on a data frame, pass the model ID as
#'   `to_analyze`.
#' @param to_analyze A model ID or formula (only used in the
#'   `define_model.data.frame` method).
#' @param ... Currently unused.
#'
#' @return A `def_model` S3 object containing `model_id` and `processed`.
#'
#' @examples
#' # model-ID first
#' define_model(x_by(extra, group), sleep)
#'
#' # data-frame first (pipe-friendly)
#' sleep |> define_model(x_by(extra, group))
#'
#' @name model-define-base
#' @export
define_model = S7::new_generic("define_model", ".x")

S7::method(define_model, S7::new_union(S7::class_formula, model_id)) = function(.x, data = parent.frame(), ...) {
    def_model(
        model_id = .x,
        processed = model_processor(.x, data)
    )
}

S7::method(define_model, S7::class_data.frame) = function(.x, to_analyze, ...) {
    def_model(
        model_id = to_analyze,
        processed = model_processor(to_analyze, .x)
    )
}

def_model = S7::new_class(
    "def_model",
    properties = list(
        model_id = S7::class_any,
        processed = S7::class_list
    )
)

S7::method(print, def_model) = function(x, ...) {
    info = model_id_info(x@model_id, x@processed)

    cat("\n")
    cat(cli::rule(left = "Model Definition", line = "-"), "\n\n")
    cat("Model ID :", info$model_type, "\n")
    cat("Args :", info$args, "\n")

    cat("Other info:\n")
    for (nm in names(info$other_info)) {
        cat("   ", nm, ":", info$other_info[[nm]], "\n")
    }

    cat("Variables :\n")
    for (v in info$vars) {
        cat("   ", v$name, ":", v$preview, "\n")
    }

    cat("\n")
    invisible(x)
}
