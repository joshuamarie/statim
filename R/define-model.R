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
define_model = function(.x, ...) {
    if (inherits(.x, "formula")) {
        class(.x) = c(class(.x), "model_id")
    }
    UseMethod("define_model")
}

#' @rdname model-define-base
#' @export
define_model.model_id = function(.x, data = parent.frame(), ...) {
    metad = model_processor(.x, data)

    model_id = if (inherits(.x, "formula")) {
        out = list(formula = metad$formula)
        class(out) = c("formula", "model_id")
        out
    } else {
        .x
    }

    out = list(
        model_id = model_id,
        # options = vctrs::vec_c(...),
        processed = metad
    )
    class(out) = "def_model"
    out
}

#' @rdname model-define-base
#' @export
define_model.data.frame = function(.x, to_analyze, ...) {
    metad = model_processor(to_analyze, .x)

    model_id = if (inherits(to_analyze, "formula")) {
        out = list(formula = metad$formula)
        class(out) = c("formula", "model_id")
        out
    } else {
        to_analyze
    }

    out = list(
        model_id = model_id,
        # options = vctrs::vec_c(...),
        processed = metad
    )
    class(out) = "def_model"
    out
}

#' @keywords internal
#' @export
print.def_model = function(x, ...) {
    info = model_id_info(x$model_id, x$processed)

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
