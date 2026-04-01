#' Model evaluator
#'
#' A function for development use to extract the information in model IDs.
#'
#' @param x The model IDs to be extracted.
#' @param data The given data frame when supplied. It can be a `NULL` or a missing argument
#' @param ... Currently unused; passed through for S3 method compatibility.
#'
#' @name model-processor
#' @export
model_processor = function(x, ...) {
    UseMethod("model_processor")
}

#' @rdname model-processor
#' @export
model_processor.formula = function(x, data = NULL, ...) {
    vars = all.vars(x)
    data = if (rlang::is_null(data)) {
        vctrs::new_data_frame(
            rlang::set_names(
                lapply(
                    vars,
                    \(v) rlang::eval_tidy(
                        rlang::sym(v), env = rlang::f_env(x)
                    )
                ),
                vars
            )
        )
    } else {
        data
    }

    list(
        data = data,
        vars = all.vars(x),
        formula = x
    )
}

#' @rdname model-processor
#' @export
model_processor.x_by = function(x, data = NULL, ...) {
    proc = two_vars_extract(x, data)
    list(
        x_data = proc$x1_data,
        group_data = proc$x2_data
    )
}

#' @rdname model-processor
#' @export
model_processor.rel = function(x, data = NULL, ...) {
    proc = two_vars_extract(x, data)
    list(
        x_data = proc$x1_data,
        resp_data = proc$x2_data
    )
}

#' @rdname model-processor
#' @export
model_processor.pairwise = function(x, data = NULL, ...) {
    pairwise_data_extract(x, data)
}
