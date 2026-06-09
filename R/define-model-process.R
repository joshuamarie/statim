#' Model evaluator
#'
#' A function for development use to extract the information in model IDs.
#'
#' @param x The model IDs to be extracted.
#' @param ... Passed through S7 method compatibility.
#'
#' @details
#' Methods accept an optional `data` argument — a data frame, or `NULL`
#' to resolve variables from the calling environment.
#'
#' @name model-processor
#' @export
model_processor = S7::new_generic("model_processor", "x")

S7::method(model_processor, model_id) = function(x, data = NULL, ...) {
    list()
}

S7::method(model_processor, S7::class_formula) =
    function(x, data = NULL, ...) {
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

S7::method(model_processor, x_by) = function(x, data = NULL, ...) {
    proc = two_vars_extract(x@x, x@group, data = data, role2 = "group")
    list(
        x_data = proc$x1_data,
        group_data = proc$x2_data
    )
}

S7::method(model_processor, rel) = function(x, data = NULL, ...) {
    proc = two_vars_extract(x@x, x@resp, data = data, role2 = "resp")
    list(
        x_data = proc$x1_data,
        resp_data = proc$x2_data
    )
}

S7::method(model_processor, pairwise) = function(x, data = NULL, ...) {
    pairwise_data_extract(x, data)
}

S7::method(model_processor, prop) = function(x, ...) {
    list(x = x@x, n = x@n)
}

