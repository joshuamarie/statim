#' Attach a model-ID class to an object
#'
#'
#' @return `model_id` S7/S3 class.
#'
#' @export
model_id = S7::new_class("model_id")

#' 'Variable compared by groups' model mapping
#'
#' Use this when you want to compare `x` by `group`.
#'
#' @param x The response variable. A bare name, `c()` of bare names, a
#'   tidyselect helper (requires `data`), or `I(expr)` for inline data.
#' @param group The grouping variable. Same rules as `x`.
#'
#' @return An `x_by` / `model_id` S3 object.
#'
#' @examples
#' # bare names (resolved from environment or data)
#' x_by(extra, group)
#'
#' # inline data
#' x_by(I(rnorm(30)), I(rep(c("a", "b"), each = 15)))
#'
#' # named inline
#' x_by(I(score = rnorm(30)), I(grp = rep(c("a", "b"), each = 15)))
#'
#' @export
x_by = S7::new_class(
    "x_by",
    parent = model_id,
    properties = list(
        x = S7::class_any,
        group = S7::class_any
    ),
    constructor = function(x, group) {
        S7::new_object(
            S7::S7_object(),
            x = rlang::enquo(x),
            group = rlang::enquo(group)
        )
    }
)

#' @rdname x_by
#' @export
`%by%` = x_by

#' 'Relationship between two variables' model mapping
#'
#' Use this when you want to define the relationship between two variables.
#'
#' @param x The predictor variable. A bare name, `c()` of bare names, a
#'   tidyselect helper (requires `data`), or `I(expr)` for inline data.
#' @param resp The response variable. Same rules as `x`.
#'
#' @return A `rel` / `model_id` S3 object.
#'
#' @examples
#' rel(speed, dist)
#'
#' @export
rel = S7::new_class(
    "rel",
    parent = model_id,
    properties = list(
        x = S7::class_any,
        resp = S7::class_any
    ),
    constructor = function(x, resp) {
        S7::new_object(
            S7::S7_object(),
            x = rlang::enquo(x),
            resp = rlang::enquo(resp)
        )
    }
)

#' 'Pairs between variables' model mapping
#'
#' Use this when you want to define all pairwise combinations of a set of
#' variables.
#'
#' @param ... Bare variable names, tidyselect helpers (requires `data`), or
#'   `I(expr)` for inline data.
#' @param direction A string controlling which pairs are kept. One of
#'   `"lt"` (default), `"lteq"`, `"gt"`, `"gteq"`, `"eq"`, `"neq"`,
#'   or `"all"`.
#'
#' @return A `pairwise` / `model_id` S3 object.
#'
#' @examples
#' pairwise(a, b, c)
#' pairwise(I(rnorm(30)), I(rnorm(30)), I(rnorm(30)))
#'
#' @export
pairwise = S7::new_class(
    "pairwise",
    parent = model_id,
    properties = list(
        dots = S7::class_any,
        dots_quos = S7::new_property(S7::class_list),
        direction = S7::new_property(S7::class_character, default = "lt")
    ),
    constructor = function(..., direction = "lt") {
        dots = rlang::enquos(...)
        S7::new_object(
            S7::S7_object(),
            dots = rlang::expr(c(!!!dots)),
            dots_quos = dots,
            direction = direction
        )
    }
)

S7::method(print, model_id) = function(x, ...) {
    info = model_id_info(x)

    cat(cli::rule(left = "Model Definition", line = "-"), "\n\n")
    cat("Model ID :", info$model_type, "\n")
    cat("Args :", info$args, "\n")

    invisible(x)
}
