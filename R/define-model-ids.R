#' 'Variable compared by groups' model mapping
#'
#' Use this when you want to compare `x` by `group`.
#'
#' @param x The response variable (bare name).
#' @param group The grouping variable (bare name).
#'
#' @return An `x_by` / `model_id` S3 object.
#'
#' @examples
#' x_by(extra, group)
#'
#' @export
x_by = function(x, group) {
    args = rlang::enquos(x, group)
    model_id_class(args, "x_by")
}

#' 'Relationship between two variables' model mapping
#'
#' Use this when you want to define the relationship between two variables.
#'
#' @param x The predictor variable (bare name).
#' @param resp The response variable (bare name).
#'
#' @return A `rel` / `model_id` S3 object.
#'
#' @examples
#' rel(speed, dist)
#'
#' @export
rel = function(x, resp) {
    args = rlang::list2(
        x = rlang::enquo(x),
        resp = rlang::enquo(resp)
    )
    model_id_class(args, "rel")
}

#' 'Pairs between variables' model mapping
#'
#' Use this when you want to define all pairwise combinations of a set of
#' variables.
#'
#' @param ... Bare variable names to pair up.
#' @param direction A string controlling which pairs are kept. One of
#'   `"lt"` (default, strict lower-triangle), `"lteq"`, `"gt"`, `"gteq"`,
#'   `"eq"`, `"neq"`, or `"all"`.
#'
#' @return A `pairwise` / `model_id` S3 object.
#'
#' @examples
#' pairwise(a, b, c)
#' pairwise(a, b, c, direction = "all")
#'
#' @export
pairwise = function(..., direction = "lt") {
    dots = rlang::enquos(...)
    out = list(
        args = list(dots = rlang::expr(c(!!!dots))),
        direction = direction
    )
    model_id_class(out, "pairwise")
}

#' Attach a model-ID class to an object
#'
#' Low-level constructor used by [x_by()], [rel()], and [pairwise()].
#' Extension authors can use this to register custom model-ID types.
#'
#' @param obj A list representing the model ID payload.
#' @param clss A string giving the primary class name (e.g. `"x_by"`).
#'
#' @return `obj` with `class` set to `c(clss, "model_id")`.
#'
#' @export
model_id_class = function(obj, clss) {
    class(obj) = c(clss, "model_id")
    obj
}
