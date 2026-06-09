#' Base class for model ID objects
#'
#' `model_id` is the abstract parent class for all model ID objects in
#' `{statim}`. Model IDs emulate R's formula interface, as they capture
#' variable expressions without evaluating them, describing the structure
#' of a statistical model to be passed into a pipeline.
#'
#' Concrete subclasses include [x_by()], [rel()], and [pairwise()]. You
#' cannot instantiate `model_id` directly; use one of its subclasses.
#'
#' @format NULL
#' @usage NULL
#'
#' @seealso [x_by()], [rel()], [pairwise()], [prop()]
#'
#' @export
model_id = S7::new_class("model_id", abstract = TRUE)

#' Compare a variable by group
#'
#' `x_by()` (and its infix alias `%by%`) creates an `x_by` model ID that
#' reads as "compare `x` by `group`". Expressions are captured unevaluated,
#' similar to how [ggplot2::aes()] captures aesthetics.
#'
#' @param x The response variable. Accepts a bare name, a `c()` of bare
#'   names, a tidyselect helper (requires `data` in [define_model()]), or
#'   `I(expr)` for inline data.
#' @param group The grouping variable. Same rules as `x`.
#'
#' @return An `x_by` / `model_id` S7 object.
#'
#' @examples
#' # Bare names — resolved later from the data or environment
#' x_by(extra, group)
#'
#' # Infix alias: identical to x_by(extra, group)
#' extra %by% group
#'
#' # Inline data via I()
#' x_by(I(rnorm(30)), I(rep(c("a", "b"), each = 15)))
#'
#' # Named inline data
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

#' Describe the relationship between two variables
#'
#' `rel()` creates a `rel` model ID that reads as "relationship between
#' `x` and `resp`". Expressions are captured unevaluated, similar to how
#' [ggplot2::aes()] captures aesthetics.
#'
#' @param x The predictor variable. Accepts a bare name, a `c()` of bare
#'   names, a tidyselect helper (requires `data` in [define_model()]), or
#'   `I(expr)` for inline data.
#' @param resp The response variable. Same rules as `x`.
#'
#' @return A `rel` / `model_id` S7 object.
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

#' Define all pairwise variable combinations
#'
#' `pairwise()` creates a `pairwise` model ID from a set of variables,
#' producing all unique variable pairs. Use `direction` to control which
#' pairs are retained. Pairs are filtered by lexicographic (alphabetical)
#' ordering of variable names.
#'
#' @param ... Bare variable names, tidyselect helpers (requires `data` in
#'   [define_model()]), or `I(expr)` for inline data.
#' @param direction A string controlling which pairs are kept:
#'   - `"lt"` (default): strict lower triangle, i.e. pairs where index(x) < index(y)
#'   - `"lteq"`: lower triangle including the diagonal (x <= y)
#'   - `"gt"`: strict upper triangle (x > y)
#'   - `"gteq"`: upper triangle including the diagonal (x >= y)
#'   - `"eq"`: diagonal only (x == y), i.e. each variable paired with itself
#'   - `"neq"`: all pairs except the diagonal (x != y)
#'   - `"all"`: all combinations including both directions and the diagonal
#'
#' @return A `pairwise` / `model_id` S7 object.
#'
#' @examples
#' pairwise(a, b, c)
#'
#' # Inline data
#' pairwise(I(rnorm(30)), I(rnorm(30)), I(rnorm(30)))
#'
#' # Keep all ordered pairs
#' pairwise(a, b, c, direction = "all")
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

#' Define a proportion test model
#'
#' `prop()` creates a `prop` model ID for proportion tests. Both arguments
#' are scalar constants, and this implies the arguments are expressions
#' that are not captured.
#'
#' @param x Number of successes. A non-negative integer scalar, `x <= n`.
#' @param n Total number of trials. A positive integer scalar.
#'
#' @return A `prop` / `model_id` S7 object.
#'
#' @examples
#' prop(45, 100)
#'
#' @export
prop = S7::new_class(
    "prop",
    parent = model_id,
    properties = list(
        x = S7::new_property(
            class = S7::class_numeric,
            validator = function(value) {
                if (length(value) != 1L)
                    return(paste0("`x` must be a scalar, not length ", length(value), "."))
                if (!is.finite(value) || value != as.integer(value) || value < 0L)
                    "`x` must be a non-negative integer."
            }
        ),
        n = S7::new_property(
            class = S7::class_numeric,
            validator = function(value) {
                if (length(value) != 1L)
                    return(paste0("`n` must be a scalar, not length ", length(value), "."))
                if (!is.finite(value) || value != as.integer(value) || value < 1L)
                    "`n` must be a positive integer."
            }
        )
    ),
    constructor = function(x, n) {
        if (x > n)
            stop("`x` must not exceed `n`.")
        S7::new_object(S7::S7_object(), x = x, n = n)
    }
)

S7::method(print, model_id) = function(x, ...) {
    info = model_id_info(x)

    cat(cli::rule(left = "Model Definition", line = "-"), "\n\n")
    cat("Model ID :", info@model_type, "\n")
    cat("Args :", info@args, "\n")

    invisible(x)
}
