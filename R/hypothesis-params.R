#' Base class for population parameters
#'
#' `param_obj` is the base S7 class for all population parameter objects,
#' analogous to [model_id()]. Concrete subclasses (`MU`, `PI`, `SIGMA`,
#' `RHO`) inherit from it. The base class is a pure marker — each subclass
#' declares its own properties.
#'
#' @export
param_obj = S7::new_class("param_obj")

#' Mean of a variable, optionally conditioned on a subgroup
#'
#' @param x A bare variable name.
#' @param given An optional filter predicate as a bare expression.
#'
#' @return A `MU` / `param_obj` S7 object.
#'
#' @examples
#' MU(extra)
#' MU(extra, group == "1")
#'
#' @export
MU = S7::new_class(
    "MU",
    parent = param_obj,
    properties = list(
        x = S7::class_any,
        given = S7::class_any
    ),
    constructor = function(x, given = NULL) {
        S7::new_object(
            S7::S7_object(),
            x = rlang::enquo(x),
            given = if (missing(given)) NULL else rlang::enquo(given)
        )
    }
)

#' Proportion of a variable, optionally conditioned on a subgroup
#'
#' @param x An empty or a bare variable name.
#' @param given An optional filter predicate as a bare expression.
#'
#' @return A `PI` / `param_obj` S7 object.
#'
#' @examples
#' PI(success)
#' PI(success, group == "treatment")
#'
#' @export
PI = S7::new_class(
    "PI",
    parent = param_obj,
    properties = list(
        x = S7::class_any,
        given = S7::class_any
    ),
    constructor = function(x, given = NULL) {
        S7::new_object(
            S7::S7_object(),
            x = if (missing(x)) NULL else rlang::enquo(x),
            given = if (missing(given)) NULL else rlang::enquo(given)
        )
    }
)

#' Variance of a variable, optionally conditioned on a subgroup
#'
#' @param x A bare variable name.
#' @param given An optional filter predicate as a bare expression.
#'
#' @return A `SIGMA` / `param_obj` S7 object.
#'
#' @examples
#' SIGMA(score)
#' SIGMA(score, group == "control")
#'
#' @export
SIGMA = S7::new_class(
    "SIGMA",
    parent = param_obj,
    properties = list(
        x = S7::class_any,
        given = S7::class_any
    ),
    constructor = function(x, given = NULL) {
        S7::new_object(
            S7::S7_object(),
            x = rlang::enquo(x),
            given = if (missing(given)) NULL else rlang::enquo(given)
        )
    }
)

#' Population correlation between two variables
#'
#' @param x A bare variable name.
#' @param y A bare variable name.
#'
#' @return A `RHO` / `param_obj` S7 object.
#'
#' @examples
#' RHO(speed, dist)
#'
#' @export
RHO = S7::new_class(
    "RHO",
    parent = param_obj,
    properties = list(
        x = S7::class_any,
        y = S7::class_any
    ),
    constructor = function(x, y) {
        S7::new_object(
            S7::S7_object(),
            x = rlang::enquo(x),
            y = rlang::enquo(y)
        )
    }
)

S7::method(print, param_obj) = function(x, ...) {
    cls_name = S7::S7_class(x)@name
    cat(sprintf("<param: %s>\n", cls_name))

    nms = S7::prop_names(x)
    vals = lapply(nms, function(nm) S7::prop(x, nm))
    names(vals) = nms

    present = Filter(function(nm) !is.null(vals[[nm]]), nms)
    if (length(present) == 0L) {
        cat("\n")
        return(invisible(x))
    }

    max_w = max(nchar(present))
    cat("\n")
    for (nm in present) {
        val = vals[[nm]]
        lbl = if (rlang::is_quosure(val)) rlang::as_label(val) else as.character(val)
        cat(sprintf("-  %-*s => %s\n", max_w, nm, lbl))
    }
    cat("\n")
    invisible(x)
}

#' @keywords internal
parse_param_call = S7::new_generic("parse_param_call", "x")

S7::method(parse_param_call, MU) = function(x, args, env) {
    if (length(args) < 1L || length(args) > 2L) {
        cli::cli_abort(c(
            "{.fn MU} requires 1 or 2 arguments.",
            "i" = "Usage: {.code MU(x)} or {.code MU(x, given)}."
        ))
    }
    obj = MU(x = !!rlang::new_quosure(quote(.dummy), emptyenv()))
    S7::prop(obj, "x") = rlang::new_quosure(args[[1]], env)
    S7::prop(obj, "given") = if (length(args) == 2L) rlang::new_quosure(args[[2]], env) else NULL
    obj
}

S7::method(parse_param_call, PI) = function(x, args, env) {
    if (length(args) > 2L) {
        cli::cli_abort(c(
            "{.fn PI} accepts 0, 1, or 2 arguments.",
            "i" = "Usage: {.code PI()}, {.code PI(x)}, or {.code PI(x, given)}."
        ))
    }
    obj = PI()
    S7::prop(obj, "x") = if (length(args) >= 1L) rlang::new_quosure(args[[1]], env) else NULL
    S7::prop(obj, "given") = if (length(args) == 2L) rlang::new_quosure(args[[2]], env) else NULL
    obj
}

S7::method(parse_param_call, SIGMA) = function(x, args, env) {
    if (length(args) < 1L || length(args) > 2L) {
        cli::cli_abort(c(
            "{.fn SIGMA} requires 1 or 2 arguments.",
            "i" = "Usage: {.code SIGMA(x)} or {.code SIGMA(x, given)}."
        ))
    }
    obj = SIGMA(x = !!rlang::new_quosure(quote(.dummy), emptyenv()))
    S7::prop(obj, "x") = rlang::new_quosure(args[[1]], env)
    S7::prop(obj, "given") = if (length(args) == 2L) rlang::new_quosure(args[[2]], env) else NULL
    obj
}

S7::method(parse_param_call, RHO) = function(x, args, env) {
    if (length(args) != 2L) {
        cli::cli_abort(c(
            "{.fn RHO} requires exactly 2 arguments.",
            "i" = "Usage: {.code RHO(x, y)}."
        ))
    }
    obj = RHO(x = !!rlang::new_quosure(quote(.dummy), emptyenv()),
              y = !!rlang::new_quosure(quote(.dummy), emptyenv()))
    S7::prop(obj, "x") = rlang::new_quosure(args[[1]], env)
    S7::prop(obj, "y") = rlang::new_quosure(args[[2]], env)
    obj
}

#' @keywords internal
param_id_label = S7::new_generic("param_id_label", "x")

S7::method(param_id_label, MU) = function(x) {
    x_lbl = rlang::as_label(x@x)
    if (is.null(x@given)) {
        paste0("MU(", x_lbl, ")")
    } else {
        paste0("MU(", x_lbl, ", ", rlang::as_label(x@given), ")")
    }
}

S7::method(param_id_label, PI) = function(x) {
    if (is.null(x@x)) {
        if (is.null(x@given)) "PI()" else paste0("PI(, ", rlang::as_label(x@given), ")")
    } else {
        x_lbl = rlang::as_label(x@x)
        if (is.null(x@given)) {
            paste0("PI(", x_lbl, ")")
        } else {
            paste0("PI(", x_lbl, ", ", rlang::as_label(x@given), ")")
        }
    }
}

S7::method(param_id_label, SIGMA) = function(x) {
    x_lbl = rlang::as_label(x@x)
    if (is.null(x@given)) {
        paste0("SIGMA(", x_lbl, ")")
    } else {
        paste0("SIGMA(", x_lbl, ", ", rlang::as_label(x@given), ")")
    }
}

S7::method(param_id_label, RHO) = function(x) {
    paste0("RHO(", rlang::as_label(x@x), ", ", rlang::as_label(x@y), ")")
}
