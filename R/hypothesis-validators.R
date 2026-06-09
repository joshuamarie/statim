#' Validate hypothesis parameter references against a model's declared variables
#'
#' An S7 generic dispatched on the model ID class. Called automatically inside
#' [state_null()] after the compatible-param guard. Implement a method for any
#' new [model_id] subclass you define.
#'
#' @param model_id A <[model_id]> object (usually carried by [define_model()]).
#' @param processed The processed list from `lazy@processed`, as returned by
#'   [model_processor()].
#' @param claims A single `null_claim` or a `null_claims` object.
#' @param ... Currently unused.
#'
#' @return `invisible(NULL)`, or aborts with a consolidated error.
#'
#' @section Implementing a new method:
#' By default, unknown model ID subclasses pass through without validation.
#' To add validation for a new [model_id] subclass, implement a method and
#' delegate to `check_param_nodes()`:
#'
#' ``` r
#' S7::method(validate_claim_vars, <model_id>) = function(model_id, processed, claims) {
#'     check_param_nodes(
#'         claims,
#'         x_vars = names(processed$x_data),
#'         by_vars = names(processed$group_data)
#'     )
#' }
#' ```
#'
#' @seealso [state_null()], [MU()], [RHO()], [PI()], [param_obj()], [model_id()]
#'
#' @name claim-vars-validators
#' @export
validate_claim_vars = S7::new_generic(
    "validate_claim_vars",
    "model_id",
    fun = function(model_id, processed, claims, ...) S7::S7_dispatch()
)

S7::method(validate_claim_vars, model_id) = function(model_id, processed, claims, ...) {
    invisible(NULL)
}

S7::method(validate_claim_vars, x_by) = function(model_id, processed, claims, ...) {
    check_param_nodes(
        claims,
        x_vars = names(processed$x_data),
        by_vars = names(processed$group_data)
    )
}

S7::method(validate_claim_vars, rel) = function(model_id, processed, claims, ...) {
    check_param_nodes(
        claims,
        x_vars = c(names(processed$x_data), names(processed$resp_data)),
        by_vars = NULL
    )
}

S7::method(validate_claim_vars, pairwise) = function(model_id, processed, claims, ...) {
    check_param_nodes(
        claims,
        x_vars = processed$var_names,
        by_vars = NULL
    )
}

S7::method(validate_claim_vars, prop) = function(model_id, processed, claims, ...) {
    invisible(NULL)
}

S7::method(validate_claim_vars, S7::class_formula) = function(model_id, processed, claims) {
    check_param_nodes(
        claims,
        x_vars = processed$vars,
        by_vars = NULL
    )
}

#' @details
#' `check_param_nodes()` is the shared walker used by all built-in
#' `validate_claim_vars()` methods. It collects every `param_obj` node,
#' runs `validate_one_param_node()` on each, deduplicates errors, and aborts
#' with a consolidated message. Call this inside your own
#' `validate_claim_vars()` method rather than re-implementing the walking and
#' accumulation logic.
#'
#' @param x_vars A character vector of declared x-variable names, or `NULL`
#'   to skip x-variable validation.
#' @param by_vars A character vector of declared grouping variable names, or
#'   `NULL` to skip grouping variable validation.
#'
#' @rdname claim-vars-validators
#' @export
check_param_nodes = function(claims, x_vars, by_vars) {
    nodes = collect_param_nodes(
        if (S7::S7_inherits(claims, null_claims))
            claims@claims else list(claims)
    )
    errors = lapply(
        nodes,
        validate_one_param_node,
        x_vars = x_vars,
        by_vars = by_vars
    ) |>
        unlist() |>
        unique()

    if (length(errors) > 0L) {
        n = length(errors)
        cli::cli_abort(c(
            "Invalid variable reference{cli::qty(n)}{?s} in hypothesis:",
            stats::setNames(errors, rep("x", n))
        ))
    }
    invisible(NULL)
}

#' @details
#' `validate_one_param_node()` is an S7 generic dispatched on `param_obj`.
#' Returns a character vector of error strings — empty if valid. The default
#' method on `param_obj` returns `character(0)`, so unknown subclasses pass
#' through safely. Implement a method for any new `param_obj` subclass whose
#' slots should be checked against the model's declared variables.
#'
#' @param node A `param_obj` instance.
#'
#' @section Implementing `validate_one_param_node` for a new param class:
#' For a subclass with `x` and `given` slots, delegate to
#' [check_x_and_given()]:
#'
#' ```r
#' S7::method(validate_one_param_node, MY_PARAM) = function(node, x_vars, by_vars) {
#'     check_x_and_given(node@x, node@given, x_vars, by_vars, "MY_PARAM")
#' }
#' ```
#'
#' For a subclass with two variable slots (like [RHO()]):
#'
#' ```r
#' S7::method(validate_one_param_node, MY_PARAM) = function(node, x_vars, by_vars) {
#'     errors = character(0)
#'     for (slot_quo in list(node@x, node@y)) {
#'         lbl = rlang::as_label(slot_quo)
#'         if (!is.null(x_vars) && !lbl %in% x_vars) {
#'             errors = c(errors, cli::format_inline(
#'                 "Unknown variable {.val {lbl}} in {.cls MY_PARAM}. ",
#'                 "Model declares x-variable{?s}: {.and {.val {x_vars}}}."
#'             ))
#'         }
#'     }
#'     errors
#' }
#' ```
#'
#' @rdname claim-vars-validators
#' @export
validate_one_param_node = S7::new_generic("validate_one_param_node", "node")

S7::method(validate_one_param_node, param_obj) = function(node, x_vars, by_vars) {
    character(0)
}

S7::method(validate_one_param_node, MU) = function(node, x_vars, by_vars) {
    check_x_and_given(node@x, node@given, x_vars, by_vars, "MU")
}

S7::method(validate_one_param_node, PI) = function(node, x_vars, by_vars) {
    check_x_and_given(node@x, node@given, x_vars, by_vars, "PI")
}

S7::method(validate_one_param_node, SIGMA) = function(node, x_vars, by_vars) {
    check_x_and_given(node@x, node@given, x_vars, by_vars, "SIGMA")
}

S7::method(validate_one_param_node, RHO) = function(node, x_vars, by_vars) {
    errors = character(0)
    for (slot_quo in list(node@x, node@y)) {
        lbl = rlang::as_label(slot_quo)
        if (!is.null(x_vars) && !lbl %in% x_vars) {
            errors = c(errors, cli::format_inline(
                "Unknown variable {.val {lbl}} in {.cls RHO}. ",
                "Model declares x-variable{?s}: {.and {.val {x_vars}}}."
            ))
        }
    }
    errors
}

#' @details
#' `check_x_and_given()` is the standard building block for
#' `validate_one_param_node()` methods on `param_obj` subclasses that follow
#' the `MU(x, given)` slot convention. It checks an `x` quosure against
#' `x_vars` and a `given` quosure against `by_vars`.
#'
#' @param x_quo A quosure holding the `x` slot value, or `NULL`.
#' @param given_quo A quosure holding the `given` slot value, or `NULL`.
#' @param cls_name A string naming the param class, used in error messages.
#'
#' @rdname claim-vars-validators
#' @export
check_x_and_given = function(x_quo, given_quo, x_vars, by_vars, cls_name) {
    errors = character(0)

    if (!is.null(x_vars) && !is.null(x_quo)) {
        x_lbl = rlang::as_label(x_quo)
        if (!x_lbl %in% x_vars) {
            errors = c(errors, cli::format_inline(
                "Unknown variable {.val {x_lbl}} in {.cls {cls_name}}. ",
                "Model declares x-variable{?s}: {.and {.val {x_vars}}}."
            ))
        }
    }

    if (!is.null(by_vars) && !is.null(given_quo)) {
        given_expr = rlang::quo_get_expr(given_quo)
        if (rlang::is_call(given_expr, "==") && length(given_expr) == 3L) {
            by_lbl = as.character(given_expr[[2]])
            if (!by_lbl %in% by_vars) {
                errors = c(errors, cli::format_inline(
                    "Unknown grouping variable {.val {by_lbl}} in {.cls {cls_name}}. ",
                    "Model declares by-variable{?s}: {.and {.val {by_vars}}}."
                ))
            }
        }
    }

    errors
}
