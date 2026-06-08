#' Validate that param nodes in a claim reference declared model variables
#'
#' Dispatches on the model ID class. Called inside `attach_claim_to_lazy`
#' after the compatible-param guard. Default method is a no-op, so model
#' types that carry no named columns (e.g. `prop`) pass through silently.
#'
#' @param model_id The model ID object from `lazy@model_id`.
#' @param processed The processed list from `lazy@processed`.
#' @param claims A single `null_claim` or a `null_claims` object.
#'
#' @keywords internal
validate_claim_vars = S7::new_generic("validate_claim_vars", "model_id")

S7::method(validate_claim_vars, model_id) = function(model_id, processed, claims) {
    cls_name = S7::S7_class(model_id)@name
    cli::cli_abort(c(
        "No {.fn validate_claim_vars} method defined for model ID {.cls {cls_name}}.",
        "i" = "Implement {.fn validate_claim_vars} for this model ID,",
        "i" = "or use {.fn validate_claim_vars_skip} to opt out explicitly."
    ))
}

S7::method(validate_claim_vars, x_by) = function(model_id, processed, claims) {
    x_vars = names(processed$x_data)
    by_vars = names(processed$group_data)
    check_param_nodes(claims, x_vars = x_vars, by_vars = by_vars)
}

S7::method(validate_claim_vars, rel) = function(model_id, processed, claims) {
    x_vars = names(processed$x_data)
    resp_vars = names(processed$resp_data)
    check_param_nodes(claims, x_vars = c(x_vars, resp_vars), by_vars = NULL)
}

S7::method(validate_claim_vars, pairwise) = function(model_id, processed, claims) {
    check_param_nodes(claims, x_vars = processed$var_names, by_vars = NULL)
}

S7::method(validate_claim_vars, prop) = function(model_id, processed, claims) {
    invisible(NULL)
}

S7::method(validate_claim_vars, S7::class_formula) = function(model_id, processed, claims) {
    declared_vars = processed$vars
    check_param_nodes(claims, x_vars = declared_vars, by_vars = NULL)
}

check_param_nodes = function(claims, x_vars, by_vars) {
    nodes = collect_param_nodes(
        if (S7::S7_inherits(claims, null_claims)) claims@claims else list(claims)
    )
    errors = lapply(nodes, \(vn) validate_one_param_node(vn, x_vars = x_vars, by_vars = by_vars)) |>
        unlist() |>
        unique()

    if (length(errors) > 0L) {
        errors = unique(errors)
        n = length(errors)
        cli::cli_abort(c(
            "Invalid variable reference{cli::qty(n)}{?s} in hypothesis:",
            stats::setNames(errors, rep("x", n))
        ))
    }
    invisible(NULL)
}

validate_one_param_node = function(node, x_vars, by_vars) {
    errors = character(0)
    cls_name = S7::S7_class(node)@name

    if (!is.null(x_vars) && !is.null(node@x)) {
        x_lbl = rlang::as_label(node@x)
        if (!x_lbl %in% x_vars) {
            errors = c(errors, cli::format_inline(
                "Unknown variable {.val {x_lbl}} in {.cls {cls_name}}. ",
                "Model declares x-variable{?s}: {.and {.val {x_vars}}}."
            ))
        }
    }

    if (!is.null(by_vars) && !is.null(node@given)) {
        given_expr = rlang::quo_get_expr(node@given)
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
