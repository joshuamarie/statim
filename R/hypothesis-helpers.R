contains_param = function(node) {
    if (S7::S7_inherits(node, param_obj)) return(TRUE)
    if (inherits(node, "arith_node")) {
        return(any(vapply(node$operands, contains_param, logical(1))))
    }
    FALSE
}

assert_linear = function(node, call_nm) {
    if (!inherits(node, "arith_node")) return(invisible(NULL))

    op = node$op
    ops = node$operands

    if (op == "*") {
        if (contains_param(ops[[1]]) && contains_param(ops[[2]])) {
            cli::cli_abort(c(
                "Non-linear hypothesis detected: parameter multiplied by parameter.",
                "i" = "{.fn {call_nm}} only handles linear combinations of parameters.",
                "x" = "Found: {.code {deparse(node$expr)}}."
            ))
        }
    }

    if (op == "/") {
        if (contains_param(ops[[2]])) {
            cli::cli_abort(c(
                "Non-linear hypothesis detected: parameter in denominator.",
                "i" = "{.fn {call_nm}} only handles linear combinations of parameters.",
                "x" = "Found: {.code {deparse(node$expr)}}."
            ))
        }
    }

    if (op == "^") {
        if (contains_param(ops[[1]])) {
            cli::cli_abort(c(
                "Non-linear hypothesis detected: parameter raised to a power.",
                "i" = "{.fn {call_nm}} only handles linear combinations of parameters.",
                "x" = "Found: {.code {deparse(node$expr)}}."
            ))
        }
    }

    lapply(ops, assert_linear, call_nm = call_nm)
    invisible(NULL)
}

collect_terms = function(node, sign = 1L, coef = 1) {
    if (is.numeric(node)) {
        return(list(list(kind = "scalar", value = sign * coef * node, node = node)))
    }

    if (S7::S7_inherits(node, param_obj)) {
        return(list(list(kind = "param", coef = sign * coef, node = node)))
    }

    if (inherits(node, "arith_node")) {
        op = node$op
        ops = node$operands

        if (op == "+") {
            return(c(
                collect_terms(ops[[1]], sign, coef),
                collect_terms(ops[[2]], sign, coef)
            ))
        }

        if (op == "-") {
            if (length(ops) == 1L) return(collect_terms(ops[[1]], -sign, coef))
            return(c(
                collect_terms(ops[[1]], sign, coef),
                collect_terms(ops[[2]], -sign, coef)
            ))
        }

        if (op == "*") {
            if (is.numeric(ops[[1]])) return(collect_terms(ops[[2]], sign, coef * ops[[1]]))
            return(collect_terms(ops[[1]], sign, coef * ops[[2]]))
        }

        if (op == "/") {
            return(collect_terms(ops[[1]], sign, coef / ops[[2]]))
        }
    }

    cli::cli_abort(
        "Cannot reduce term to a linear combination: {.code {deparse(node$expr %||% node)}}."
    )
}

#' Extract the hypothesized scalar value from a null claim
#'
#' Rearranges the hypothesis by moving all `param_obj` terms to the left
#' and all scalar terms to the right. Returns the resulting scalar and the
#' (possibly flipped) operator.
#'
#' Only handles linear combinations of parameters.
#'
#' @param claim A `null_claim` object.
#'
#' @return A list with fields `scalar` and `op`.
#'
#' @export
claim_scalar_diff = function(claim) {
    assert_linear(claim@lhs, "claim_scalar_diff")
    assert_linear(claim@rhs, "claim_scalar_diff")

    lhs_terms = collect_terms(claim@lhs, sign = 1L)
    rhs_terms = collect_terms(claim@rhs, sign = -1L)
    all_terms = c(lhs_terms, rhs_terms)

    param_terms = Filter(function(t) t$kind == "param", all_terms)
    scalar_terms = Filter(function(t) t$kind == "scalar", all_terms)

    if (length(param_terms) == 0L) {
        cli::cli_abort(c(
            "No population parameter found in hypothesis.",
            "i" = "At least one side must contain a parameter like {.fn MU}, {.fn PI}, etc."
        ))
    }

    scalar_val = -Reduce("+", lapply(scalar_terms, `[[`, "value"), 0)

    op = claim@op
    lhs_has_only_scalars = !any(vapply(lhs_terms, function(t) t$kind == "param", logical(1)))
    if (lhs_has_only_scalars && length(lhs_terms) > 0L) {
        op = unname(FLIP_OP[op])
    }

    list(scalar = scalar_val, op = op)
}

#' Extract contrast coefficients from a null claim
#'
#' Decomposes the hypothesis into a named numeric vector of coefficients,
#' one per `param_obj` term, plus the hypothesized scalar value and operator.
#'
#' @param claim A `null_claim` object.
#'
#' @return A list with fields `coefs`, `scalar`, and `op`.
#'
#' @export
claim_contrast_coefs = function(claim) {
    assert_linear(claim@lhs, "claim_contrast_coefs")
    assert_linear(claim@rhs, "claim_contrast_coefs")

    lhs_terms = collect_terms(claim@lhs, sign = 1L)
    rhs_terms = collect_terms(claim@rhs, sign = -1L)
    all_terms = c(lhs_terms, rhs_terms)

    param_terms = Filter(function(t) t$kind == "param", all_terms)
    scalar_terms = Filter(function(t) t$kind == "scalar", all_terms)

    if (length(param_terms) == 0L) {
        cli::cli_abort(c(
            "No population parameter found in hypothesis.",
            "i" = "At least one side must contain a parameter like {.fn MU}, {.fn PI}, etc."
        ))
    }

    nms = vapply(param_terms, function(t) extract_param_name(t$node), character(1))
    raw_coefs = vapply(param_terms, `[[`, numeric(1), "coef")
    names(raw_coefs) = nms

    unique_nms = unique(nms)
    coefs = vapply(unique_nms, function(nm) sum(raw_coefs[nms == nm]), numeric(1))
    names(coefs) = unique_nms

    zero_terms = names(coefs[coefs == 0])
    if (length(zero_terms) > 0L) {
        cli::cli_warn(c(
            "Zero-coefficient term{?s} in contrast: {.val {zero_terms}}.",
            "i" = "Duplicate parameters with opposite signs cancelled out.",
            "i" = "Verify the hypothesis is written as intended."
        ))
    }

    scalar_val = -Reduce("+", lapply(scalar_terms, `[[`, "value"), 0)

    op = claim@op
    lhs_has_only_scalars = !any(vapply(lhs_terms, function(t) t$kind == "param", logical(1)))
    if (lhs_has_only_scalars && length(lhs_terms) > 0L) {
        op = unname(FLIP_OP[op])
    }

    list(coefs = coefs, scalar = scalar_val, op = op)
}

extract_param_name = function(node) {
    if (!S7::S7_inherits(node, param_obj)) {
        cli::cli_abort("Expected a param_obj node.")
    }

    given = node@given

    if (!is.null(given)) {
        given_expr = rlang::quo_get_expr(given)
        if (rlang::is_call(given_expr, "==") && length(given_expr) == 3L) {
            return(as.character(given_expr[[3]]))
        }
        return(deparse(given_expr))
    }

    rlang::as_label(node@x)
}

#' Package resolved claim arguments for injection
#'
#' Used inside a `claim_translator` to declare argument names and values
#' merged into the impl's call. Names must match the formals of the impl's
#' `fn`.
#'
#' @param ... Named arguments to inject.
#'
#' @return A named list with class `"claim_args"`.
#'
#' @keywords internal
#' @noRd
claim_args = function(...) {
    args = list(...)
    if (length(args) == 0L || is.null(names(args)) || any(!nzchar(names(args)))) {
        cli::cli_abort("All arguments to {.fn claim_args} must be named.")
    }
    structure(args, class = "claim_args")
}
