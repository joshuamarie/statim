#' State a null hypothesis in the pipeline
#'
#' `state_null()` captures a hypothesis expression and attaches it to a
#' `test_lazy` object. Accepts either a single expression or a `more_h0()`
#' block for multiple named hypotheses.
#'
#' @param .x A `test_lazy` object from [prepare_test()].
#' @param expr A hypothesis expression, or a `more_h0()` block.
#'
#' @return The modified `test_lazy` object.
#'
#' @examples
#' sleep |>
#'     define_model(extra ~ group) |>
#'     prepare_test(TTEST) |>
#'     state_null(MU(extra) == 0) |>
#'     conclude()
#'
#' sleep |>
#'     define_model(extra ~ group) |>
#'     prepare_test(TTEST) |>
#'     state_null(more_h0(
#'         h01 = MU(extra) == 2,
#'         h02 = MU(extra) == 2 * h01
#'     )) |>
#'     conclude()
#'
#' @export
state_null = S7::new_generic("state_null", ".x")

S7::method(state_null, test_lazy) = function(.x, expr, ...) {
    raw = rlang::enquo(expr)
    expr_val = rlang::quo_get_expr(raw)
    env = rlang::caller_env()
    claim = if (rlang::is_call(expr_val, "more_h0")) {
        eval_more_h0(expr_val, env)
    } else {
        parse_null_claim(rlang::new_quosure(expr_val, env))
    }
    lazy = attach_claim_to_lazy(.x, claim)
    stated_null(
        model_id = lazy@model_id,
        processed = lazy@processed,
        test_spec = lazy@test_spec,
        recalibrate_spec = lazy@recalibrate_spec,
        claims = lazy@claims,
        data_name = lazy@data_name
    )
}

stated_null = S7::new_class(
    "stated_null",
    parent = test_lazy
)

S7::method(print, stated_null) = function(x, ...) {
    cat("\n")
    print(x@model_id)

    cat("\n")
    cat(cli::rule(left = "Test Specification", line = "-"), "\n\n")
    cat("Test   :", x@test_spec@name, "\n")

    method = x@recalibrate_spec$method_name %||% "default"
    cat("Method :", method, "\n")

    if (!is.null(x@recalibrate_spec$args)) {
        method_args = Filter(Negate(is.null), x@recalibrate_spec$args)
        if (length(method_args) > 0L) {
            args_str = paste(
                names(method_args),
                vapply(method_args, as.character, character(1)),
                sep = " = ",
                collapse = ", "
            )
            cat("Args   :", args_str, "\n")
        }
    }

    if (!is.null(x@claims)) {
        cat("\n")
        print(x@claims)
    }

    cat("\n")
    invisible(x)
}

#' @export
write_claims = function(...) {
    quos = rlang::enquos(...)
    if (length(quos) == 0L) {
        cli::cli_abort("Supply at least one hypothesis expression.")
    }
    claims = lapply(quos, parse_null_claim)
    null_claims(
        claims = claims,
        expr = rlang::expr(write_claims(!!!lapply(quos, rlang::quo_get_expr)))
    )
}

#' @export
more_h0 = function(...) {
    cli::cli_abort(
        "{.fn more_h0} must be used inside {.fn state_null}."
    )
}

#' @keywords internal
#' @noRd
eval_more_h0 = function(expr, env) {
    args = as.list(expr[-1])
    nms = names(args)

    if (is.null(nms) || any(!nzchar(nms))) {
        cli::cli_abort(
            "All expressions in {.fn more_h0} must be named."
        )
    }

    ref_env = new.env(parent = env)

    claims = vector("list", length(args))
    names(claims) = nms

    for (i in seq_along(args)) {
        nm = nms[[i]]
        raw = rlang::new_quosure(args[[i]], ref_env)
        claim = parse_null_claim(raw)

        scalar_val = tryCatch(
            claim_scalar_diff(claim),
            error = function(e) {
                structure(list(msg = conditionMessage(e)), class = "unresolvable")
            }
        )

        if (inherits(scalar_val, "unresolvable")) {
            local({
                captured_nm = nm
                captured_msg = scalar_val$msg
                makeActiveBinding(captured_nm, function() {
                    cli::cli_abort(c(
                        "Cannot reference {.val {captured_nm}} in a subsequent claim.",
                        "i" = "Only scalar-reducible claims can be referenced by name.",
                        "x" = captured_msg
                    ))
                }, ref_env)
            })
        } else {
            assign(nm, scalar_val, envir = ref_env)
        }

        claims[[i]] = claim
    }

    null_claims(claims = claims, expr = expr)
}

#' @keywords internal
attach_claim_to_lazy = function(lazy, claim) {
    if (!S7::S7_inherits(lazy, test_lazy)) {
        cli::cli_abort(c(
            "{.fn state_null} in pipe mode expects a {.cls test_lazy} object.",
            "i" = "Did you forget {.fn prepare_test} before {.fn state_null}?"
        ))
    }

    model_type = if (inherits(lazy@model_id, "formula")) {
        "formula"
    } else {
        S7::S7_class(lazy@model_id)@name
    }

    def = find_def(lazy@test_spec@lookup, model_type = model_type)

    if (is.null(def@claim_translator)) {
        cli::cli_abort(c(
            "The {.val {lazy@test_spec@name}} implementation for {.val {model_type}}",
            "does not support hypothesis claims.",
            "i" = "No {.fn claim_translator} is defined for this test."
        ))
    }

    # ---- Compatible param guard ----
    allowed = def@compatible_params
    if (length(allowed) > 0L) {
        claims_list = if (S7::S7_inherits(claim, null_claims)) {
            claim@claims
        } else {
            list(claim)
        }

        used_nodes = collect_param_nodes(claims_list)
        bad = Filter(
            function(node) !any(vapply(allowed, function(cl) S7::S7_inherits(node, cl), logical(1))),
            used_nodes
        )

        if (length(bad) > 0L) {
            bad_nms = unique(vapply(bad, function(n) S7::S7_class(n)@name, character(1)))
            allowed_nms = vapply(allowed, function(cl) cl@name, character(1))
            param_label = if (length(bad_nms) == 1L) "parameter" else "parameters"
            cli::cli_abort(c(
                "Invalid {param_label} for {.val {lazy@test_spec@name}}: {.and {.cls {bad_nms}}}.",
                "i" = "This test only supports: {.and {.cls {allowed_nms}}}."
            ))
        }
    }

    lazy@claims = claim
    lazy
}

#' Helper function to guard the `state_null()`
#'
#' Walk all param nodes across a list of null_claim objects and return a flat
#' list of every param_obj instance encountered (not deduplicated — callers
#' filter by class, so duplicates are harmless and cheap to produce).
#'
#' @keywords internal
#' @noRd
collect_param_nodes = function(claims_list) {
    unlist(lapply(claims_list, function(cl) {
        nodes = if (cl@op == "%=%") cl@lhs else list(cl@lhs, cl@rhs)
        unlist(lapply(nodes, param_nodes_from_node), recursive = FALSE)
    }), recursive = FALSE)
}

# Recursively collect param_obj instances from a node tree.
#
#' @keywords internal
#' @noRd
param_nodes_from_node = function(node) {
    if (S7::S7_inherits(node, param_obj)) return(list(node))
    if (inherits(node, "arith_node")) {
        return(unlist(lapply(node$operands, param_nodes_from_node), recursive = FALSE))
    }
    list()
}

# ---- null_claim / null_claims ----

null_claim = S7::new_class(
    "null_claim",
    properties = list(
        lhs = S7::class_any,
        rhs = S7::class_any,
        op = S7::new_property(S7::class_character),
        alt_op = S7::new_property(S7::class_character),
        expr = S7::class_any
    )
)

null_claims = S7::new_class(
    "null_claims",
    properties = list(
        claims = S7::new_property(S7::class_list),
        expr = S7::class_any
    )
)

S7::method(print, null_claim) = function(x, ...) {
    cat("\n")
    cat(cli::rule(left = "Null Hypothesis", line = "-"), "\n\n")
    cat("H\u2080 :", deparse(x@expr), "\n")

    if (x@op == "%=%") {
        labels = vapply(x@lhs, param_node_label, character(1))
        cat("Op  :", x@op, "\n")
        cat("Params :\n")
        for (lbl in labels) cat("  -", lbl, "\n")
    } else {
        cat("LHS :", param_node_label(x@lhs), "\n")
        cat("Op  :", x@op, "\n")
        cat("RHS :", param_node_label(x@rhs), "\n")
    }

    cat("\n")
    invisible(x)
}

S7::method(print, null_claims) = function(x, ...) {
    cat("\n")
    cat(cli::rule(left = "Null Hypotheses", line = "-"), "\n\n")
    for (i in seq_along(x@claims)) {
        cl = x@claims[[i]]
        if (cl@op == "%=%") {
            labels = vapply(cl@lhs, param_node_label, character(1))
            cat(sprintf("  [%d] H\u2080 : %s\n", i, paste(labels, collapse = " %=% ")))
        } else {
            cat(sprintf(
                "  [%d] H\u2080 : %s %s %s\n",
                i,
                param_node_label(cl@lhs),
                cl@op,
                param_node_label(cl@rhs)
            ))
        }
    }
    cat("\n")
    invisible(x)
}
