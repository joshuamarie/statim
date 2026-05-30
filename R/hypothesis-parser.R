RELATIONAL_OPS = c("==", "!=", "<", ">", "<=", ">=", "%=%")

FLIP_OP = c(
    "==" = "!=",
    "!=" = "==",
    "<" = ">=",
    ">" = "<=",
    "<=" = ">",
    ">=" = "<",
    "%=%" = "%!=%"
)

parse_null_claim = function(quo) {
    expr = rlang::quo_get_expr(quo)
    env = rlang::quo_get_env(quo)

    op = validate_top_level_op(expr)

    # %=% chains are left-associative in R:
    # MU(a) %=% MU(b) %=% MU(c) parses as ((MU(a) %=% MU(b)) %=% MU(c))
    # We flatten all operands recursively into a list and store in lhs.
    # rhs is NULL for %=% claims.
    if (op == "%=%") {
        nodes = flatten_peq(expr, env)
        return(null_claim(
            lhs = nodes,
            rhs = NULL,
            op = "%=%",
            alt_op = "%!=%",
            expr = expr
        ))
    }

    lhs = parse_param_node(expr[[2]], env, side = "LHS")
    rhs = parse_param_node(expr[[3]], env, side = "RHS")

    null_claim(
        lhs = lhs,
        rhs = rhs,
        op = op,
        alt_op = unname(FLIP_OP[op]),
        expr = expr
    )
}

#' Recursively flatten a left-associative %=% chain into a list of nodes.
#'
#' ((MU(a) %=% MU(b)) %=% MU(c)) → list(MU(a), MU(b), MU(c))
#'
#' @keywords internal
#' @noRd
flatten_peq = function(expr, env) {
    if (rlang::is_call(expr, "%=%")) {
        lhs_nodes = flatten_peq(expr[[2]], env)
        rhs_expr = expr[[3]]
        if (rlang::is_call(rhs_expr, "(")) rhs_expr = rhs_expr[[2]]
        rhs_node = parse_param_node(rhs_expr, env, side = "RHS")
        c(lhs_nodes, list(rhs_node))
    } else if (rlang::is_call(expr, "(")) {
        list(parse_param_node(expr[[2]], env, side = "LHS"))
    } else {
        list(parse_param_node(expr, env, side = "LHS"))
    }
}

validate_top_level_op = function(expr) {
    if (!rlang::is_call(expr)) {
        cli::cli_abort(c(
            "A hypothesis must be a comparison expression.",
            "i" = "Expected one of: {.code {RELATIONAL_OPS}}.",
            "x" = "Got: {.code {deparse(expr)}}."
        ))
    }
    op = as.character(expr[[1]])
    if (!op %in% RELATIONAL_OPS) {
        cli::cli_abort(c(
            "Unsupported operator {.code {op}} in hypothesis.",
            "i" = "Use one of: {.code {RELATIONAL_OPS}}."
        ))
    }
    op
}

is_param_class = function(x) {
    if (!inherits(x, "S7_class")) return(FALSE)
    cls = x
    while (!is.null(cls)) {
        if (identical(cls, param_obj)) return(TRUE)
        cls = cls@parent
    }
    FALSE
}

parse_param_node = function(expr, env, side = "expression") {
    if (is.numeric(expr) || is.integer(expr)) return(as.numeric(expr))

    if (rlang::is_call(expr)) {
        fn_name = as.character(expr[[1]])
        args = as.list(expr[-1])

        proto = tryCatch(
            get(fn_name, envir = env, inherits = TRUE),
            error = function(e) NULL
        )

        if (is.null(proto)) {
            proto = tryCatch(
                get(fn_name, envir = asNamespace("statim"), inherits = FALSE),
                error = function(e) NULL
            )
        }

        if (!is.null(proto) && is_param_class(proto)) {
            return(parse_param_call(proto(), args = args, env = env))
        }

        if (fn_name %in% c("+", "-", "*", "/", "^")) {
            return(build_arith_node(expr, env, side))
        }
    }

    if (is.symbol(expr)) {
        val = tryCatch(eval(expr, envir = env), error = function(e) NULL)
        if (is.numeric(val)) return(val)
    }

    cli::cli_abort(c(
        "Cannot parse {side} of hypothesis: {.code {deparse(expr)}}.",
        "i" = "Each side must be a population parameter, a numeric scalar,",
        "i" = "or an arithmetic combination of these."
    ))
}

build_arith_node = function(expr, env, side) {
    op = as.character(expr[[1]])
    operands = lapply(as.list(expr[-1]), function(a) parse_param_node(a, env, side))
    structure(list(op = op, operands = operands, expr = expr), class = "arith_node")
}

param_node_label = function(x) {
    if (is.numeric(x)) return(as.character(x))
    if (inherits(x, "arith_node")) {
        ops = vapply(x$operands, param_node_label, character(1))
        if (length(ops) == 1L) {
            paste0(x$op, ops[[1]])
        } else {
            paste0(ops[[1]], " ", x$op, " ", ops[[2]])
        }
    } else {
        param_id_label(x)
    }
}
