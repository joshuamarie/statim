#' Inline multiple expressions in a model ID
#'
#' `inlines()` is the multi-expression analogue of `c()` for inline data.
#' Where `c(x1, x2)` selects multiple columns by name, `inlines()` holds
#' multiple inline expressions evaluated immediately.
#'
#' @param ... Named or unnamed expressions. Unnamed elements are auto-named
#'   based on their role and position (e.g. `xv1`, `xv2` under role `"x"`).
#'
#' @examples
#' x_by(inlines(rnorm(30), rnorm(30)), I(rep(c("a", "b"), each = 15)))
#' x_by(inlines(x1 = rnorm(30), rnorm(30)), group)
#'
#' @export
inlines = function(...) {
    rlang::enquos(...)
}

#' Classify a single quosure into one of five input types.
#'
#' @param quo A quosure from `rlang::enquo()` / `rlang::enquos()`.
#' @return A list with fields `type`, `expr`, `env`.
#' @keywords internal
#' @noRd
classify_quo = function(quo) {
    expr = rlang::quo_get_expr(quo)
    env = rlang::quo_get_env(quo)

    type = if (rlang::is_missing(expr)) {
        ":error"
    } else if (is.symbol(expr)) {
        ":symbol"
    } else if (rlang::is_call(expr, "I")) {
        ":i_call"
    } else if (rlang::is_call(expr, "c")) {
        args = as.list(expr[-1])
        all_symbols = all(vapply(args, is.symbol, logical(1)))
        if (!all_symbols) ":error" else ":c_call"
    } else if (rlang::is_call(expr, "inlines")) {
        ":inlines_call"
    } else if (rlang::is_call(expr)) {
        fn_name = as.character(expr[[1]])
        if (fn_name %in% getNamespaceExports("tidyselect")) {
            ":tidyselect"
        } else {
            ":error"
        }
    } else {
        ":error"
    }

    list(type = type, expr = expr, env = env)
}

#' Resolve a single quosure to a named data frame.
#'
#' @param quo A quosure.
#' @param data A data frame, or `NULL`.
#' @param role Role name used for auto-naming (`"x"`, `"group"`, `"resp"`,
#'   `"p"` for pairwise).
#' @param idx Integer index used only for pairwise and `inlines()` auto-naming.
#' @keywords internal
#' @noRd
resolve_quo = function(quo, data = NULL, role = "x", idx = 1L) {
    cl = classify_quo(quo)

    switch(
        cl$type,

        ":symbol" = {
            nm = as.character(cl$expr)
            if (is.null(data) || is.environment(data)) {
                val = tryCatch(
                    rlang::eval_tidy(quo),
                    error = function(e) {
                        rlang::abort(
                            c(
                                paste0("Object `", nm, "` not found in the calling environment."),
                                "i" = "Supply a data frame as `data`:",
                                "i" = paste0("define_model(x_by(", nm, ", ...), data)")
                            ),
                            class = "check_missing_data",
                            parent = e
                        )
                    }
                )
                vctrs::new_data_frame(rlang::set_names(list(val), nm))
            } else {
                dplyr::select(data, dplyr::all_of(nm))
            }
        },

        ":c_call" = {
            syms = as.list(cl$expr[-1])
            nms = vapply(syms, as.character, character(1))
            if (is.null(data) || is.environment(data)) {
                vals = lapply(seq_along(syms), function(i) {
                    tryCatch(
                        rlang::eval_tidy(rlang::new_quosure(syms[[i]], cl$env)),
                        error = function(e) {
                            rlang::abort(
                                c(
                                    paste0("Object `", nms[[i]], "` not found in the calling environment."),
                                    "i" = "Supply a data frame as `.data`:",
                                    "i" = paste0("define_model(x_by(c(", paste(nms, collapse = ", "), "), ...), data)")
                                ),
                                class = "check_missing_data",
                                parent = e
                            )
                        }
                    )
                })
                vctrs::new_data_frame(rlang::set_names(vals, nms))
            } else {
                dplyr::select(data, dplyr::all_of(nms))
            }
        },

        ":tidyselect" = {
            if (is.null(data) || is.environment(data)) {
                cli::cli_abort(c(
                    "tidyselect helpers require a data frame.",
                    "i" = "Supply {.arg data} or use bare variable names instead."
                ))
            }
            cols = tidyselect::eval_select(cl$expr, data = data)
            data[, cols, drop = FALSE]
        },

        ":i_call" = {
            inner = cl$expr[[2]]
            user_nm = if (!is.null(names(cl$expr)) && nzchar(names(cl$expr)[[2]])) {
                names(cl$expr)[[2]]
            } else {
                NULL
            }
            val = rlang::eval_tidy(rlang::new_quosure(inner, cl$env))
            nm = user_nm %||% auto_name(role, idx)
            vctrs::new_data_frame(rlang::set_names(list(val), nm))
        },

        ":inlines_call" = {
            resolve_inlines(cl, data = data, role = role)
        },

        ":error" = {
            expr_lbl = rlang::as_label(quo)
            cli::cli_abort(c(
                "Invalid input in model ID: {.code {expr_lbl}}.",
                "i" = "Wrap inline expressions with {.fn I}: {.code I({expr_lbl})}.",
                "i" = "Use bare names or {.code c()} for column references."
            ))
        }
    )
}

#' Resolve an `inlines()` call to a named multi-column data frame.
#'
#' @details
#' `inlines()` returns a list of quosures when called normally,
#' but here we see its *call* expression — re-evaluate it to get the quosures
#'
#' @param cl A classified quosure list with `type == ":inlines_call"`.
#' @param data A data frame or `NULL`.
#' @param role Role name for auto-naming.
#' @keywords internal
#' @noRd
resolve_inlines = function(cl, data = NULL, role = "x") {
    inner_quos = rlang::eval_tidy(
        rlang::new_quosure(cl$expr, cl$env)
    )

    user_nms = names(inner_quos)

    cols = lapply(seq_along(inner_quos), function(i) {
        q = inner_quos[[i]]
        nm = if (!is.null(user_nms) && nzchar(user_nms[[i]])) {
            user_nms[[i]]
        } else {
            auto_name(role, i)
        }
        val = rlang::eval_tidy(q)
        rlang::set_names(list(val), nm)
    })

    vctrs::new_data_frame(do.call(c, cols))
}

auto_name = function(role, idx) {
    paste0(role, "v", idx)
}

two_vars_extract = function(args, data = NULL) {
    if (length(args) != 2L) {
        cli::cli_abort("This model ID requires exactly 2 arguments.")
    }

    roles = names(args)

    x1_df = resolve_quo(args[[1]], data = data, role = roles[[1]], idx = 1L)
    x2_df = resolve_quo(args[[2]], data = data, role = roles[[2]], idx = 1L)

    list(x1_data = x1_df, x2_data = x2_df)
}

pairwise_data_extract = function(args, data = NULL) {
    direction = args$direction
    dots_quos = args$args$dots_quos

    resolved = lapply(seq_along(dots_quos), function(i) {
        resolve_quo(dots_quos[[i]], data = data, role = "p", idx = i)
    })

    var_names = vapply(resolved, \(df) names(df)[[1]], character(1))
    selected_data = do.call(cbind, resolved)

    pairs = pairs_generator(
        var_names,
        direction = direction,
        simplify = TRUE
    )

    list(
        var_names = var_names,
        pairs = pairs,
        data = selected_data
    )
}
