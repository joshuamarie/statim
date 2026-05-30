anova_def_formula = test_define(
    model_type = S7::class_formula,
    # impl_class = "anova_formula",
    impl = agendas(
        base = baseline(
            fn = function(.proc, .contrasts = NULL) {
                formula = .proc$formula
                data = .proc$data

                # Stamp unnamed contrast columns with h01, h02, ...
                if (!is.null(.contrasts)) {
                    grp_nm = names(.contrasts)[[1]]
                    mat = .contrasts[[grp_nm]]
                    if (is.null(colnames(mat)) || any(!nzchar(colnames(mat)))) {
                        colnames(mat) = sprintf("h0%d", seq_len(ncol(mat)))
                        .contrasts[[grp_nm]] = mat
                    }
                }

                fit = stats::aov(formula, data = data)

                contrast_stats = if (!is.null(.contrasts)) {
                    compute_aov_contrasts(fit, .contrasts)
                } else {
                    NULL
                }

                list(fit = fit, contrasts = .contrasts, contrast_stats = contrast_stats)
            },
            print = function(x, ...) {
                rlang::check_installed(
                    "broom",
                    reason = "to tidy ANOVA results"
                )

                dat = x@data
                fit = dat$fit

                pval_styler = function(x) {
                    x_num = suppressWarnings(as.numeric(x$value))
                    if (is.na(x_num) || x_num > 0.05) {
                        cli::style_italic(x$value)
                    } else if (x_num > 0.01) {
                        cli::col_red(x$value)
                    } else {
                        cli::style_bold("<0.001")
                    }
                }

                tidy = broom::tidy(fit)
                tidy_terms = tidy[tidy$term != "Residuals", ]
                resid_row = tidy[tidy$term == "Residuals", ]

                stat_out = tibble::tibble(
                    term = c(tidy_terms$term, "Residuals"),
                    df = as.integer(c(tidy_terms$df, resid_row$df)),
                    SS = round(c(tidy_terms$sumsq, resid_row$sumsq), 4),
                    MS = round(c(tidy_terms$meansq, resid_row$meansq), 4),
                    `F` = c(round(tidy_terms$statistic, 4), NA_real_),
                    pval = c(round(tidy_terms$p.value, 4), NA_real_)
                )

                cli::cat_line(cli::rule(left = "ANOVA Table", line = "-"), "\n")
                tabstats::table_default(
                    stat_out,
                    style_columns = tabstats::td_style(pval = pval_styler)
                )
                cat("\n")

                if (!is.null(dat$contrast_stats)) {
                    cli::cat_line(cli::rule(left = "Contrasts", line = "-"), "\n")
                    tabstats::table_default(
                        dat$contrast_stats,
                        style_columns = tabstats::td_style(pval = pval_styler)
                    )
                    cat("\n\n")
                }

                invisible(x)
            }
        )
    ),
    compatible_params = list(MU),
    claim_translator = claim_translate(
        default = map_claim(
            .contrasts = function(claim, processed) {
                claims_list = if (S7::S7_inherits(claim, null_claims)) {
                    claim@claims
                } else {
                    list(claim)
                }

                ops = vapply(claims_list, function(cl) cl@op, character(1))
                all_peq = all(ops == "%=%")
                any_peq = any(ops == "%=%")

                if (any_peq && !all_peq) {
                    cli::cli_abort(c(
                        "Cannot mix {.code %=%} and {.code ==} in the same {.fn more_h0} block.",
                        "i" = "Use {.code %=%} alone for omnibus equality, or {.code ==} alone for contrasts."
                    ))
                }

                # Omnibus %=% — no custom contrast matrix needed
                if (all_peq) {
                    return(NULL)
                }

                # Infer the grouping variable from the first claim's param nodes.
                # For the formula variant we don't have processed$group_data, so
                # we extract the group name from the `given` predicate of each
                # MU() call (e.g. MU(y, group == "A") → "group").
                grp_name = infer_group_name_from_claims(claims_list)

                resolved = lapply(claims_list, claim_contrast_coefs)

                # Collect all level names mentioned across all contrasts so the
                # matrix row set is complete. compute_aov_contrasts aligns rows
                # to fit$model levels at compute time, so partial coverage here
                # is fine — missing levels stay at zero.
                all_lvls = unique(unlist(lapply(resolved, function(r) names(r$coefs))))

                mat = matrix(0, nrow = length(all_lvls), ncol = length(resolved))
                rownames(mat) = all_lvls
                colnames(mat) = names(claims_list)
                scalars = numeric(length(resolved))

                for (j in seq_along(resolved)) {
                    coef = resolved[[j]]$coefs
                    matched = intersect(names(coef), all_lvls)
                    mat[matched, j] = coef[matched]
                    scalars[[j]] = resolved[[j]]$scalar
                }

                ops_vec = vapply(claims_list, function(cl) cl@op, character(1))
                attr(mat, "ops") = ops_vec
                attr(mat, "scalars") = scalars
                rlang::set_names(list(mat), grp_name)
            }
        )
    )
)

# Infer the grouping variable name from a list of null_claim objects.
#
# Walks the param nodes of each claim and extracts the LHS variable name
# from any `given` predicate of the form `var == "level"`. Errors if
# claims reference more than one grouping variable, since a single contrast
# matrix can only index one factor.
#
#' @keywords internal
#' @noRd
infer_group_name_from_claims = function(claims_list) {
    grp_names = character(0)

    for (cl in claims_list) {
        nodes = if (cl@op == "%=%") cl@lhs else list(cl@lhs, cl@rhs)
        nodes = Filter(Negate(is.numeric), nodes)

        for (node in nodes) {
            nms = extract_group_names_from_node(node)
            grp_names = c(grp_names, nms)
        }
    }

    grp_names = unique(grp_names)

    if (length(grp_names) == 0L) {
        cli::cli_abort(c(
            "Cannot infer grouping variable from hypothesis.",
            "i" = "Use {.code MU(y, group == \"level\")} syntax so the group name is recoverable."
        ))
    }

    if (length(grp_names) > 1L) {
        cli::cli_abort(c(
            "Contrasts reference more than one grouping variable: {.val {grp_names}}.",
            "i" = "All contrasts in a single {.fn more_h0} block must share one grouping variable."
        ))
    }

    grp_names[[1]]
}

# Recursively extract grouping variable names from a param node or arith_node.
#
#' @keywords internal
#' @noRd
extract_group_names_from_node = function(node) {
    if (S7::S7_inherits(node, param_obj)) {
        given = node@given
        if (is.null(given)) return(character(0))
        given_expr = rlang::quo_get_expr(given)
        if (rlang::is_call(given_expr, "==") && length(given_expr) == 3L) {
            return(as.character(given_expr[[2]]))
        }
        return(character(0))
    }

    if (inherits(node, "arith_node")) {
        return(unlist(lapply(node$operands, extract_group_names_from_node)))
    }

    character(0)
}
