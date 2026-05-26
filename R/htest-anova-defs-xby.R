anova_def_xby = test_define(
    model_type = x_by,
    # impl_class = "anova_xby",
    impl = agendas(
        base = baseline(
            fn = function(x, group_data, .contrasts = NULL) {
                grp_names = names(group_data)

                df = vctrs::new_data_frame(
                    c(list(x = x), as.list(group_data))
                )

                f = stats::as.formula(
                    paste("x ~", paste(grp_names, collapse = " + "))
                )

                # Stamp unnamed contrast columns with h01, h02, ...
                if (!is.null(.contrasts)) {
                    grp_nm = grp_names[[1]]
                    mat = .contrasts[[grp_nm]]
                    if (is.null(colnames(mat)) || any(!nzchar(colnames(mat)))) {
                        colnames(mat) = sprintf("h0%d", seq_len(ncol(mat)))
                        .contrasts[[grp_nm]] = mat
                    }
                }

                # Do NOT pass .contrasts to stats::aov — the claim-derived matrix
                # only contains rows for levels explicitly named in the hypothesis,
                # which would cause aov() to silently drop unmentioned levels from
                # the fit. Fit unconditionally on the full data; contrasts are
                # applied post-fit in compute_aov_contrasts() where all levels are
                # available from fit$model.
                fit = stats::aov(f, data = df)

                contrast_stats = if (!is.null(.contrasts)) {
                    compute_aov_contrasts(fit, .contrasts)
                } else {
                    NULL
                }

                list(
                    fit = fit,
                    group_names = grp_names,
                    contrasts = .contrasts,
                    contrast_stats = contrast_stats
                )
            },
            print = function(x, ...) {
                rlang::check_installed(
                    c("broom", "dplyr"),
                    reason = "to tidy ANOVA results"
                )

                dat = x@data
                fit = dat$fit
                contrasts = dat$contrasts
                grp_nm = dat$group_names[[1]]

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

                # ---- Omnibus table (Residuals merged as last row) ----
                new_tidy = broom::tidy(fit)
                tidy_terms = new_tidy[new_tidy$term != "Residuals", ]
                resid_row = new_tidy[new_tidy$term == "Residuals", ]

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

                # ---- Per-contrast table ----
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
        ),
        weighted = variant(
            # ---- Weighted equality test ----
            #
            # Implements the general linear hypothesis test for weighted
            # equality constraints on group means, following:
            #
            #   Rencher & Schaalje (2008), "Linear Models in Statistics",
            #   2nd ed., Chapter 8 — General Linear Hypothesis Lβ = c.
            #
            # Test statistic:
            #
            #   F = (Lβ̂ - c)' [L(X'X)⁻¹L']⁻¹ (Lβ̂ - c) / (q * MSE)
            #
            # Where:
            #   L   — q × p contrast matrix (one row per constraint)
            #   β̂   — OLS group mean estimates (cell means model)
            #   c   — hypothesized value vector (zeros for equality)
            #   q   — number of constraints (number of %=% pairs)
            #   MSE — mean squared error from the full model
            #   F   ~ F(q, N - p) under H₀
            #
            fn = function(x, group_data, .L = NULL, .c = NULL) {
                grp_name = names(group_data)[[1]]
                grp = as.character(group_data[[grp_name]])
                lvls = unique(grp)
                p = length(lvls)
                n = length(x)

                if (is.null(.L)) {
                    cli::cli_abort(c(
                        "No contrast matrix supplied.",
                        "i" = "Use {.fn state_null} with {.code %=%} and {.code via(\"weighted\")}."
                    ))
                }

                # ---- Step 1: Fit cell means model ----
                # β̂ is the vector of group sample means, one per level.
                grp_means = vapply(lvls, function(lv) mean(x[grp == lv]), numeric(1))
                names(grp_means) = lvls

                grp_ns = vapply(lvls, function(lv) sum(grp == lv), integer(1))
                names(grp_ns) = lvls

                # ---- Step 2: Compute MSE ----
                # MSE = within-group SS / (N - p)
                ss_resid = sum(vapply(lvls, function(lv) {
                    xi = x[grp == lv]
                    sum((xi - mean(xi))^2)
                }, numeric(1)))
                df_resid = n - p
                mse = ss_resid / df_resid

                # ---- Step 3: Build (X'X)⁻¹ ----
                # Under the cell means model, X'X is diagonal with group
                # sizes on the diagonal, so its inverse is diag(1/n_i).
                XtX_inv = diag(1 / grp_ns)

                # ---- Step 4: Align L columns to group level order ----
                # Column names of .L come from extract_param_name —
                # align to lvls so matrix multiplication is correct.
                L_cols = colnames(.L)
                missing_lvls = setdiff(L_cols, lvls)
                if (length(missing_lvls) > 0L) {
                    cli::cli_abort(c(
                        "Contrast matrix references groups not found in data.",
                        "i" = "Missing: {.val {missing_lvls}}.",
                        "i" = "Available: {.val {lvls}}."
                    ))
                }

                # Expand L to full group set, filling missing groups with 0
                L_full = matrix(0, nrow = nrow(.L), ncol = p)
                colnames(L_full) = lvls
                rownames(L_full) = rownames(.L)
                L_full[, L_cols] = .L[, L_cols, drop = FALSE]

                c_vec = if (is.null(.c)) rep(0, nrow(L_full)) else .c
                q = nrow(L_full)

                # ---- Step 5: Compute F statistic ----
                # Following Rencher & Schaalje (2008), eq. 8.14:
                # F = (Lβ̂ - c)' [L(X'X)⁻¹L']⁻¹ (Lβ̂ - c) / (q * MSE)
                Lb = L_full %*% grp_means
                diff_vec = Lb - c_vec
                middle = L_full %*% XtX_inv %*% t(L_full)
                f_stat = as.numeric(
                    t(diff_vec) %*% solve(middle) %*% diff_vec / (q * mse)
                )

                p.value = stats::pf(f_stat, df1 = q, df2 = df_resid, lower.tail = FALSE)

                list(
                    group = grp_name,
                    f_stat = f_stat,
                    df1 = q,
                    df2 = df_resid,
                    p.value = p.value,
                    L = L_full,
                    c_vec = c_vec,
                    grp_means = grp_means,
                    mse = mse
                )
            },
            print = function(x, ...) {
                dat = x@data

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

                stat_out = tibble::tibble(
                    group = dat$group,
                    `F` = round(dat$f_stat, 4),
                    df1 = dat$df1,
                    df2 = dat$df2,
                    pval = round(dat$p.value, 4)
                )

                cli::cat_line(cli::rule(left = "Weighted Equality Test", line = "-"), "\n")
                tabstats::table_default(
                    stat_out,
                    style_columns = tabstats::td_style(pval = pval_styler)
                )
                cat("\n\n")

                invisible(x)
            }
        )
    ),
    # ---- Modify Modelled Hypothesis ----
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

                if (all_peq) {
                    has_weighted = any(vapply(claims_list, function(cl) {
                        any(vapply(cl@lhs, inherits, logical(1), "arith_node"))
                    }, logical(1)))
                    if (has_weighted) {
                        cli::cli_abort(c(
                            "Weighted terms found in {.code %=%} hypothesis.",
                            "i" = "Use {.code via(\"weighted\")} for weighted equality hypotheses."
                        ))
                    }
                    return(NULL)
                }

                grp_name = names(processed$group_data)[[1]]
                lvls = unique(as.character(processed$group_data[[grp_name]]))

                resolved = lapply(claims_list, claim_contrast_coefs)

                mat = matrix(0, nrow = length(lvls), ncol = length(resolved))
                rownames(mat) = lvls
                colnames(mat) = names(claims_list)
                scalars = numeric(length(resolved))

                for (j in seq_along(resolved)) {
                    coef = resolved[[j]]$coefs
                    matched = intersect(names(coef), lvls)
                    if (length(matched) == 0L) {
                        cli::cli_abort(c(
                            "Contrast references groups not found in data.",
                            "i" = "Available levels: {.val {lvls}}."
                        ))
                    }
                    mat[matched, j] = coef[matched]
                    scalars[[j]] = resolved[[j]]$scalar
                }

                ops_vec = vapply(claims_list, function(cl) cl@op, character(1))
                attr(mat, "ops") = ops_vec
                attr(mat, "scalars") = scalars
                rlang::set_names(list(mat), grp_name)
            }
        ),
        weighted = map_claim(
            .L = function(claim, processed) {
                nodes = claim@lhs

                operand_terms = lapply(nodes, function(node) {
                    terms = collect_terms(node, sign = 1L)
                    param_terms = Filter(function(t) t$kind == "param", terms)

                    if (length(param_terms) != 1L) {
                        cli::cli_abort(
                            "Each operand in {.code %=%} must reference exactly one parameter."
                        )
                    }

                    list(
                        nm = extract_param_name(param_terms[[1]]$node),
                        coef = param_terms[[1]]$coef
                    )
                })

                all_nms = vapply(operand_terms, `[[`, character(1), "nm")
                unique_nms = unique(all_nms)
                k = length(operand_terms)

                # k-1 rows: one constraint per adjacent pair
                L = matrix(0, nrow = k - 1L, ncol = length(unique_nms))
                colnames(L) = unique_nms

                for (i in seq_len(k - 1L)) {
                    lhs_term = operand_terms[[i]]
                    rhs_term = operand_terms[[i + 1L]]
                    L[i, lhs_term$nm] = lhs_term$coef
                    L[i, rhs_term$nm] = -rhs_term$coef
                }

                L
            },
            .c = function(claim, processed) {
                nodes = claim@lhs
                rep(0, length(nodes) - 1L)
            }
        )
    )
)
