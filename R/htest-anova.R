#' ANOVA
#'
#' `ANOVA()` performs an analysis of variance for one-way, two-way,
#' or formula-based comparisons. If `ANOVA` is supplied within the lazy-loaded pipeline,
#' supply `ANOVA` as a function within i.e. `prepare_test(.test = ANOVA)` call.
#'
#' @param .model A model ID from `x_by()` or a formula.
#'   When supplied, the test executes immediately.
#' @param .data A data frame. Only used on the standalone path.
#' @param ... Additional arguments passed to the implementation.
#'
#' @return A `cld_exec` object (pipeline), or a `test_spec` object.
#'
#' @section Supported model IDs:
#' - `x_by()`: one-way or two-way ANOVA
#' - formula: standard `aov()` formula interface
#'
#' @examples
#' # pipeline — one-way
#' npk |>
#'     define_model(x_by(yield, block)) |>
#'     prepare_test(ANOVA) |>
#'     conclude()
#'
#' # pipeline — two-way
#' npk |>
#'     define_model(x_by(yield, c(block, N))) |>
#'     prepare_test(ANOVA) |>
#'     conclude()
#'
#' # formula interface
#' npk |>
#'     define_model(yield ~ block + N) |>
#'     prepare_test(ANOVA) |>
#'     conclude()
#'
#' # with hypothesis
#' npk |>
#'     define_model(x_by(yield, block)) |>
#'     prepare_test(ANOVA) |>
#'     state_null(
#'         MU(yield, block == "1") %=%
#'         MU(yield, block == "2") %=%
#'         MU(yield, block == "3")
#'     ) |>
#'     conclude()
#'
#' @export
ANOVA = HTEST_FN(
    cls = "anova",
    defs = list(
        anova_def_xby,
        anova_def_formula
    ),
    .name = "ANOVA"
)

#' Compute per-contrast statistics for a fitted aov object.
#'
#' Returns a tibble with one row per contrast — the same structure that
#' print() displays, and that tidy() will be able to forward directly.
#'
#' Each column of the contrast matrix lives in group-mean space (one weight
#' per level). We work entirely in the cell-means parameterisation:
#'
#'   μ̂       = group sample means  (k × 1)
#'   (X'X)⁻¹ = diag(1/nᵢ)         (k × k)
#'
#'   Lb     = c'μ̂ - k             (k = scalar shift from RHS)
#'   var_Lb = Σ cᵢ²/nᵢ * MSE
#'   SS     = Lb² / Σ(cᵢ²/nᵢ)
#'   F      = SS / MSE             (two-sided ==)
#'   t      = Lb / sqrt(var_Lb)    (one-sided >=, <=, >, <, !=)
#'
#' @param fit A fitted object from [stats::aov()].
#' @param contrasts A named list where the name is the grouping variable and
#'   the value is the contrast matrix, as produced by the `claim_translator`.
#'   The matrix may carry `"ops"` and `"scalars"` attributes.
#'
#' @return A tibble with columns: contrast, df, SS, MS, F, pval.
#'
#' @keywords internal
#' @noRd
compute_aov_contrasts = function(fit, contrasts) {
    grp_nm = names(contrasts)[[1]]
    mat = contrasts[[grp_nm]]

    mse = sum(fit$residuals^2) / fit$df.residual
    df_res = fit$df.residual

    grp_col = fit$model[[grp_nm]]
    lvl_ord = levels(grp_col)
    grp_ns = tabulate(grp_col)[seq_along(lvl_ord)]
    names(grp_ns) = lvl_ord

    resp = fit$model[[1]]
    grp_means = vapply(
        lvl_ord,
        function(lv) mean(resp[grp_col == lv]),
        numeric(1)
    )
    names(grp_means) = lvl_ord

    mat_aligned = mat[lvl_ord, , drop = FALSE]
    ops_vec = attr(mat, "ops") %||% rep("==", ncol(mat))
    scalars = attr(mat, "scalars") %||% rep(0, ncol(mat))

    contrast_rows = lapply(seq_len(ncol(mat)), function(j) {
        c_j = mat_aligned[, j]
        k_j = scalars[[j]]

        Lb = sum(c_j * grp_means) - k_j
        var_Lb = sum(c_j^2 / grp_ns) * mse
        ss = Lb^2 / sum(c_j^2 / grp_ns)
        op = ops_vec[[j]]

        if (op == "==") {
            f_stat = ss / mse
            pval = stats::pf(f_stat, 1, df_res, lower.tail = FALSE)
            list(df = 1L, SS = ss, MS = ss, F = f_stat, pval = pval)
        } else {
            t_stat = Lb / sqrt(var_Lb)
            pval = switch(
                op,
                ">=" = ,
                ">" = stats::pt(-t_stat, df = df_res),
                "<=" = ,
                "<" = stats::pt(t_stat, df = df_res),
                "!=" = 2 * stats::pt(-abs(t_stat), df = df_res)
            )
            list(df = 1L, SS = ss, MS = ss, F = t_stat^2, pval = pval)
        }
    })

    tibble::tibble(
        contrast = colnames(mat),
        df = vapply(contrast_rows, `[[`, integer(1), "df"),
        SS = round(vapply(contrast_rows, `[[`, numeric(1), "SS"), 4),
        MS = round(vapply(contrast_rows, `[[`, numeric(1), "MS"), 4),
        `F` = round(vapply(contrast_rows, `[[`, numeric(1), "F"), 4),
        pval = round(vapply(contrast_rows, `[[`, numeric(1), "pval"), 4)
    )
}
