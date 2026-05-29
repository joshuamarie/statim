#' Linear regression
#'
#' Fits an ordinary least squares linear regression model.
#' Accepts [rel()] or a formula as the model ID.
#'
#' The result is an [lm_object], which satisfies the [anova()]
#' protocol and prints coefficients and model fit universally across
#' all engines and variants.
#'
#' @param .model A model ID from [define_model()], or `NULL` to return a
#'   `model_spec` for use in [prepare_model()].
#' @param .data A data frame. Used when `.model` is supplied directly.
#' @param ... Currently unused.
#'
#' @return A `cld_exec` object containing an [lm_object], or a `model_spec`
#'   when `.model = NULL`.
#'
#' @examples
#' # via rel()
#' cars |>
#'     define_model(rel(speed, dist)) |>
#'     prepare_model(LINEAR_REG) |>
#'     conclude()
#'
#' # via formula
#' cars |>
#'     define_model(dist ~ speed) |>
#'     prepare_model(LINEAR_REG) |>
#'     conclude()
#'
#' # write_models() pipeline
#' LifeCycleSavings |>
#'     write_models(
#'         f1 = sr ~ 1,
#'         f2 = sr ~ pop15,
#'         f3 = sr ~ pop15 + pop75,
#'         f4 = sr ~ pop15 + pop75 + dpi,
#'         f5 = sr ~ pop15 + pop75 + dpi + ddpi
#'     ) |>
#'     prepare_model(LINEAR_REG) |>
#'     anova()
#'
#' # individual conclude(), compare after
#' mod1 = LifeCycleSavings |> define_model(sr ~ 1) |> prepare_model(LINEAR_REG) |> conclude()
#' mod2 = LifeCycleSavings |> define_model(sr ~ pop15) |> prepare_model(LINEAR_REG) |> conclude()
#'
#' anova(mod1, mod2)
#'
#' @export
LINEAR_REG = MODEL_FN(
    cls = "linear_reg",
    defs = list(linear_reg_def_rel, linear_reg_def_formula),
    .name = "Linear Regression"
)

#' Structured result container for linear model fits
#'
#' `lm_object` is the standard return type for any `fn` inside a
#' `model_infer_define` that fits a linear model. It holds only the
#' information that is universally meaningful across all engines and
#' variants, and satisfies the protocol required by [anova()].
#'
#' Implementors must populate all slots from their engine's output.
#' The method label is supplied automatically from [agendas()] via
#' the enclosing `cld_exec` objects, no manual labelling is needed.
#'
#' @section anova() protocol:
#' `lm_object` participates in [anova()] directly. The comparison
#' is computed from `@residuals`, `@df_residual`, and `@terms`.
#'
#' @slot terms The model terms object. Used by [anova()] to
#'   verify response consistency across models.
#' @slot residuals A numeric vector of model residuals. Used by
#'   [anova()] for nobs checking and deviance computation.
#' @slot df_residual Residual degrees of freedom.
#' @slot coefficients A data frame with columns `term`, `estimate`,
#'   `std_error`, `statistic`, `p_value`.
#' @slot fit_summary A data frame with columns `r_squared`,
#'   `adj_r_squared`, `sigma`, `df_residual`, `nobs`.
#'
#' @seealso [anova()], [LINEAR_REG]
#'
#' @export
lm_object = S7::new_class(
    "lm_object",
    parent = anova_able,
    properties = list(
        residuals = S7::class_numeric,
        coefficients = S7::class_data.frame,
        fit_summary = S7::new_property(class = S7::class_data.frame, default = NULL)
    )
)

S7::method(print, lm_object) = function(x, ...) {
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

    cli::cat_line(cli::rule(left = "Coefficients", line = "-"), "\n")
    tabstats::table_default(
        x@coefficients,
        style_columns = tabstats::td_style(p_value = pval_styler),
        nrows = nrow(x@coefficients),
        justify_cols = list(term = "left"),
        vb = list(char = "\u2502", after = "term")
    )
    cat("\n\n")

    cli::cat_line(cli::rule(left = "Model Fit", line = "-"), "\n")
    tabstats::table_default(x@fit_summary)
    cat("\n\n")

    invisible(x)
}

#' Extract slots from a fitted lm into an lm_object
#'
#' Internal helper used by `linear_reg_def_*` implementations.
#'
#' @param fit A fitted `lm` object.
#' @return An `lm_object`.
#'
#' @keywords internal
#' @noRd
lm_to_lm_object = function(fit) {
    if (!inherits(fit, "lm")) {
        cli::cli_abort(c(
            "{.fn lm_to_lm_object} requires a fitted {.cls lm} object.",
            "i" = "Got {.cls {class(fit)[[1]]}}.",
            "i" = "Did you pass {.code method = \"model.frame\"} or similar?"
        ))
    }

    coef_tbl = as.data.frame(summary(fit)$coefficients)
    coef_tbl = tibble::tibble(
        term = rownames(coef_tbl),
        estimate = coef_tbl[[1]],
        std_error = coef_tbl[[2]],
        statistic = coef_tbl[[3]],
        p_value = coef_tbl[[4]]
    )

    s = summary(fit)
    rss = sum(fit$residuals^2)
    df_res = fit$df.residual

    fit_tbl = tibble::tibble(
        r_squared = s$r.squared,
        adj_r_squared = s$adj.r.squared,
        sigma = s$sigma,
        df_residual = as.integer(df_res),
        n_obs = as.integer(length(fit$residuals))
    )

    lm_object(
        terms = fit$terms,
        residuals = fit$residuals,
        df_residual = df_res,
        deviance = rss,
        dispersion = rss / df_res,
        family = "gaussian",
        coefficients = coef_tbl,
        fit_summary = fit_tbl
    )
}
# lm_to_lm_object = function(fit) {
#     if (!inherits(fit, "lm")) {
#         cli::cli_abort(c(
#             "{.fn lm_to_lm_object} requires a fitted {.cls lm} object.",
#             "i" = "Got {.cls {class(fit)[[1]]}}.",
#             "i" = "Did you pass {.code method = \"model.frame\"} or similar?"
#         ))
#     }
#
#     coef_tbl = as.data.frame(summary(fit)$coefficients)
#     coef_tbl = tibble::tibble(
#         term = rownames(coef_tbl),
#         estimate = coef_tbl[[1]],
#         std_error = coef_tbl[[2]],
#         statistic = coef_tbl[[3]],
#         p_value = coef_tbl[[4]]
#     )
#
#     s = summary(fit)
#     fit_tbl = tibble::tibble(
#         r_squared = s$r.squared,
#         adj_r_squared = s$adj.r.squared,
#         sigma = s$sigma,
#         df_residual = as.integer(fit$df.residual),
#         n_obs = as.integer(length(fit$residuals))
#     )
#
#     lm_object(
#         terms = fit$terms,
#         residuals = fit$residuals,
#         df_residual = fit$df.residual,
#         coefficients = coef_tbl,
#         fit_summary = fit_tbl
#     )
# }

