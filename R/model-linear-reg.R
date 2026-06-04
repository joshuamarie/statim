#' Linear regression
#'
#' Fits an ordinary least squares linear regression model.
#' Accepts [rel()] or a formula as the model ID.
#'
#' The result is an [class_lm_object], which satisfies the [anova()]
#' protocol and prints coefficients and model fit universally across
#' all engines and variants.
#'
#' @param .model A model ID from [define_model()], or `NULL` to return a
#'   `model_spec` for use in [prepare_model()].
#' @param .data A data frame. Used when `.model` is supplied directly.
#' @param ... Currently unused.
#'
#' @return A `cld_exec` object containing a [class_lm_object], or a `model_spec`
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
#' @description
#' An S7 class produced by [LINEAR_REG] pipelines. Not constructed manually —
#' use `define_model() |> prepare_model(LINEAR_REG) |> conclude()` instead.
#'
#' Inherits from [anova_able], so it participates in [anova()] directly.
#' Downstream packages can use it as a `parent` in `S7::new_class()`.
#'
#' @usage NULL
#'
#' @details
#' Constructor arguments (populated automatically by [LINEAR_REG]):
#'
#' - `terms`: model terms object.
#' - `df_residual`: residual degrees of freedom.
#' - `deviance`: residual sum of squares (RSS).
#' - `dispersion`: mean squared error (MSE), i.e. `rss / df_residual`.
#' - `family`: always `"gaussian"` for OLS.
#' - `fitted`: numeric vector of fitted values.
#' - `residuals`: numeric vector of model residuals.
#' - `beta`: named numeric vector of coefficient estimates.
#' - `std_beta`: named numeric vector of coefficient standard errors.
#' - `x_mat`: model matrix stored as a flat numeric vector via
#'   `as.numeric(stats::model.matrix(fit))`. Required for single-model
#'   [anova()]; omit only if Type I ANOVA is not needed.
#'
#' The following are computed automatically and do not need to be supplied:
#'
#' - `statistic`: per-coefficient t-statistics (`beta / std_beta`).
#' - `p_value`: per-coefficient two-sided p-values.
#' - `coefficients`: tibble with columns `term`, `estimate`, `std_error`,
#'   `statistic`, `p_value`.
#' - `fit_summary`: two-column tibble (`statistic`, `value`) with R-squared,
#'   adjusted R-squared, sigma, n, residual df, F-statistic, df1, df2, and
#'   F p-value.
#'
#' @section anova() protocol:
#' `class_lm_object` supports two [anova()] modes:
#'
#' - **Single model** — Type I (sequential) ANOVA. Each term is tested
#'   against the model containing all preceding terms. Requires `x_mat`
#'   to be populated.
#' - **Multiple models** — incremental F-test or LRT across nested models.
#'   Uses `@deviance` and `@df_residual` only; `x_mat` is not needed.
#'
#' @seealso [anova()], [LINEAR_REG]
#'
#' @examples
#' # Inheriting from class_lm_object in a downstream package:
#' my_lm = S7::new_class(
#'     "my_lm",
#'     parent = statim::class_lm_object
#' )
#'
#' # Populating class_lm_object from a fitted lm (as done internally):
#' fit = lm(dist ~ speed, data = cars)
#' coef_tbl = summary(fit)$coefficients
#' rss = sum(fit$residuals^2)
#' df_res = fit$df.residual
#'
#' obj = class_lm_object(
#'     terms = fit$terms,
#'     fitted = unname(fit$fitted.values),
#'     residuals = unname(fit$residuals),
#'     beta = coef_tbl[, 1],
#'     std_beta = coef_tbl[, 2],
#'     df_residual = df_res,
#'     deviance = rss,
#'     dispersion = rss / df_res,
#'     family = "gaussian",
#'     x_mat = as.numeric(stats::model.matrix(fit))
#' )
#'
#' obj@coefficients
#' obj@fit_summary
#'
#' @export
class_lm_object = S7::new_class(
    "class_lm_object",
    parent = anova_able,
    properties = list(

        # ---- Required inputs ----
        fitted = S7::class_numeric,
        residuals = S7::class_numeric,
        beta = S7::class_numeric,
        std_beta = S7::class_numeric,

        # ---- Computed: per-coefficient stats ----
        statistic = S7::new_property(
            getter = function(self) self@beta / self@std_beta
        ),
        p_value = S7::new_property(
            getter = function(self) {
                2 * pt(abs(self@statistic), df = self@df_residual, lower.tail = FALSE)
            }
        ),

        # ---- Computed: coefficients table ----
        coefficients = S7::new_property(
            getter = function(self) {
                nms = if (!is.null(names(self@beta))) {
                    names(self@beta)
                } else {
                    trms = attr(self@terms, "term.labels")
                    if (attr(self@terms, "intercept") == 1L) c("(Intercept)", trms) else trms
                }
                tibble::tibble(
                    term = nms,
                    estimate = unname(self@beta),
                    std_error = unname(self@std_beta),
                    statistic = unname(self@statistic),
                    p_value = unname(self@p_value)
                )
            }
        ),

        # ---- Computed: model fit summary ----
        fit_summary = S7::new_property(
            getter = function(self) {
                y = self@fitted + self@residuals
                n = length(y)
                df_res = self@df_residual
                rss = self@deviance
                tss = sum((y - mean(y))^2)
                p = n - df_res - 1L

                r2 = 1 - rss / tss
                adj_r2 = 1 - (1 - r2) * (n - 1L) / df_res
                sigma = sqrt(rss / df_res)
                f_stat = (r2 / p) / ((1 - r2) / df_res)
                f_p_value = pf(f_stat, p, df_res, lower.tail = FALSE)

                tibble::tibble(
                    statistic = c(
                        "R Squared",
                        "Adj. R Squared",
                        "Sigma",
                        "n",
                        "df (residual)",
                        "F-statistic",
                        "df1",
                        "df2",
                        "p-value"
                    ),
                    value = c(
                        r2,       # round(r2, 4),
                        adj_r2,   # round(adj_r2, 4),
                        sigma,    # round(sigma, 4),
                        n,        # n,
                        df_res,   # df_res,
                        f_stat,   # round(f_stat, 4),
                        p,        # p,
                        df_res,   # df_res,
                        f_p_value # round(f_p_value, 6)
                    )
                )
            }
        ),

        # ---- Must be supplied to obtain type I ANOVA ----
        x_mat = S7::new_property(
            class = S7::new_union(S7::class_numeric, S7::class_missing),
            default = NULL
        )
    )
)

S7::method(print, class_lm_object) = function(x, ...) {
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

    integer_stats = c("n", "df (residual)", "df1", "df2")
    p_stats = "p-value"

    fs = x@fit_summary
    fs$value = vapply(seq_len(nrow(fs)), function(i) {
        nm = fs$statistic[[i]]
        v = fs$value[[i]]
        if (nm %in% integer_stats) {
            formatC(as.integer(v), format = "d")
        } else if (nm %in% p_stats) {
            x_num = suppressWarnings(as.numeric(v))
            if (is.na(x_num) || x_num > 0.001) {
                formatC(x_num, digits = 2, format = "f")
            } else {
                "<0.001"
            }
        } else {
            formatC(v, digits = 2, format = "f")
        }
    }, character(1))

    cli::cat_line(cli::rule(left = "Model Fit", line = "-"), "\n")
    tabstats::table_summary(
        fs,
        center_table = TRUE,
        l = 5L,
        style = tabstats::sm_style(sep = ":  ")
    )
    cat("\n\n")

    invisible(x)
}

#' Extract slots from a fitted lm into an lm_object
#'
#' Internal helper used by `linear_reg_def_*` implementations.
#'
#' @param fit A fitted `lm` object.
#' @return A `class_lm_object`.
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

    coef_tbl = summary(fit)$coefficients
    rss = sum(fit$residuals^2)
    df_res = fit$df.residual

    class_lm_object(
        terms = fit$terms,
        fitted = unname(fit$fitted.values),
        residuals = unname(fit$residuals),
        beta = coef_tbl[, 1],
        std_beta = coef_tbl[, 2],
        df_residual = df_res,
        deviance = rss,
        dispersion = rss / df_res,
        family = "gaussian",
        x_mat = as.numeric(stats::model.matrix(fit))
    )
}
