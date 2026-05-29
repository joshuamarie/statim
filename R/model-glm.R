#' Generalized linear model
#'
#' A modified GLM for `{statim}` pipeline passed through [stats::glm()].
#'
#' @param .model A model ID from [define_model()], or `NULL` to return a
#'   `model_spec` for use in [prepare_model()].
#' @param .data A data frame. Used when `.model` is supplied directly.
#' @param family A family object, e.g. [stats::binomial()], [stats::poisson()].
#'   Defaults to [stats::gaussian()].
#' @param ... Additional arguments passed to [stats::glm()].
#'
#' @return A `cld_exec` object in a `glm_object`, or a `model_spec`
#'   when `.model = NULL`.
#'
#' @examples
#' # logistic regression
#' mtcars |>
#'     define_model(am ~ wt + hp) |>
#'     prepare_model(GLM) |>
#'     update(family = binomial()) |>
#'     conclude()
#'
#' # model comparison via anova()
#' mod1 = mtcars |> define_model(am ~ 1) |> prepare_model(GLM) |> update(family = binomial()) |> conclude()
#' mod2 = mtcars |> define_model(am ~ wt) |> prepare_model(GLM) |> update(family = binomial()) |> conclude()
#' mod3 = mtcars |> define_model(am ~ wt + hp) |> prepare_model(GLM) |> update(family = binomial()) |> conclude()
#'
#' anova(mod1, mod2, mod3)
#'
#' @export
GLM = MODEL_FN(
    cls = "glm",
    defs = list(glm_def_formula),
    .name = "Generalized Linear Model"
)

#' Structured result container for GLM fits
#'
#' @slot family A string naming the GLM family, e.g. `"binomial"`.
#'   Populated automatically from the fitted object.
#'
#' @seealso [anova_able], [GLM]
#'
#' @export
glm_object = S7::new_class(
    "glm_object",
    parent = anova_able,
    properties = list(
        coefficients = S7::class_data.frame,
        fit_summary = S7::new_property(class = S7::class_data.frame, default = NULL)
    )
)

S7::method(print, glm_object) = function(x, ...) {
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

#' Extract slots from a fitted glm into a glm_object
#'
#' @param fit A fitted `glm` object.
#' @return A `glm_object`.
#'
#' @keywords internal
#' @noRd
glm_to_glm_object = function(fit) {
    if (!inherits(fit, "glm")) {
        cli::cli_abort(c(
            "{.fn glm_to_glm_object} requires a fitted {.cls glm} object.",
            "i" = "Got {.cls {class(fit)[[1]]}}."
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

    fam = fit$family$family
    phi = if (fam %in% c("binomial", "poisson")) {
        1
    } else {
        summary(fit)$dispersion
    }

    fit_tbl = tibble::tibble(
        family = fam,
        link = fit$family$link,
        null_deviance = fit$null.deviance,
        deviance = fit$deviance,
        df_residual = as.integer(fit$df.residual),
        aic = fit$aic,
        n_obs = as.integer(length(fit$residuals))
    )

    glm_object(
        terms = fit$terms,
        df_residual = fit$df.residual,
        deviance = fit$deviance,
        dispersion = phi,
        family = fam,
        coefficients = coef_tbl,
        fit_summary = fit_tbl
    )
}
