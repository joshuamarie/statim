#' Generalized linear model
#'
#' A modified GLM for `{statim}` pipeline passed through [stats::glm()].
#'
#' Additional arguments are passed to [stats::glm()]. The most important
#' is `family`, which controls the error distribution and link function
#' (e.g. [stats::binomial()], [stats::poisson()]). Defaults to
#' [stats::gaussian()] when omitted.
#'
#' @param .model A model ID from [define_model()], or `NULL` to return a
#'   `model_spec` for use in [prepare_model()].
#' @param .data A data frame. Used when `.model` is supplied directly.
#' @param ... Additional arguments passed to [stats::glm()].
#'
#' @return A `cld_exec` object in a `class_glm_object`, or a `model_spec`
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
#' \dontrun{
#' # model comparison via anova()
#' mod1 = mtcars |>
#'     define_model(am ~ 1) |>
#'     prepare_model(GLM) |>
#'     update(family = binomial()) |>
#'     conclude()
#' mod2 = mtcars |>
#'     define_model(am ~ wt) |>
#'     prepare_model(GLM) |>
#'     update(family = binomial()) |>
#'     conclude()
#' mod3 = mtcars |>
#'     define_model(am ~ wt + hp) |>
#'     prepare_model(GLM) |>
#'     update(family = binomial()) |>
#'     conclude()
#'
#' anova(mod1, mod2, mod3)
#' }
#'
#' @export
GLM = MODEL_FN(
    cls = "glm",
    defs = list(glm_def_formula),
    .name = "Generalized Linear Model"
)

#' Structured result container for GLM fits
#'
#' @description
#' An S7 class produced by [GLM] pipelines. Not constructed manually —
#' use `define_model() |> prepare_model(GLM) |> conclude()` instead.
#'
#' Inherits from [anova_able], so it participates in [anova()] directly.
#' Downstream packages can use it as a `parent` in `S7::new_class()`.
#'
#' @usage NULL
#'
#' @details
#' Constructor arguments (populated automatically by [GLM]):
#'
#' - `terms`: model terms object.
#' - `df_residual`: residual degrees of freedom.
#' - `deviance`: scalar deviance.
#' - `dispersion`: scalar dispersion parameter.
#' - `family`: string naming the error family, e.g. `"binomial"`.
#' - `coefficients`: data frame with columns `term`, `estimate`,
#'   `std_error`, `statistic`, `p_value`.
#' - `fit_summary`: data frame with columns `family`, `link`,
#'   `null_deviance`, `deviance`, `df_residual`, `aic`, `n_obs`.
#'
#' @seealso [anova_able], [GLM]
#'
#' @examples
#' # Inheriting from class_glm_object in a downstream package:
#' my_glm = S7::new_class(
#'     "my_glm",
#'     parent = class_glm_object
#' )
#'
#' # Populating class_glm_object from a fitted glm (as done internally):
#' fit = glm(am ~ wt + hp, data = mtcars, family = binomial())
#' s = summary(fit)
#' fam = fit$family$family
#'
#' obj = class_glm_object(
#'     terms = fit$terms,
#'     df_residual = fit$df.residual,
#'     deviance = fit$deviance,
#'     dispersion = if (fam %in% c("binomial", "poisson")) 1 else s$dispersion,
#'     family = fam,
#'     coefficients = tibble::tibble(
#'         term = rownames(coef(s)),
#'         estimate = coef(s)[, 1],
#'         std_error = coef(s)[, 2],
#'         statistic = coef(s)[, 3],
#'         p_value = coef(s)[, 4]
#'     ),
#'     fit_summary = tibble::tibble(
#'         family = fam,
#'         link = fit$family$link,
#'         null_deviance = fit$null.deviance,
#'         deviance = fit$deviance,
#'         df_residual = as.integer(fit$df.residual),
#'         aic = fit$aic,
#'         n_obs = as.integer(length(fit$residuals))
#'     )
#' )
#'
#' @export
class_glm_object = S7::new_class(
    "glm_object",
    parent = anova_able,
    properties = list(
        coefficients = S7::new_property(class = S7::class_data.frame, default = data.frame()),
        fit_summary = S7::new_property(class = S7::class_data.frame, default = data.frame())
    )
)

S7::method(print, class_glm_object) = function(x, ...) {
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

#' Extract slots from a fitted glm into a class_glm_object
#'
#' @param fit A fitted `glm` object.
#' @return A `class_glm_object`.
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

    class_glm_object(
        terms = fit$terms,
        df_residual = fit$df.residual,
        deviance = fit$deviance,
        dispersion = phi,
        family = fam,
        coefficients = coef_tbl,
        fit_summary = fit_tbl
    )
}
