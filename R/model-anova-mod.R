anova_lazy = S7::new_class(
    "anova_lazy",
    properties = list(
        models = S7::class_list,
        labels = S7::new_property(class = S7::class_character, default = character(0)),
        args = S7::new_property(class = S7::class_list, default = list())
    )
)

cld_anova = S7::new_class(
    "cld_anova",
    parent = cld_exec,
    properties = list(
        labels = S7::new_property(class = S7::class_character, default = character(0))
    )
)

S7::method(print, cld_anova) = function(x, ...) {
    stat_label = if (identical(x@cld_meta$method, "default")) {
        x@cld_meta$stat_name
    } else {
        paste0(x@cld_meta$stat_name, " \u00b7 ", x@cld_meta$method)
    }

    cat("\n")
    cat(cli::rule(left = stat_label, line = "="), "\n\n")

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

    cli::cat_line(cli::rule(left = "ANOVA Table", line = "-"), "\n")
    tabstats::table_default(
        x@data,
        style_columns = tabstats::td_style(p_value = pval_styler)
    )
    cat("\n\n")

    invisible(x)
}

#' ANOVA table for linear model comparisons
#'
#' `anova()` computes an incremental F-test across two or more fitted linear
#' models. It dispatches on three input types:
#'
#' - An `anova_lazy` from `write_models() |> prepare_model()`.
#' - One or more `model_lazy` objects from `prepare_model()`.
#' - One or more `cld_exec` objects from `conclude()`.
#'
#' @param object An `anova_lazy`, `model_lazy`, or `cld_exec` object.
#' @param ... Additional `model_lazy` or `cld_exec` objects.
#' @param test A string. One of `"F"` (default), `"LRT"`, or `"Chisq"`.
#'
#' @return A `cld_anova` object, invisibly.
#'
#' @seealso [write_models()], [prepare_model()], [conclude()]
#'
#' @examples
#' # via write_models()
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
#' # via model_lazy
#' mod1 = LifeCycleSavings |> define_model(sr ~ 1) |> prepare_model(LINEAR_REG)
#' mod2 = LifeCycleSavings |> define_model(sr ~ pop15) |> prepare_model(LINEAR_REG)
#'
#' anova(mod1, mod2)
#'
#' # via conclude()
#' mod1 = LifeCycleSavings |> define_model(sr ~ 1) |> prepare_model(LINEAR_REG) |> conclude()
#' mod2 = LifeCycleSavings |> define_model(sr ~ pop15) |> prepare_model(LINEAR_REG) |> conclude()
#'
#' anova(mod1, mod2)
#' anova(mod1, mod2, test = "LRT")
#'
#' @name anova-mod
#' @export
anova = S7::new_external_generic("stats", "anova", "object")

S7::method(anova, anova_lazy) = function(object, ..., test = "F") {
    valid_tests(test)
    fitted = lapply(object@models, run_model_lazy_raw)
    build_anova(fitted, labels = object@labels, test = test)
}

S7::method(anova, model_lazy) = function(object, ..., test = "F") {
    rest = list(...)

    not_lazy = !vapply(rest, function(m) S7::S7_inherits(m, model_lazy), logical(1))
    if (any(not_lazy)) {
        cli::cli_abort(c(
            "All arguments to {.fn anova} must be {.cls model_lazy} objects.",
            "x" = "Not a {.cls model_lazy}: argument{?s} {which(not_lazy) + 1L}."
        ))
    }

    valid_tests(test)

    all_lazy = c(list(object), rest)
    fitted = lapply(all_lazy, run_model_lazy_raw)
    build_anova(fitted, labels = character(0), test = test)
}

S7::method(anova, cld_exec) = function(object, ..., test = "F") {
    rest = list(...)

    not_exec = !vapply(rest, function(m) S7::S7_inherits(m, cld_exec), logical(1))
    if (any(not_exec)) {
        cli::cli_abort(c(
            "All arguments to {.fn anova} must be {.cls cld_exec} objects.",
            "x" = "Not a {.cls cld_exec}: argument{?s} {which(not_exec) + 1L}."
        ))
    }

    valid_tests(test)

    all_execs = c(list(object), rest)
    fitted = lapply(all_execs, function(e) e@data)
    build_anova(fitted, labels = character(0), test = test)
}

valid_tests = function(test) {
    valid = c("F", "LRT", "Chisq")
    if (!test %in% valid) {
        cli::cli_abort(c(
            "{.arg test} must be one of {.val {valid}}.",
            "x" = "Got {.val {test}}."
        ))
    }
}

build_anova = S7::new_generic("build_anova", "fitted")

S7::method(build_anova, S7::class_list) = function(fitted, labels, test) {
    if (length(fitted) < 2L) {
        cli::cli_abort("{.fn anova} requires at least 2 models.")
    }

    not_lm_object = !vapply(fitted, function(f) S7::S7_inherits(f, lm_object), logical(1))
    if (any(not_lm_object)) {
        cli::cli_abort(c(
            "All models must return an {.cls lm_object} to participate in {.fn anova}.",
            "x" = "Model{?s} {which(not_lm_object)} do{?es/} not."
        ))
    }

    responses = vapply(fitted, function(f) deparse(f@terms[[2L]]), character(1))
    if (length(unique(responses)) > 1L) {
        cli::cli_abort(c(
            "All models must have the same response variable.",
            "x" = "Found: {.val {unique(responses)}}."
        ))
    }

    nobs = vapply(fitted, function(f) length(f@residuals), integer(1))
    if (length(unique(nobs)) > 1L) {
        cli::cli_abort(
            "Models were not all fitted to the same number of observations."
        )
    }

    res_df = vapply(fitted, function(f) f@df_residual, numeric(1))
    rss = vapply(fitted, function(f) sum(f@residuals^2), numeric(1))
    df_diff = c(NA_real_, -diff(res_df))
    rss_diff = c(NA_real_, -diff(rss))

    big = which.min(res_df)
    scale = rss[big] / res_df[big]

    if (test == "F") {
        stat = rss_diff / df_diff / scale
        p_val = pf(stat, df_diff, res_df[big], lower.tail = FALSE)
        stat_col = "f_value"
    } else {
        stat = rss_diff / scale
        p_val = pchisq(stat, df_diff, lower.tail = FALSE)
        stat_col = "chisq_value"
    }

    tbl = tibble::tibble(
        model = if (length(labels) == length(fitted)) labels else as.character(seq_along(fitted)),
        res_df = res_df,
        rss = rss,
        df = df_diff,
        sum_sq = rss_diff,
        !!stat_col := stat,
        p_value = p_val
    )

    cld_anova(
        data = tbl,
        impl_cls = "anova_lm",
        stat_cls = "anova_lm",
        print_fn = NULL,
        name = "ANOVA",
        labels = labels,
        cld_meta = list(
            stat_name = "ANOVA",
            method = test,
            data_name = ""
        )
    )
}

#' @keywords internal
#' @noRd
run_model_lazy_raw = function(m) {
    model_type = if (inherits(m@model_id, "formula")) {
        "formula"
    } else {
        S7::S7_class(m@model_id)@name
    }
    def = find_def(m@model_spec@lookup, model_type = model_type)
    all_args = utils::modifyList(
        m@model_spec@args,
        m@recalibrate_spec$args %||% list()
    )
    inject_and_run(impl = def@impl$base, processed = m@processed, args = all_args)
}
