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

S7::method(update, anova_lazy) = function(object, ...) {
    dots = list(...)
    object@models = lapply(object@models, function(m) {
        if (!is.null(m@recalibrate_spec)) {
            m@recalibrate_spec$args = utils::modifyList(
                m@recalibrate_spec$args,
                dots
            )
        } else {
            m@model_spec@args = utils::modifyList(
                m@model_spec@args,
                dots
            )
        }
        m
    })

    object
}

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
        style_columns = tabstats::td_style(p_value = pval_styler),
        nrows = nrow(x@data)
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
#' @usage anova(object, ..., test = "F")
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

S7::method(print, anova_lazy) = function(x, ...) {
    m1 = x@models[[1]]
    spec = m1@model_spec

    method = m1@recalibrate_spec$method_name %||% "default"

    all_args = utils::modifyList(
        spec@args,
        m1@recalibrate_spec$args %||% list()
    )

    cat("\n")
    cat(cli::rule(left = "Models", line = "-"), "\n\n")
    for (i in seq_along(x@models)) {
        m = x@models[[i]]
        lbl = x@labels[[i]]
        formula_str = model_id_info(m@model_id)$args
        cat(sprintf("  %s : %s\n", lbl, formula_str))
    }

    cat("\n")
    cat(cli::rule(left = "Model Specification", line = "-"), "\n\n")
    cat("Model  :", spec@name, "\n")
    cat("Method :", method, "\n")

    if (length(all_args) > 0L) {
        args_str = paste(
            names(all_args),
            vapply(all_args, function(a) {
                if (is.function(a)) {
                    paste0(deparse(a[[1]]), "()")
                } else {
                    as.character(a)
                }
            }, character(1)),
            sep = " = ",
            collapse = ", "
        )
        cat("Args   :", args_str, "\n")
    }

    cat("\n")
    invisible(x)
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
    not_able = !vapply(fitted, function(f) S7::S7_inherits(f, anova_able), logical(1))
    if (any(not_able)) {
        cli::cli_abort(c(
            "All models must return an {.cls anova_able} object to participate in {.fn anova}.",
            "x" = "Model{?s} {which(not_able)} do{?es/} not."
        ))
    }

    # Single model: sequential (Type I) ANOVA
    if (length(fitted) == 1L) {
        obj = fitted[[1L]]
        if (!S7::S7_inherits(obj, class_lm_object)) {
            cli::cli_abort(c(
                "Single-model {.fn anova} is only supported for {.cls class_lm_object}.",
                "i" = "Pass at least 2 models for other model types."
            ))
        }
        stats_tbl = compute_anova_stats_single(obj)
        return(cld_anova(
            data = stats_tbl,
            impl_cls = "anova_lm",
            stat_cls = "anova_lm",
            print_fn = NULL,
            name = "ANOVA",
            labels = character(0),
            cld_meta = list(
                stat_name = "ANOVA",
                method = "Type I",
                data_name = ""
            )
        ))
    }

    # Multi-model: incremental F / LRT
    if (length(fitted) < 2L) {
        cli::cli_abort("{.fn anova} requires at least 2 models.")
    }

    responses = vapply(fitted, function(f) deparse(f@terms[[2L]]), character(1))
    if (length(unique(responses)) > 1L) {
        cli::cli_abort(c(
            "All models must have the same response variable.",
            "x" = "Found: {.val {unique(responses)}}."
        ))
    }

    families = vapply(fitted, function(f) f@family, character(1))
    if (length(unique(families)) > 1L) {
        cli::cli_abort(c(
            "All models must belong to the same error family.",
            "x" = "Found: {.val {unique(families)}}.",
            "i" = "Comparing models across families is not meaningful."
        ))
    }

    nobs = vapply(fitted, function(f) length(f@terms), integer(1))
    if (length(unique(nobs)) > 1L) {
        cli::cli_abort(
            "Models were not all fitted to the same number of observations."
        )
    }

    family = families[[1L]]
    stats_tbl = compute_anova_stats(fitted, labels = labels, family = family, test = test)

    cld_anova(
        data = stats_tbl,
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

#' Compute ANOVA test statistics from a list of anova_able objects
#'
#' Dispatches on family to select the right test statistic. Gaussian models
#' use RSS-based F or chi-squared. All other families use deviance-based LRT.
#' The `test` argument is honoured for Gaussian; for non-Gaussian families
#' it is forced to `"LRT"` with a message.
#'
#' @param fitted A list of `anova_able` objects, sorted by increasing
#'   complexity (fewest to most parameters).
#' @param labels If `length(mod) == 1L` in [anova()], the type I ANOVA will return
#'   the terms of the covariate. Otherwise, the name of the model itself.
#' @param family A string, e.g. `"gaussian"`, `"binomial"`, `"poisson"`.
#' @param test One of `"F"`, `"LRT"`, `"Chisq"`.
#'
#' @return A tibble with one row per model.
#'
#' @keywords internal
#' @noRd
compute_anova_stats = function(fitted, labels, family, test) {
    res_df = vapply(fitted, function(f) f@df_residual, numeric(1))
    dev = vapply(fitted, function(f) f@deviance, numeric(1))
    df_diff = c(NA_real_, -diff(res_df))
    dev_diff = c(NA_real_, -diff(dev))

    if (family != "gaussian" && test == "F") {
        cli::cli_inform(c(
            "!" = "F-test is only valid for Gaussian models.",
            "i" = "Switching to LRT for family {.val {family}}."
        ))
        test = "LRT"
    }

    if (test == "F") {
        big = which.min(res_df)
        scale = dev[big] / res_df[big]
        stat = dev_diff / df_diff / scale
        p_val = pf(stat, df_diff, res_df[big], lower.tail = FALSE)
        stat_col = "f_value"
    } else {
        scale = vapply(fitted, function(f) f@dispersion, numeric(1))
        stat = dev_diff / scale[[which.min(res_df)]]
        p_val = pchisq(stat, df_diff, lower.tail = FALSE)
        stat_col = "chisq_value"
    }

    tibble::tibble(
        model = if (length(labels) == length(fitted)) labels else as.character(seq_along(fitted)),
        res_df = res_df,
        deviance = dev,
        df = df_diff,
        dev_diff = dev_diff,
        !!stat_col := stat,
        p_value = p_val
    )
}

#' Type I ANOVA
#'
#' @keywords internal
#' @noRd
compute_anova_stats_single = function(obj) {
    if (is.null(obj@x_mat)) {
        cli::cli_abort(c(
            "Single-model {.fn anova} requires {.field x_mat} to be populated.",
            "i" = "Set {.code x_mat = as.numeric(stats::model.matrix(fit))} when constructing {.cls class_lm_object}."
        ))
    }

    trms = obj@terms
    all_labels = attr(trms, "term.labels")
    resp = as.character(attr(trms, "variables")[[attr(trms, "response") + 1L]])
    term_labels = all_labels[all_labels != resp]
    has_intercept = attr(trms, "intercept") == 1L

    y = obj@fitted + obj@residuals
    n = length(y)
    df_res = obj@df_residual
    ncols = length(obj@beta)
    X = matrix(obj@x_mat, nrow = n, ncol = ncols)

    rss_baseline = if (has_intercept) sum((y - mean(y))^2) else sum(y^2)

    start_col = if (has_intercept) 2L else 1L
    rss_seq = numeric(length(term_labels))
    for (k in seq_along(term_labels)) {
        cols = seq_len(k) + (start_col - 1L)
        X_sub = if (has_intercept) cbind(1, X[, cols, drop = FALSE]) else X[, cols, drop = FALSE]
        rss_seq[k] = sum(.lm.fit(X_sub, y)$residuals^2)
    }

    rss_all = c(rss_baseline, rss_seq)
    ss_terms = -diff(rss_all)
    rss_final = rss_seq[length(rss_seq)]
    ms_res = rss_final / df_res
    f_val = ss_terms / ms_res
    p_val = pf(f_val, 1L, df_res, lower.tail = FALSE)

    df_terms = rep(1L, length(term_labels))

    tibble::tibble(
        term = c(term_labels, "Residuals"),
        df = c(df_terms, df_res),
        ss = c(ss_terms, rss_final),
        ms = c(ss_terms / df_terms, rss_final / df_res),
        f_value = c(f_val, NA_real_),
        p_value = c(p_val, NA_real_)
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

#' Protocol class for ANOVA participation
#'
#' Any model result container that should participate in [anova()] must
#' inherit from `anova_able`. Subclasses fill the four required slots;
#' `build_anova()` reads only those slots and dispatches the test statistic
#' computation on `@family`.
#'
#' @slot terms The model terms object. Used to verify response consistency.
#' @slot df_residual Residual degrees of freedom.
#' @slot deviance Scalar deviance measure. For Gaussian LMs this is the
#'   residual sum of squares. For GLMs this is the model deviance from
#'   [stats::deviance()].
#' @slot dispersion Scalar dispersion parameter. For Gaussian LMs this is
#'   `sigma^2` (`rss / df_residual`). For GLMs with a known dispersion
#'   (binomial, Poisson) set to `1`. For quasi-families use the estimated
#'   Pearson dispersion.
#' @slot family A string identifying the error family, e.g. `"gaussian"`,
#'   `"binomial"`, `"poisson"`. Used by `build_anova()` to select the
#'   correct test statistic. Must be consistent across all models passed to
#'   a single [anova()] call.
#'
#' @seealso [anova()]
#'
#' @keywords internal
anova_able = S7::new_class(
    "anova_able",
    parent = class_stat_infer,
    properties = list(
        terms = S7::new_property(class = S7::class_any),
        df_residual = S7::class_numeric,
        deviance = S7::class_numeric,
        dispersion = S7::class_numeric,
        family = S7::new_property(class = S7::class_character, default = "gaussian")
    )
)
