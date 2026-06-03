#' @title T-Test: Pairwise (`pairwise`)
#'
#' @description
#' The `pairwise` implementation performs pairwise t-tests across a set of
#' numeric variables. Each pair of variables is compared independently, and
#' results are presented as a matrix.
#'
#' Use [pairwise()] as the model ID to select this implementation.
#'
#' @section Arguments:
#' The following arguments are passed via `...` in [TTEST()]:
#'
#' \describe{
#'   \item{`.paired`}{Logical. Whether to perform paired comparisons.
#'     Default `FALSE`.}
#'   \item{`.mu`}{Numeric. Hypothesized mean or mean difference. Length 1
#'     (applied to all pairs) or one value per variable. Default `0`.}
#'   \item{`.alt`}{String. One of `"two.sided"`, `"greater"`, or `"less"`.
#'     Default `"two.sided"`.}
#'   \item{`.ci`}{Numeric. Confidence level. Default `0.95`.}
#' }
#'
#' @section Variants:
#' No variants are currently registered for the `pairwise` path. Use
#' [add_variant()] to register custom variants at the user or package level.
#'
#' @section Result class:
#' Returns a [class_ttest_pairwise] object inheriting from [class_stat_infer].
#' Results are printed as a pairwise matrix via [tabstats::pairwise_matrix()].
#'
#' @section One-sample mode:
#' When [pairwise()] is constructed with a `direction = "eq"` argument, each
#' variable is tested against its own `.mu` value rather than against another
#' variable. The result matrix displays diagonal entries only.
#'
#' @examples
#' iris |>
#'     define_model(pairwise(Sepal.Length, Sepal.Width, Petal.Length)) |>
#'     prepare_test(TTEST) |>
#'     conclude()
#'
#' @keywords internal
#' @name ttest-pairwise
#' @family ttest-implementations
NULL

ttest_def_pairwise = test_define(
    model_type = pairwise,
    impl = agendas(
        base = baseline(
            fn = function(.proc, .paired = FALSE, .mu = 0, .alt = "two.sided", .ci = 0.95) {
                var_names = .proc$var_names
                pairs = .proc$pairs
                data = .proc$data
                direction = .proc$direction %||% "lt"
                is_one_sample = direction == "eq"

                n_vars = length(var_names)

                if (length(.mu) == 1L) {
                    .mu = rep(.mu, n_vars)
                } else if (length(.mu) != n_vars) {
                    cli::cli_abort(c(
                        "{.arg .mu} must be length 1 or length {n_vars} (one per variable).",
                        "i" = "Variables: {.val {var_names}}.",
                        "x" = "Got length {length(.mu)}."
                    ))
                }
                names(.mu) = var_names

                tests = lapply(seq_along(pairs), function(i) {
                    a = pairs[[i]][[1]]
                    b = pairs[[i]][[2]]

                    res = if (is_one_sample) {
                        stats::t.test(
                            x = data[[a]],
                            mu = .mu[[a]],
                            alternative = .alt,
                            conf.level = .ci
                        )
                    } else {
                        stats::t.test(
                            x = data[[a]],
                            y = data[[b]],
                            paired = .paired,
                            mu = .mu[[a]] - .mu[[b]],
                            alternative = .alt,
                            conf.level = .ci
                        )
                    }

                    list(a = a, b = b, ttest = res)
                })

                class_ttest_pairwise(
                    var1 = vapply(tests, function(x) x[["a"]], character(1)),
                    var2 = vapply(tests, function(x) x[["b"]], character(1)),
                    est = vapply(tests, function(t) {
                        est = t$ttest$estimate
                        if (length(est) == 2L) est[[1L]] - est[[2L]] else est[[1L]]
                    }, numeric(1)),
                    df = vapply(tests, function(t) t$ttest$parameter[["df"]], numeric(1)),
                    t_stat = vapply(tests, function(t) t$ttest$statistic[["t"]], numeric(1)),
                    p_value = vapply(tests, function(t) t$ttest$p.value, numeric(1)),
                    method_name = unique(
                        vapply(tests, function(t) t$ttest$method, character(1))
                    )
                )
            }
        )
    )
)

#' Structured result container for pairwise t-tests
#'
#' @description
#' An S7 class produced by [TTEST] pipelines using [pairwise()] as the
#' model ID. Not constructed manually — use the pipeline instead.
#'
#' Inherits from [class_stat_infer], so [auto_tidy()] dispatches on it
#' automatically. Downstream packages can use it as a `parent` in
#' `S7::new_class()`.
#'
#' @usage NULL
#'
#' @details
#' Slots (populated automatically by [TTEST]):
#'
#' - `var1`: first variable in each pair.
#' - `var2`: second variable in each pair.
#' - `est`: mean difference per pair (or sample mean for one-sample mode).
#' - `df`: degrees of freedom per pair.
#' - `t_stat`: t-statistic per pair.
#' - `p_value`: p-value per pair.
#' - `method_name`: scalar string describing the test method, taken directly
#'   from [stats::t.test()]. Must be length 1 — all pairs must share the
#'   same method.
#'
#' @section One-sample mode:
#' When [pairwise()] uses `direction = "eq"`, `var1` and `var2` are
#' identical (each variable tested against itself). [print()] detects this
#' and renders a diagonal-only matrix.
#'
#' @seealso [TTEST], [ttest-pairwise], [auto_tidy()], [class_stat_infer]
#'
#' @export
class_ttest_pairwise = S7::new_class(
    "class_ttest_pairwise",
    parent = class_stat_infer,
    properties = list(
        var1 = S7::class_character,
        var2 = S7::class_character,
        est = S7::class_numeric,
        df = S7::class_numeric,
        t_stat = S7::class_numeric,
        p_value = S7::class_numeric,
        method_name = S7::new_property(
            class = S7::class_character,
            default = "",
            validator = function(value) {
                if (length(value) != 1L)
                    paste0("`method_name` must be length 1, not ", length(value), ".")
            }
        )
    )
)

S7::method(print, class_ttest_pairwise) = function(x, ...) {
    is_one_sample = all(x@var1 == x@var2)

    if (is_one_sample) {
        vars = x@var1
        grid = expand.grid(var1 = vars, var2 = vars, stringsAsFactors = FALSE)

        diff_vec = rep("", nrow(grid))
        t_vec = rep("", nrow(grid))
        pval_vec = rep("", nrow(grid))

        for (k in seq_along(vars)) {
            idx = which(grid$var1 == vars[[k]] & grid$var2 == vars[[k]])
            diff_vec[[idx]] = formatC(x@est[[k]], digits = 3, format = "f")
            t_vec[[idx]] = formatC(x@t_stat[[k]], digits = 3, format = "f")
            pval_vec[[idx]] = formatC(x@p_value[[k]], digits = 3, format = "f")
        }

        spec = tabstats::new_pairwise_data(
            var1 = grid$var1,
            var2 = grid$var2,
            diff = diff_vec,
            t_stat = t_vec,
            pval = pval_vec
        )
    } else {
        spec = tabstats::new_pairwise_data(
            var1 = x@var1,
            var2 = x@var2,
            diff = formatC(x@est, digits = 3, format = "f"),
            t_stat = formatC(x@t_stat, digits = 3, format = "f"),
            pval = formatC(x@p_value, digits = 3, format = "f")
        )
    }

    tabstats::pairwise_matrix(
        spec,
        title = if (nzchar(x@method_name)) x@method_name else "Pairwise t-Tests",
        layout_view = TRUE,
        diag_1 = FALSE,
        style = tabstats::cm_style(
            pval = function(x) {
                x_num = suppressWarnings(as.numeric(x))
                if (is.na(x_num) || x_num > 0.05) {
                    cli::style_italic(x)
                } else if (x_num > 0.01) {
                    cli::col_red(x)
                } else {
                    cli::style_bold("<0.001")
                }
            }
        )
    )

    invisible(x)
}
