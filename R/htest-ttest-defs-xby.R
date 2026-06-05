#' @title T-Test: Two-Sample (`x_by`)
#'
#' @description
#' The `x_by` implementation performs an independent or paired two-sample
#' t-test. It accepts one or more grouping variables via [x_by()].
#'
#' @section Arguments:
#' The following arguments are passed via `...` in [TTEST()] or [via()]:
#'
#' \describe{
#'   \item{`.paired`}{Logical. Whether to perform a paired t-test. Default `FALSE`.}
#'   \item{`.mu`}{Numeric. Hypothesized mean difference. Default `0`.}
#'   \item{`.alt`}{Direction: `"two.sided"`, `"greater"`, or `"less"`. Default `"two.sided"`.}
#'   \item{`.ci`}{Confidence level. Default `0.95`.}
#' }
#'
#' @section Variants:
#' \describe{
#'   \item{`"boot"`}{Bootstrap CI. Accepts `n` (reps) and `seed`.}
#'   \item{`"permute"`}{Permutation test. Accepts `n` and `seed`.}
#'   \item{`"weighted"`}{Weighted contrast. Accepts `.w`, `.mu`, `.ci`, `.op`.}
#' }
#'
#' @section Result class:
#' Returns a [class_ttest_two] object. All variants that also return
#' [class_ttest_two] inherit [auto_tidy()] and [print()] automatically.
#'
#' @section Hypothesis claims:
#' Supports [MU()] via [state_null()]. The `weighted` variant additionally
#' accepts contrast coefficients via `.w`.
#'
#' @examples
#' sleep |>
#'     define_model(x_by(extra, group)) |>
#'     prepare_test(TTEST) |>
#'     conclude()
#'
#' sleep |>
#'     define_model(x_by(extra, group)) |>
#'     prepare_test(TTEST) |>
#'     via("boot", n = 2000) |>
#'     conclude()
#'
#' @keywords internal
#' @name ttest-xby
#' @family ttest-implementations
NULL

ttest_def_two = test_define(
    model_type = x_by,
    impl = agendas(
        base = baseline(
            # ---- Default implementation ----
            fn = function(.proc, .paired = FALSE, .mu = 0, .alt = "two.sided", .ci = 0.95) {
                x = .proc$x_data[[1]]
                group_data = .proc$group_data

                tests = lapply(names(group_data), function(grp_name) {
                    grp = as.character(group_data[[grp_name]])
                    lvls = unique(grp)

                    if (length(lvls) != 2L) {
                        cli::cli_abort(c(
                            "Two-sample t-test requires exactly 2 groups.",
                            "i" = "Found {length(lvls)} group{{?s}} in {.val {grp_name}}."
                        ))
                    }

                    res = stats::t.test(
                        x = x[grp == lvls[[1]]],
                        y = x[grp == lvls[[2]]],
                        paired = .paired,
                        mu = .mu,
                        alternative = .alt,
                        conf.level = .ci
                    )

                    list(
                        group = grp_name,
                        # estimate = unname(res$estimate),
                        estimate = if (.paired) {
                            unname(res$estimate)
                        } else {
                            unname(res$estimate[[1]] - res$estimate[[2]])
                        },
                        t_stat = unname(res$statistic),
                        df = unname(res$parameter),
                        p_val = res$p.value,
                        lower_ci = res$conf.int[[1]],
                        upper_ci = res$conf.int[[2]]
                    )
                })

                class_ttest_two(
                    group = vapply(tests, \(x) x$group, character(1)),
                    estimate = vapply(tests, \(x) x$estimate, numeric(1)),
                    t_stat = vapply(tests, \(x) x$t_stat, numeric(1)),
                    df = vapply(tests, \(x) x$df, numeric(1)),
                    p_val = vapply(tests, \(x) x$p_val, numeric(1)),
                    lower_ci = vapply(tests, \(x) x$lower_ci, numeric(1)),
                    upper_ci = vapply(tests, \(x) x$upper_ci, numeric(1)),
                    ci_level = .ci
                )
            }
        ),
        weighted = variant(
            # ---- Weighted t-test ----
            # ---- variant: "weighted" ----
            fn = function(.proc, .mu = 0, .ci = 0.95, .w = NULL, .op = "==") {
                x = .proc$x_data[[1]]
                group_data = .proc$group_data

                grp_name = names(group_data)[[1]]
                grp = as.character(group_data[[grp_name]])
                lvls = unique(grp)

                if (length(lvls) != 2L) {
                    cli::cli_abort(c(
                        "Contrast t-test requires exactly 2 groups.",
                        "i" = "Found {length(lvls)} group{{?s}} in {.val {grp_name}}."
                    ))
                }

                x1 = x[grp == lvls[[1]]]
                x2 = x[grp == lvls[[2]]]
                n1 = length(x1)
                n2 = length(x2)
                xbar1 = mean(x1)
                xbar2 = mean(x2)
                s1 = stats::var(x1)
                s2 = stats::var(x2)

                coefs = if (is.null(.w)) {
                    c(1, -1)
                } else {
                    coef_nms = names(.w)
                    c(.w[coef_nms == lvls[[1]]], .w[coef_nms == lvls[[2]]])
                }

                c1 = coefs[[1]]
                c2 = coefs[[2]]
                est_val = c1 * xbar1 + c2 * xbar2
                se = sqrt(c1^2 * s1 / n1 + c2^2 * s2 / n2)
                tstat = (est_val - .mu) / se
                df = (c1^2 * s1 / n1 + c2^2 * s2 / n2)^2 /
                    ((c1^2 * s1 / n1)^2 / (n1 - 1) + (c2^2 * s2 / n2)^2 / (n2 - 1))

                p.value = switch(
                    .op,
                    "==" = 2 * stats::pt(-abs(tstat), df = df),
                    ">=" = , ">" = stats::pt(-tstat, df = df),
                    "<=" = , "<" = stats::pt(tstat, df = df),
                    "!=" = 2 * stats::pt(-abs(tstat), df = df)
                )

                alpha = 1 - .ci
                ci = switch(
                    .op,
                    "==" = , "!=" = {
                        t_crit = stats::qt(1 - alpha / 2, df = df)
                        c(.mu - t_crit * se, .mu + t_crit * se)
                    },
                    ">=" = , ">" = {
                        t_crit = stats::qt(1 - alpha, df = df)
                        c(.mu - t_crit * se, Inf)
                    },
                    "<=" = , "<" = {
                        t_crit = stats::qt(1 - alpha, df = df)
                        c(-Inf, .mu + t_crit * se)
                    }
                )

                class_ttest_two(
                    group = grp_name,
                    estimate = est_val,
                    t_stat = tstat,
                    df = df,
                    p_val = p.value,
                    lower_ci = ci[[1]],
                    upper_ci = ci[[2]],
                    ci_level = .ci
                )
            }
        ),
        boot = variant(
            # ---- Bootstrapping ----
            # ---- variant: boot ----
            fn = function(.proc, .ci = 0.95, n = 1000L, seed = NULL) {
                x = .proc$x_data[[1]]
                group_data = .proc$group_data

                if (!is.null(seed)) set.seed(seed)

                grp = as.character(group_data[[1]])
                lvls = unique(grp)

                idx1 = which(grp == lvls[[1]])
                idx2 = which(grp == lvls[[2]])

                boot_dist = replicate(n, {
                    b1 = x[sample(idx1, replace = TRUE)]
                    b2 = x[sample(idx2, replace = TRUE)]
                    mean(b1) - mean(b2)
                })

                ci = quantile(boot_dist, c((1 - .ci) / 2, 1 - (1 - .ci) / 2))
                list(boot_dist = boot_dist, ci = ci, n = n)
            },
            print = function(x, ...) {
                ci = round(x@data$ci, 4)
                summary_data = tibble::tibble(
                    names = c("CI", "n_reps"),
                    vals = c(paste0("[", ci[[1]], ", ", ci[[2]], "]"), x@data$n)
                )

                cli::cat_line(cli::rule(center = "Bootstrapped T-test", line = "="), "\n\n")
                cli::cat_line(cli::rule(left = "Summary", line = "-"), "\n")
                tabstats::table_summary(
                    summary_data,
                    style = tabstats::sm_style(sep = ":  "),
                    center_table = TRUE
                )
                cat("\n\n")
                invisible(x)
            }
        ),
        permute = variant(
            # ---- Permutation test ----
            # ---- variant: permute ----
            fn = function(.proc, n = 1000L, seed = NULL) {
                x = .proc$x_data[[1]]
                group_data = .proc$group_data

                if (!is.null(seed)) set.seed(seed)

                grp = as.character(group_data[[1]])
                lvls = unique(grp)

                obs = mean(x[grp == lvls[[1]]]) - mean(x[grp == lvls[[2]]])

                null_dist = replicate(n, {
                    perm = sample(x)
                    mean(perm[grp == lvls[[1]]]) - mean(perm[grp == lvls[[2]]])
                })

                list(
                    observed = obs,
                    null_dist = null_dist,
                    p.value = mean(abs(null_dist) >= abs(obs)),
                    n = n
                )
            },
            print = function(x, ...) {
                summary_data = tibble::tibble(
                    Statistic = round(x@data$observed, 4),
                    `p-value` = round(x@data$p.value, 4),
                    n_perms = x@data$n
                )

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

                cli::cat_line(cli::rule(center = "T-test Permutation", line = "="), "\n\n")
                cli::cat_line(cli::rule(left = "Summary", line = "-"), "\n")
                tabstats::table_default(
                    summary_data,
                    style_columns = tabstats::td_style(`p-value` = pval_styler)
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
            .mu = function(claim, processed) {
                resolved = claim_contrast_coefs(claim)
                coefs = resolved$coefs

                valid_two_sample = length(coefs) == 2L && identical(sort(unname(coefs)), c(-1, 1))
                valid_one_sample = length(coefs) == 1L && coefs == 1

                if (!valid_two_sample && !valid_one_sample) {
                    cli::cli_abort(c(
                        "T-test only supports simple mean differences.",
                        "i" = "Found weighted contrast: {.val {coefs}}.",
                        "i" = "Use {.code via(\"weighted\")} for weighted hypotheses."
                    ))
                }

                resolved$scalar
            },
            .alt = function(claim, processed = NULL) {
                switch(
                    claim@op,
                    "==" = , "!=" = "two.sided",
                    ">=" = , ">" = "less", # "greater",
                    "<=" = , "<" = "greater" # "less"
                )
            }
        ),
        weighted = map_claim(
            .mu = function(claim, processed) claim_contrast_coefs(claim)$scalar,
            .op = function(claim, processed) claim_contrast_coefs(claim)$op,
            .w = function(claim, processed) claim_contrast_coefs(claim)$coefs
        )
    )
)
