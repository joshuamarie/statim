ttest_def_two = test_define(
    model_type = "x_by",
    impl_class = "ttest_two",
    fun_args = fun_args(
        .paired = FALSE,
        .mu = 0,
        .alt = "two.sided",
        .ci = 0.95
    ),
    vars = list(
        x = function(p) p$x_data[[1]],
        group = function(p) p$group_data[[1]]
    ),
    run = function(self) {
        grp = as.character(ic_pull(self, "group"))
        resp = ic_pull(self, "x")
        lvls = unique(grp)

        if (length(lvls) != 2L) {
            cli::cli_abort(c(
                "Two-sample t-test requires exactly 2 groups.",
                "i" = "Found {length(lvls)} group{{?s}} in {.val {ic_name(self, 'group')}}."
            ))
        }

        stats::t.test(
            x = resp[grp == lvls[[1]]],
            y = resp[grp == lvls[[2]]],
            paired = ic_arg(self, ".paired"),
            mu = ic_arg(self, ".mu"),
            alternative = ic_arg(self, ".alt"),
            conf.level = ic_arg(self, ".ci")
        )
    },
    print = function(x, ...) {
        rlang::check_installed(c("broom", "pander"),
            reason = "to print t-test results in tabular form")
        pander::pander(broom::tidy(x$data))
        invisible(x)
    }
)

ttest_def_boot = test_define(
    model_type = "x_by",
    impl_class = "ttest_boot",
    method = method_spec(
        "boot",
        method_type = "bootstrap",
        defaults = list(n = 1000L, seed = NULL)
    ),
    vars = list(
        x = function(p) p$x_data[[1]],
        group = function(p) p$group_data[[1]]
    ),
    run = function(self) {
        n = ic_method_arg(self, "n")
        seed = ic_method_arg(self, "seed")

        if (!is.null(seed)) set.seed(seed)

        grp = as.character(ic_pull(self, "group"))
        resp = ic_pull(self, "x")
        lvls = unique(grp)

        idx1 = which(grp == lvls[[1]])
        idx2 = which(grp == lvls[[2]])

        boot_dist = replicate(n, {
            b1 = resp[sample(idx1, replace = TRUE)]
            b2 = resp[sample(idx2, replace = TRUE)]
            mean(b1) - mean(b2)
        })

        ci = quantile(
            boot_dist,
            c(
                (1 - ic_arg(self, ".ci", 0.95)) / 2,
                1 - (1 - ic_arg(self, ".ci", 0.95)) / 2
            )
        )

        list(
            boot_dist = boot_dist,
            ci = ci,
            n = n
        )
    },
    print = function(x, ...) {
        ci = round(x$data$ci, 4)
        cli::cli_text("{.field Bootstrap CI} : [{ci[[1]]}, {ci[[2]]}]")
        cli::cli_text("{.field Replicates}   : {x$data$n}")
        invisible(x)
    }
)

ttest_def_permute = test_define(
    model_type = "x_by",
    impl_class = "ttest_permute",
    engine = "default",
    method = method_spec(
        "permute",
        method_type = "replicate",
        defaults = list(n = 1000L, seed = NULL)
    ),
    vars = list(
        x = function(p) p$x_data[[1]],
        group = function(p) p$group_data[[1]]
    ),
    run = function(self) {
        n = ic_method_arg(self, "n")
        seed = ic_method_arg(self, "seed")

        if (!is.null(seed)) set.seed(seed)

        grp = as.character(ic_pull(self, "group"))
        resp = ic_pull(self, "x")
        lvls = unique(grp)

        obs = mean(resp[grp == lvls[[1]]]) -
            mean(resp[grp == lvls[[2]]])

        null_dist = replicate(n, {
            perm = sample(resp)
            mean(perm[grp == lvls[[1]]]) -
                mean(perm[grp == lvls[[2]]])
        })

        list(
            observed  = obs,
            null_dist = null_dist,
            p.value = mean(abs(null_dist) >= abs(obs)),
            n = n
        )
    },
    print = function(x, ...) {
        cli::cli_text("{.field Observed}      : {round(x$data$observed, 4)}")
        cli::cli_text("{.field p-value (perm)}: {round(x$data$p.value, 4)}")
        cli::cli_text("{.field Permutations}  : {x$data$n}")
        invisible(x)
    }
)

ttest_def_permute_rfast = test_define(
    model_type = "x_by",
    impl_class = "ttest_permute_rfast",
    engine = "rfast",
    method = method_spec(
        "permute",
        method_type = "replicate",
        defaults = list(B = 999L)
    ),
    vars = list(
        x = function(p) p$x_data[[1]],
        group = function(p) p$group_data[[1]]
    ),
    run = function(self) {
        rlang::check_installed("Rfast2",
            reason = "to run the Rfast2-backed permutation t-test engine")

        B = ic_method_arg(self, "B")
        grp = as.character(ic_pull(self, "group"))
        resp = ic_pull(self, "x")
        lvls = unique(grp)

        if (length(lvls) != 2L)
            cli::cli_abort(c(
                "Permutation t-test requires exactly 2 groups.",
                "i" = "Found {length(lvls)} group{{?s}}."
            ))

        x = resp[grp == lvls[[1]]]
        y = resp[grp == lvls[[2]]]

        # Rfast2 requires numeric vectors, no NAs
        res = Rfast2::perm.ttest(x = x, y = y, B = B)

        list(
            stat = res[["stat"]],
            p.value = res[["permutation p-value"]],
            B = B
        )
    },
    print = function(x, ...) {
        cli::cli_text("{.field Statistic}            : {round(x$data$stat, 4)}")
        cli::cli_text("{.field p-value (permutation)}: {round(x$data$p.value, 4)}")
        cli::cli_text("{.field Permutations}         : {x$data$B}")
        invisible(x)
    }
)

#' T-Test
#'
#' `TTEST()` performs a t-test for one-sample, two-sample, paired, pairwise,
#' or formula-based comparisons.
#'
#' @param .model A model ID from `x_by()`, `pairwise()`, or a formula.
#'   When supplied, the test executes immediately. When `NULL` (default),
#'   returns a `test_spec` for use in the pipeline via [prepare_test()].
#' @param .data A data frame. Only used on the standalone path.
#' @param ... Additional arguments passed to the implementation:
#'   `.paired`, `.mu`, `.alt`, `.ci` for the classical path.
#' @param .extra_defs A list of additional `test_define` objects supplied
#'   by the user. These extend the available implementations and engines.
#'
#' @return An `htest_spec` object (standalone or eager), or a `test_spec`
#'   object (pipeline).
#'
#' @section Supported model IDs:
#' - `x_by()` — two-sample or paired t-test
#'
#' @section Method variants:
#' - `"boot"` — bootstrap confidence interval via [via()]
#'
#' @examples
#' # standalone
#' TTEST(x_by(extra, group), sleep)
#'
#' # Main pipeline
#' sleep |>
#'     define_model(x_by(extra, group)) |>
#'     prepare_test(TTEST) |>
#'     conclude()
#'
#' # bootstrap
#' sleep |>
#'     define_model(x_by(extra, group)) |>
#'     prepare_test(TTEST) |>
#'     via("boot", n = 2000) |>
#'     conclude()
#'
#' @export
TTEST = HTEST_FN(
    cls = "ttest",
    defs = list(ttest_def_two, ttest_def_boot, ttest_def_permute, ttest_def_permute_rfast),
    .name = "T-Test"
)
