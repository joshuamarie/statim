
<!-- README.md is generated from README.Rmd. Please edit that file -->

> This package is under active development. APIs may change.

# statim <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/statim)](https://CRAN.R-project.org/package=statim)
[![R-CMD-check](https://github.com/s7-stats/statim/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/s7-stats/statim/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/s7-stats/statim/graph/badge.svg)](https://app.codecov.io/gh/s7-stats/statim)
<!-- badges: end -->

**A Declarative Interface for Statistical Inference**

## Package Overview

What does `{statim}` mean?

`{statim}` is a Latin word for “immediately, at once”. The name carries
a double meaning:

- *stat*: as in statistics, the domain this package lives in
- *im* (*statim*): as in “immediate”, signalling that inference should
  be expressible as a direct declaration, not somewhat a sequence of
  mechanical steps

This simply means: you declare *what* statistical inference you want to
perform, then `{statim}` immediately delivers *how*.

## Why statim?

R has a rich statistical ecosystem, although it is yet for the use of S7
into statistical analysis to be a norm, which the existing R packages
are written based on S3, S4, Reference Class, or R6. Statistical
inference in general is served by an assortment of disconnected
functions: the functions you’re looking for may exist but they are
scattered across different packages.

R gained a grammar for graphics (`{ggplot2}`), and one for data
manipulation (`{dplyr}`). And then there’s `{statim}`, an attempt to
re-imagine the “grammar of statistical inference” from the ground up.
The core idea of `{statim}` in general is it’s fully declarative, and
that any inferential procedure can be described in [three
steps](#general-usage).

What makes `{statim}` *composable* for statistical workflows is the
*verbs* and the *accessibility* of the methods you’re looking for. For
example, you want to write a t-test pipeline, and you want to use the
classical one and then the permutation method. `{statim}` lets you do
that with `via()`, and while you can use t-test from `default`
(classical), you can access its permutation method through
`... |> via(permute)` (or whatever the keyword is) with one line of code
only. You won’t need you to do a lot of work (which sometimes require
rewriting your code), just a single addition to the syntax.

``` r
# Classical t-test
sleep |> 
    define_model(x_by(extra, group)) |> 
    prepare_test(TTEST) |> 
    conclude()

# Permutation t-test
sleep |> 
    define_model(x_by(extra, group)) |> 
    prepare_test(TTEST) |> 
    via("permute", n = 1000L) |>         # Here, one line added, nothing else changes
    conclude()
```

For a quick result, the eager form skips the piped syntax entirely:

``` r
# Only works for `stat_fn` functions
TTEST(x_by(extra, group), sleep)
```

But it’s not as expressive and assertive as the piped syntax form as
shown above, and you can’t process the output after executing this ([see
for more details](#core-semantics)).

## Installation

The package is yet to be submitted into CRAN.

``` r
# Stable version (not yet released)
install.packages("statim")
```

For the time being, you can install the current implementation on
GitHub:

``` r
# Development version from GitHub
# install.packages("pak")
pak::pak("s7-stats/statim")
```

## General Usage

By the way, loading a library comes with [a lot of
preferences](https://s7-stats.com/posts/06-load-pkg/). Let us start by
loading `{statim}` first:

``` r
library(statim)
```

All you need to know is that the usual workflow of `{statim}` comes with
three usual steps.

``` r
sleep |>                                # 1
    define_model(extra %by% group) |>   # 1              
    prepare_test(TTEST) |>              # 2            
    conclude() |>                       # 3           
    tidy()                              # 3          
```

Brief explanation of the code above:

1.  *Model processor and definition*, where defining the shape of model
    *to be analyzed* happens at the beginning during statistical
    inference. Typically, this step where supplying either a data frame
    or a `<model_id>` objects into `define_model()` occurs, and then
    some functions to be appended in the future updates.

2.  *Parameterization*, where the estimation process of the statistical
    inference pipeline is defined lazily. Our usual statistical
    inference application can be either a model-based inference
    (e.g. linear regression through `prepare_model()`) or H-test
    inference (e.g. t-test through `prepare_test()`). With that said,
    the execution is lazy-loaded, and only executed if needed.

3.  *Execution and retrieval*, where the first 2 steps is (re-)executed
    and then retrieve the output. The most common function is
    `conclude()`. There are several techniques to retrieve the output,
    e.g. through `tidy()`. This is functional if there are available
    methods are registered, automatically or from a manual step.

For more information, see through `vignette("statim")`, and learn more
about how `{statim}` works.

## Core Semantics

The package is designed around three ideas:

1.  **A shared grammar**: every inferential procedure follows the same
    shape — `define_model()`, `prepare_test()`, `conclude()`, regardless
    of which test or model ID is used. The model ID objects
    (e.g. `x_by`, `rel`, `pairwise`) defines the shape of the
    statistical inference throughout `{statim}` pipeline, while the
    grammar stays the same. Eager forms (`TTEST()`, `CORTEST()`, …)
    provide a shortcut when the full pipeline (in a form of piped syntax
    that reads like a sentence) is not needed.

2.  **Composable pipelines**: the pipeline has two forms: the eager form
    and the piped syntax form. The eager form skips the verbs and cannot
    be recalibrated, only skips to the output. On the other hand, the
    piped syntax form relies on verbs and lazy loading, which comes with
    the recalibration of the estimation method with a single `via()`
    call, and the execution of the lazy-loaded pipeline with
    `conclude()`.

3.  **Extensible by design**: to form an implementation is through
    filling up the `stat_define()` object (then store it within list of
    `defs` from `STAT_CONSTRUCTOR()` functions, saved as `<STAT_FN>`),
    then `baseline()` to write the default form of `<STAT_FN>` and
    `variant()` to extend the current `<STAT_FN>` form (only be accessed
    with `via()` only). With these, you can bring your own engine, your
    own method, your own implementation, or use them to extend the
    current ones.

## License

MIT © Joshua Marie

## Contributing

We are sincerely grateful for contributions; they are beneficial for the
project and for us as maintainers. Please read
[CONTRIBUTING.md](CONTRIBUTING.md) for development setup, pull request
guidelines, and workflow notes.

## Code of Conduct

Please note that the statim project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
