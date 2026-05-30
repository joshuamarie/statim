
<!-- README.md is generated from README.Rmd. Please edit that file -->

> This package is under active development. APIs may change.

# statim <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/statim)](https://CRAN.R-project.org/package=statim)
[![R-CMD-check](https://github.com/joshuamarie/statim/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/joshuamarie/statim/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/joshuamarie/statim/graph/badge.svg)](https://app.codecov.io/gh/joshuamarie/statim)
<!-- badges: end -->

**Higher Level Interface for Statistical Inference**

## Package Overview

What does `{statim}` mean?

`{statim}` is a Latin word for “immediately, at once”. The name carries
a double meaning:

- *stat*: as in statistics, the domain this package lives in
- *im* (*statim*): as in immediate, signalling that inference should be
  expressible as a direct declaration, not somewhat a sequence of
  mechanical steps

This simply means: you declare *what* statistical inference you want to
perform, then `{statim}` immediately delivers *how*.

## Why statim?

Base R’s statistical functions are imperative and scattered. H-tests
like t-test through `t.test()`, and a correlation test through
`cor.test()`, each takes different arguments, returns a different output
format, and switching from a classical procedure to a bootstrap or
permutation variant means rewriting from scratch. There is no shared
grammar, and no way to describe *what* you want without also specifying
*how* to compute it step by step.

`{statim}` replaces that with a declarative pipeline. You describe the
model structure once with `define_model()`, attach a test, and
optionally recalibrate the method with `via()`, without touching
anything else:

``` r
# Base R: two functions, two different interfaces, two different output formats
t.test(extra ~ group, data = sleep)
cor.test(cars$speed, cars$dist)

# statim: same pipeline shape regardless of the test
sleep |> define_model(x_by(extra, group)) |> prepare_test(TTEST) |> conclude()
cars |> define_model(rel(speed, dist)) |> prepare_test(CORTEST) |> conclude()

# Switching to permutation is one word; nothing else in the pipeline moves
sleep |> define_model(x_by(extra, group)) |> prepare_test(TTEST) |> via("permute", n = 1000L) |> conclude()
```

Any test registered with `test_define()` plugs into the same pipeline,
including custom implementations. Most of the API is written in S7 with
the purpose to enforce flexibility and strictness to make `{statim}`
much usable and robust

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
pak::pak("joshuamarie/statim")
```

## Usages

``` r
library(statim)
```

### T-test

The pipeline form lets you recalibrate the method without rewriting
anything else. Switching from a classical t-test to a permutation t-test
is a single `via()` call:

``` r
# Classical
sleep |>
    define_model(x_by(extra, group)) |>
    prepare_test(TTEST) |>
    conclude()
```

    #> 
    #> == Model ======================================================================= 
    #> 
    #> Model ID : x_by 
    #> Args : extra | group 
    #>     x_vars : 1 
    #>     by_vars : 1 
    #> 
    #> == T-Test ====================================================================== 
    #> 
    #> -- Summary ---------------------------------------------------------------------
    #> 
    #> ─────────────────────────────────
    #>   groups   diff   t-stat  pval   
    #> ─────────────────────────────────
    #>   group   -1.580  -1.861  0.079  
    #> ─────────────────────────────────
    #> 
    #> 
    #> -- Confidence Interval ---------------------------------------------------------
    #> 
    #> ──────────────────────────────
    #>   groups  lower_95  upper_95  
    #> ──────────────────────────────
    #>   group    -3.365    0.205    
    #> ──────────────────────────────

``` r
# Permutation: one line added, nothing else changes
sleep |>
    define_model(x_by(extra, group)) |>
    prepare_test(TTEST) |>
    via("permute", n = 500L, seed = 123L) |>
    conclude()
```

    #> 
    #> == Model ======================================================================= 
    #> 
    #> Model ID : x_by 
    #> Args : extra | group 
    #>     x_vars : 1 
    #>     by_vars : 1 
    #> 
    #> == T-Test · permute ============================================================ 
    #> 
    #> ============================== T-test Permutation ==============================
    #> 
    #> 
    #> -- Summary ---------------------------------------------------------------------
    #> 
    #> ───────────────────────────────
    #>   Statistic  p-value  n_perms  
    #> ───────────────────────────────
    #>    -1.580     0.072     500    
    #> ───────────────────────────────

For a quick one-shot result, the eager form skips the pipeline entirely:

``` r
TTEST(x_by(extra, group), sleep)
```

    #> -- Summary ---------------------------------------------------------------------
    #> 
    #> ─────────────────────────────────
    #>   groups   diff   t-stat  pval   
    #> ─────────────────────────────────
    #>   group   -1.580  -1.861  0.079  
    #> ─────────────────────────────────
    #> 
    #> 
    #> -- Confidence Interval ---------------------------------------------------------
    #> 
    #> ──────────────────────────────
    #>   groups  lower_95  upper_95  
    #> ──────────────────────────────
    #>   group    -3.365    0.205    
    #> ──────────────────────────────

### Correlation test

The same pipeline shape works for any registered test. Here is the same
structure used for a correlation test:

``` r
cars |>
    define_model(rel(speed, dist)) |>
    prepare_test(CORTEST) |>
    conclude()
```

    #> 
    #> == Model ======================================================================= 
    #> 
    #> Model ID : rel 
    #> Args : speed ; dist 
    #>     x_vars : 1 
    #>     resp_vars : 1 
    #> 
    #> == Correlation Test ============================================================ 
    #> 
    #> -- Summary ---------------------------------------------------------------------
    #> 
    #> ─────────────────────────────────────────
    #>       pair      estimate  stat    pval   
    #> ─────────────────────────────────────────
    #>   dist ~ speed   0.807    9.464  <0.001  
    #> ─────────────────────────────────────────
    #> 
    #> 
    #> -- Confidence Interval ---------------------------------------------------------
    #> 
    #> ────────────────────────────────────
    #>       pair      lower_95  upper_95  
    #> ────────────────────────────────────
    #>   dist ~ speed   0.682     0.886    
    #> ────────────────────────────────────

## Core Ideas

The package is designed around three ideas:

1.  **Declarative models**: describe the structure of your data with
    `define_model()` and model IDs like `x_by()`, `rel()`, and
    `pairwise()`.
2.  **Composable pipeline**: build up a test specification lazily, then
    execute with `conclude()`.
3.  **Extensible implementations**: for instance, every test is a
    `test_define()` object; bring your own engine, your own method, your
    own implementation.

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
