
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
- *im* (*statim*): as in “immediate”, signalling that inference should
  be expressible as a direct declaration, not somewhat a sequence of
  mechanical steps

This simply means: you declare *what* statistical inference you want to
perform, then `{statim}` immediately delivers *how*.

## Why statim?

R has a rich statistical ecosystem, but hypothesis testing is served by
an assortment of disconnected functions. R gained a grammar for graphics
(`{ggplot2}`) and one for data manipulation (`{dplyr}`), but statistical
inference has no equivalent: each testing function ships with its own
interface, its own way of specifying data, and its own output format.
There is no shared grammar for inference: no way to say *what* you want
to test without simultaneously committing to *how* the procedure carries
it out.

`{statim}` is an attempt to re-imagine this from the ground up, the same
way `{ggplot2}` introduced a grammar for graphics without replacing base
plotting functions. The core idea is that any inferential procedure can
be described in three steps: define the structure of the data
(`define_model()`), declare what you want to infer (`prepare_test()`),
and optionally recalibrate the estimation method (`via()`). The
procedure executes only when you call `conclude()`.

This separation matters because it makes statistical workflows
*composable*. Switching from a classical to a permutation procedure does
not require rewriting your code; it is a single addition to the
pipeline:

``` r
# Classical t-test
sleep |> define_model(x_by(extra, group)) |> prepare_test(TTEST) |> conclude()

# Permutation t-test: one line added, nothing else changes
sleep |> define_model(x_by(extra, group)) |> prepare_test(TTEST) |> via("permute", n = 1000L) |> conclude()

# The same pipeline shape works for any registered test
cars |> define_model(rel(speed, dist)) |> prepare_test(CORTEST) |> conclude()

# For a quick result, the eager form skips the pipeline entirely
TTEST(x_by(extra, group), sleep)
CORTEST(rel(speed, dist), cars)
```

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

## General Usage

Loading a library comes with preference. In this example, `library()` is
used for a simple demonstration:

``` r
library(statim)
```

All you need to know is that the usual workflow of `{statim}` has three
usual steps:

``` r
sleep |>                                # 1
    define_model(extra %by% group) |>   # 1              
    prepare_test(TTEST) |>              # 2         
    update(.ci = 0.9) |>                # 2      
    conclude() |>                       # 3           
    tidy()                              # 3          
```

1.  *Model processor and definition*, where defining the model *to be
    analyzed* happens at the beginning during statistical inference.
    Typically, this step where supplying either a data frame or a
    `<model_id>` objects into `define_model()` occurs, and then some
    functions to be appended in the future updates.

2.  *Parameterization* and proceed to writing the estimation process of
    the statistical inference pipeline. It is either a model-based
    inference (e.g. linear regression) or H-test inference
    (e.g. t-test). They are lazy-loaded, and you should be able to do
    anything.

3.  *Execution and retrieval* then (re-)executes the first 2 steps and
    retrieves the output.

See through `vignette("statim")`, and learn more about the API design as
a starter.

## Core Semantics

The package is designed around three ideas:

1.  **A shared grammar**: every inferential procedure follows the same
    shape – `define_model()`, `prepare_test()`, `conclude()` –
    regardless of which test or model ID is used. The model ID objects
    (e.g. `x_by`, `rel`, `pairwise`) determines what the test does; the
    grammar stays the same. Eager forms (`TTEST()`, `CORTEST()`, …)
    provide a shortcut when the full pipeline is not needed.

2.  **Composable pipelines**: build up a test specification lazily,
    recalibrate the estimation method with a single `via()` call, and
    execute with `conclude()`.

3.  **Extensible by design**: every test is a `stat_define()` object;
    bring your own engine, your own method, your own implementation.
    Auto dispatch handles `tidy()` for your method without requiring you
    to write it.

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
