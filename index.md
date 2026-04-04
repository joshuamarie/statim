# statim ![](reference/figures/logo.png)

> This package is under active development. APIs may change.

**Higher Level Interface for Statistical Inference**

## Package Overview

What does [statim](https://github.com/joshuamarie/statim) mean?

[statim](https://github.com/joshuamarie/statim) is a Latin word for
“immediately, at once”. The name carries a double meaning:

- *stat*: as in statistics, the domain this package lives in
- *im* (*statim*): as in immediate, signalling that inference should be
  expressible as a direct declaration, not somewhat a sequence of
  mechanical steps

This simply means: you declare *what* statistical inference you want to
perform, then [statim](https://github.com/joshuamarie/statim)
immediately delivers *how*.

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

[statim](https://github.com/joshuamarie/statim) provides a
human-readable, easy-to-write syntax.
[statim](https://github.com/joshuamarie/statim) is a much higher-level
package, inherits most of strongest features in R that makes the code
easy to write and understand, such as
[dplyr](https://dplyr.tidyverse.org) /
[tidyr](https://tidyr.tidyverse.org)’s use of `<tidyselect-helpers>`.
This re-imagines the grammar of statistical inference implementation in
R. It embodies almost the same paradigm as
[ggplot2](https://ggplot2.tidyverse.org), except it enforces the use of
actual pipes, not `+`.

``` r
library(statim)
```

Here’s an example of a quick H-test pipeline:

1.  tidyverse-like grammar semantics

    ``` r
    sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        conclude()
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

    sleep |>
        define_model(x_by(extra, group)) |>
        prepare_test(TTEST) |>
        via("permute", n = 500L) |> 
        conclude()
    ```

    The bottom part of this code is the part where the “pipeline” is
    re-calibrated from the classical t-test procedure, into permutation
    t-test.

2.  1-liner syntax

    ``` r
    TTEST(x_by(extra, group), sleep)
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
    ```

&nbsp;

1.  is better since you can re-calibrate the declaration of the H-test
    you want to perform. Otherwise, (2) if you want to eagerly run the
    actual H-test.

## Core Ideas

The package is designed around three ideas:

1.  **Declarative models**: describe the structure of your data with
    [`define_model()`](https://joshuamarie.github.io/statim/reference/model-define-base.md)
    and model IDs like
    [`x_by()`](https://joshuamarie.github.io/statim/reference/x_by.md),
    [`rel()`](https://joshuamarie.github.io/statim/reference/rel.md),
    and
    [`pairwise()`](https://joshuamarie.github.io/statim/reference/pairwise.md)
2.  **Composable pipeline**: build up a test specification lazily, then
    execute with
    [`conclude()`](https://joshuamarie.github.io/statim/reference/conclude.md)
3.  **Extensible implementations**: every test is a
    [`test_define()`](https://joshuamarie.github.io/statim/reference/test_define.md)
    object; bring your own engine, your own method, your own
    implementation

## License

MIT © Joshua Marie

## Contributing

We are sincerely grateful for contributions; they are beneficial for the
project and for us as maintainers. Please read
[CONTRIBUTING.md](https://joshuamarie.github.io/statim/CONTRIBUTING.md)
for development setup, pull request guidelines, and workflow notes.

## Code of Conduct

Please note that the statim project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
