# statim ![](reference/figures/logo.png)

> This package is under active development. APIs may change.

**Higer Level Interface for Statistical Inference**

## What does *statim* mean?

*statim* is a Latin word for “immediately, at once”. The name carries a
double meaning:

- *stat*: as in statistics, the domain this package lives in
- *im* (*statim*): as in immediate, signalling that inference should be
  expressible as a direct declaration, not a sequence of mechanical
  steps

The philosophy: you declare *what* you want to infer.
[statim](https://github.com/joshuamarie/statim) figures out *how*.

## Overview

[statim](https://github.com/joshuamarie/statim) re-imagines the grammar
of statistical inference in R. You think about statistical statements:

``` r
sleep |>
    define_model(x_by(extra, group)) |>
    prepare_test(TTEST) |>
    conclude()
```

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

## Installation

``` r
# Development version from GitHub
# install.packages("pak")
pak::pak("joshuamarie/statim")
```

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
