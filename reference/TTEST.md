# T-Test

`TTEST()` performs a t-test for one-sample, two-sample, paired,
pairwise, or formula-based comparisons.

## Usage

``` r
TTEST(.model = NULL, .data = NULL, ..., .extra_defs = list())
```

## Arguments

- .model:

  A model ID from
  [`x_by()`](https://joshuamarie.github.io/statim/reference/x_by.md),
  [`pairwise()`](https://joshuamarie.github.io/statim/reference/pairwise.md),
  or a formula. When supplied, the test executes immediately. When
  `NULL` (default), returns a `test_spec` for use in the pipeline via
  [`prepare_test()`](https://joshuamarie.github.io/statim/reference/prepare-test.md).

- .data:

  A data frame. Only used on the standalone path.

- ...:

  Additional arguments passed to the implementation: `.paired`, `.mu`,
  `.alt`, `.ci` for the classical path.

- .extra_defs:

  A list of additional `test_define` objects supplied by the user. These
  extend the available implementations and engines.

## Value

An `htest_spec` object (standalone or eager), or a `test_spec` object
(pipeline).

## Supported model IDs

- [`x_by()`](https://joshuamarie.github.io/statim/reference/x_by.md) —
  two-sample or paired t-test

## Method variants

- `"boot"` — bootstrap confidence interval via
  [`via()`](https://joshuamarie.github.io/statim/reference/via.md)

## Examples

``` r
# standalone
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
#> 
#> 

# Main pipeline
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
#> 
#> 

# bootstrap
sleep |>
    define_model(x_by(extra, group)) |>
    prepare_test(TTEST) |>
    via("boot", n = 2000) |>
    conclude()
#> ============================== Bootstrapped T-test =============================
#> 
#> 
#> -- Summary ---------------------------------------------------------------------
#> 
#> Warning: running command 'tput cols' had status 2
#> ------------------------------
#>   CI     :   [-3.21, 0.0702]
#>   n_reps :              2000
#> ------------------------------
#> 
#> 
```
