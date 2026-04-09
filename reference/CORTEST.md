# Correlation test

`CORTEST()` performs a t-test for one-sample, two-sample, paired,
pairwise, or formula-based comparisons.

## Usage

``` r
CORTEST(.model = NULL, .data = NULL, ..., .extra_defs = list())
```

## Arguments

- .model:

  A model ID from
  [`rel()`](https://joshuamarie.github.io/statim/reference/rel.md),
  [`pairwise()`](https://joshuamarie.github.io/statim/reference/pairwise.md),
  or a formula. When supplied, the test executes immediately. When
  `NULL` (default), returns a `test_spec` for use in the pipeline via
  [`prepare_test()`](https://joshuamarie.github.io/statim/reference/prepare-test.md).

- .data:

  A data frame. Only used on the standalone path.

- ...:

  Additional arguments passed to the implementation: `.method`, `.ci`
  for the classical path.

- .extra_defs:

  A list of additional `test_define` objects supplied by the user. These
  extend the available implementations and engines.

## Value

An `htest_spec` object (standalone or eager), or a `test_spec` object
(pipeline).

## Supported model IDs

- [`rel()`](https://joshuamarie.github.io/statim/reference/rel.md):
  Many-to-one correlation test

- [`pairwise()`](https://joshuamarie.github.io/statim/reference/pairwise.md):
  Pairwise correlation test

## Examples

``` r
# standalone
CORTEST(rel(speed, dist), cars)
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
#> 
#> 

# Main pipeline
cars |>
    define_model(rel(speed, dist)) |>
    prepare_test(CORTEST) |>
    conclude()
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
#> 
#> 
```
