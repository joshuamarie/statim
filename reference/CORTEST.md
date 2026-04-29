# Correlation Test

`CORTEST()` performs a correlation test for one-to-one or many-to-one
variable relationships.

## Usage

``` r
CORTEST(.model = NULL, .data = NULL, ...)
```

## Arguments

- .model:

  A model ID from
  [`rel()`](https://joshuamarie.github.io/statim/reference/rel.md). When
  supplied, the test executes immediately. When `NULL` (default),
  returns a `test_spec` for use in the pipeline via
  [`prepare_test()`](https://joshuamarie.github.io/statim/reference/prepare-test.md).

- .data:

  A data frame. Only used on the standalone path.

- ...:

  Additional arguments passed to the implementation: `.cor_type`,
  `.alt`, `.ci` for the classical path.

## Value

An `htest_spec` object (standalone or eager), or a `test_spec` object
(pipeline).

## Supported model IDs

- [`rel()`](https://joshuamarie.github.io/statim/reference/rel.md) —
  many-to-one correlation test

## Examples

``` r
# eager
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

# pipeline
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
