# Recalibrate arguments from the main pipeline

[`update()`](https://rdrr.io/r/stats/update.html) modifies the arguments
of a lazy test pipeline without changing the method variant or engine.

## Usage

``` r
# S3 method for class 'test_lazy'
update(object, ...)
```

## Arguments

- object:

  A `test_lazy` object.

- ...:

  Named arguments to update.

## Value

The modified `test_lazy` object.

## Examples

``` r
sleep |>
    define_model(extra ~ group) |>
    prepare_test(TTEST) |>
    update(.paired = TRUE) |>
    conclude()
#> -- Summary ---------------------------------------------------------------------
#> 
#> ───────────────────────────────────────────────────────
#>   groups     type     est_type   est    t-stat  pval   
#> ───────────────────────────────────────────────────────
#>   group   two sample  mu_diff   -1.580  -1.861  0.079  
#> ───────────────────────────────────────────────────────
#> 
#> 
#> -- Confidence Interval ---------------------------------------------------------
#> 
#> ──────────────────────────────────────────
#>   groups     type     lower_95  upper_95  
#> ──────────────────────────────────────────
#>   group   two sample   -3.365    0.205    
#> ──────────────────────────────────────────
#> 
#> 
```
