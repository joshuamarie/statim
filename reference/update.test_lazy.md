# Recalibrate arguments from the main pipeline

This function is ideal to transmute and modify the parameters being used
in the test under the pipeline.

## Usage

``` r
# S3 method for class 'test_lazy'
update(object, ...)
```

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
