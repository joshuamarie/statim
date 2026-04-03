# Lazily prepare a single test

`prepare_test()` attaches a test specification to a `def_model` object,
producing a `test_lazy` ready for optional recalibration with
[`via()`](https://joshuamarie.github.io/statim/reference/via.md) or
[`through()`](https://joshuamarie.github.io/statim/reference/through.md)
before being executed with
[`conclude()`](https://joshuamarie.github.io/statim/reference/conclude.md).

## Usage

``` r
prepare_test(.x, .test, ...)

# S3 method for class 'def_model'
prepare_test(.x, .test, ...)
```

## Arguments

- .x:

  A `def_model` object from
  [`define_model()`](https://joshuamarie.github.io/statim/reference/model-define-base.md).

- .test:

  A test function such as
  [TTEST](https://joshuamarie.github.io/statim/reference/TTEST.md), or a
  `test_spec` object returned by calling such a function with no
  arguments.

- ...:

  Additional arguments passed to methods.

## Value

A `test_lazy` S3 object.

## See also

[`define_model()`](https://joshuamarie.github.io/statim/reference/model-define-base.md),
[`via()`](https://joshuamarie.github.io/statim/reference/via.md),
[`through()`](https://joshuamarie.github.io/statim/reference/through.md),
[`conclude()`](https://joshuamarie.github.io/statim/reference/conclude.md)

## Examples

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
#> 
#> 
```
