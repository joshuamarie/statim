# Execute a lazy test pipeline

`conclude()` is the terminal step of the pipeline. It resolves the
method variant and engine, builds the execution context, runs the
implementation, and returns an `htest_spec` object.

## Usage

``` r
conclude(.x, ...)

# S3 method for class 'test_lazy'
conclude(.x, ...)

# S3 method for class 'engine_set'
conclude(.x, ...)
```

## Arguments

- .x:

  A `test_lazy` or `engine_set` object produced by
  [`prepare_test()`](https://joshuamarie.github.io/statim/reference/prepare-test.md)
  (optionally followed by
  [`through()`](https://joshuamarie.github.io/statim/reference/through.md)
  and/or
  [`via()`](https://joshuamarie.github.io/statim/reference/via.md)).

- ...:

  Currently unused.

## Value

An `htest_spec` S3 object.

## Details

The engine and method variant are resolved in this order:

1.  If [`via()`](https://joshuamarie.github.io/statim/reference/via.md)
    was called, its `method_name` and `engine` win.

2.  If
    [`through()`](https://joshuamarie.github.io/statim/reference/through.md)
    was called (producing an `engine_set`), its engine is used with
    method `""` (the classical path).

3.  Otherwise `engine = "default"` and `method = ""` are used.

## See also

[`prepare_test()`](https://joshuamarie.github.io/statim/reference/prepare-test.md),
[`through()`](https://joshuamarie.github.io/statim/reference/through.md),
[`via()`](https://joshuamarie.github.io/statim/reference/via.md),
[`HTEST_FN()`](https://joshuamarie.github.io/statim/reference/HTEST_FN.md)

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
#> -------------------------------
#>   CI     :   [-3.2002, -0.05]
#>   n_reps :               2000
#> -------------------------------
#> 
#> 
```
