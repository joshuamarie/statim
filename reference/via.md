# Recalibrate the test method variant

`via()` switches a lazy pipeline to an alternative method variant and
merges user-supplied arguments with the variant's declared defaults.

## Usage

``` r
via(.x, .method, ...)

# S3 method for class 'test_lazy'
via(.x, .method, ...)
```

## Arguments

- .x:

  A `test_lazy` object.

- .method:

  A string naming the method variant. Must match a named
  [`variant()`](https://joshuamarie.github.io/statim/reference/variant.md)
  in the
  [`agendas()`](https://joshuamarie.github.io/statim/reference/agendas.md)
  of the matched
  [`test_define()`](https://joshuamarie.github.io/statim/reference/test_define.md).
  E.g. `"boot"`, `"permute"`, `"permute_rfast"`.

- ...:

  Named arguments forwarded to the variant.

## Value

The modified `test_lazy` object with `recalibrate_spec` populated.

## See also

[`conclude()`](https://joshuamarie.github.io/statim/reference/conclude.md),
[`test_define()`](https://joshuamarie.github.io/statim/reference/test_define.md)

## Examples

``` r
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
#>   CI     :   [-3.11, -0.0798]
#>   n_reps :               2000
#> -------------------------------
#> 
#> 

sleep |>
    define_model(x_by(extra, group)) |>
    prepare_test(TTEST) |>
    via("permute", n = 999L) |>
    conclude()
#> ============================== T-test Permutation ==============================
#> 
#> 
#> -- Summary ---------------------------------------------------------------------
#> 
#> ───────────────────────────────
#>   Statistic  p-value  n_perms  
#> ───────────────────────────────
#>    -1.580     0.074     999    
#> ───────────────────────────────
#> 
#> 
```
