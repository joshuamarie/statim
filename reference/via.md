# Recalibrate the test method variant

`via()` switches a lazy pipeline to an alternative method variant (e.g.
bootstrap, permutation) and merges user-supplied arguments with the
variant's declared defaults.

## Usage

``` r
via(.x, .method, ...)

# S3 method for class 'test_lazy'
via(.x, .method, ..., engine = NULL)

# S3 method for class 'engine_set'
via(.x, .method, ..., engine = NULL)
```

## Arguments

- .x:

  A `test_lazy` or `engine_set` object.

- .method:

  A string naming the method variant. Must match the `name` passed to
  [`method_spec()`](https://joshuamarie.github.io/statim/reference/method_spec.md)
  in one of the registered
  [`test_define()`](https://joshuamarie.github.io/statim/reference/test_define.md)
  objects. E.g. `"boot"`, `"permute"`.

- ...:

  Named arguments forwarded to the method (override defaults).

- engine:

  A string naming the engine to use. Defaults to the engine already set
  by
  [`through()`](https://joshuamarie.github.io/statim/reference/through.md),
  or `"default"` if none was set.

## Value

The modified `test_lazy` object with `recalibrate_spec` populated.

## See also

[`through()`](https://joshuamarie.github.io/statim/reference/through.md),
[`conclude()`](https://joshuamarie.github.io/statim/reference/conclude.md),
[`method_spec()`](https://joshuamarie.github.io/statim/reference/method_spec.md),
[`test_define()`](https://joshuamarie.github.io/statim/reference/test_define.md)

## Examples

``` r
sleep |>
    define_model(x_by(extra, group)) |>
    prepare_test(TTEST) |>
    via("boot", n = 2000) |>
    conclude()
#> Bootstrap CI : [-3.07, 0.02]
#> Replicates : 2000
```
