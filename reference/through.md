# Set the computational engine for a test pipeline

`through()` sets the engine without changing the method variant. The
engine is inherited by
[`via()`](https://joshuamarie.github.io/statim/reference/via.md) if
called afterwards.

## Usage

``` r
through(.x, engine, ...)

# S3 method for class 'test_lazy'
through(.x, engine, ...)
```

## Arguments

- .x:

  A `test_lazy` object.

- engine:

  A string naming the engine. E.g. `"cpp"`, `"rust"`.

- ...:

  Additional engine-level arguments.

## Value

A `test_lazy` object.

## See also

[`via()`](https://joshuamarie.github.io/statim/reference/via.md),
[`conclude()`](https://joshuamarie.github.io/statim/reference/conclude.md),
[`update()`](https://rdrr.io/r/stats/update.html)

## Examples

``` r
# The "cpp" engine is hypothetical; replace with a registered engine name.
if (FALSE) { # \dontrun{
sleep |>
    define_model(x_by(extra, group)) |>
    prepare_test(TTEST) |>
    through("cpp") |>
    conclude()

sleep |>
    define_model(x_by(extra, group)) |>
    prepare_test(TTEST) |>
    through("cpp") |>
    via("boot", n = 2000) |>
    conclude()
} # }
```
