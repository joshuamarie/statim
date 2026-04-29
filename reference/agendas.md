# Collect implementations for a test definition

`agendas()` is the container for all implementations of a test. It
requires exactly one
[`baseline()`](https://joshuamarie.github.io/statim/reference/baseline.md)
as its first argument, and accepts any number of named
[`variant()`](https://joshuamarie.github.io/statim/reference/variant.md)
objects.

## Usage

``` r
agendas(base, ...)
```

## Arguments

- base:

  A
  [`baseline()`](https://joshuamarie.github.io/statim/reference/baseline.md)
  object. Required.

- ...:

  Named
  [`variant()`](https://joshuamarie.github.io/statim/reference/variant.md)
  objects.

## Value

An `agendas` S3 object.

## See also

[`baseline()`](https://joshuamarie.github.io/statim/reference/baseline.md),
[`variant()`](https://joshuamarie.github.io/statim/reference/variant.md),
[`test_define()`](https://joshuamarie.github.io/statim/reference/test_define.md)
