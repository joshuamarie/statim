# Declare an alternative implementation of a test

`variant()` declares a named method variant reachable only via
[`via()`](https://joshuamarie.github.io/statim/reference/via.md). Never
runs on the eager path.

## Usage

``` r
variant(fn, print = NULL)
```

## Arguments

- fn:

  A function with named arguments. The framework injects data and
  arguments by matching formals to the processed model output.

- print:

  A function with signature `function(x, ...)` for formatting the
  result. `NULL` falls back to `print(x$data)`.

## Value

A `variant` S7 object.

## See also

[`baseline()`](https://joshuamarie.github.io/statim/reference/baseline.md),
[`agendas()`](https://joshuamarie.github.io/statim/reference/agendas.md),
[`via()`](https://joshuamarie.github.io/statim/reference/via.md)
