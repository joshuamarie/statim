# Declare the canonical implementation of a test

`baseline()` declares the default implementation of a test, which is the
only implementation reachable on the eager path. It is frozen — no user
or package can swap it out via
[`swap_variant()`](https://joshuamarie.github.io/statim/reference/htest-defs-modifiers.md).

## Usage

``` r
baseline(fn, print = NULL)
```

## Arguments

- fn:

  A function with named arguments. The framework injects data and
  arguments by matching formals to the processed model output.

- print:

  A function with signature `function(x, ...)` for formatting the
  result. `NULL` falls back to `print(x$data)`.

## Value

A `baseline` S7 object.

## See also

[`variant()`](https://joshuamarie.github.io/statim/reference/variant.md),
[`agendas()`](https://joshuamarie.github.io/statim/reference/agendas.md),
[`test_define()`](https://joshuamarie.github.io/statim/reference/test_define.md)
