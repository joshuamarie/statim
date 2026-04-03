# Declare a method variant for a test implementation

`method_spec()` declares a named method variant and its default
arguments for use in
[`test_define()`](https://joshuamarie.github.io/statim/reference/test_define.md).
Pass the result to the `method` property.

## Usage

``` r
method_spec(name, method_type, defaults = list())
```

## Arguments

- name:

  A string naming the method. E.g. `"boot"`, `"permute"`. Must match the
  `.method` argument passed to
  [`via()`](https://joshuamarie.github.io/statim/reference/via.md).

- method_type:

  A string naming the method type. E.g. `"bootstrap"`, `"replicate"`,
  `"bayes"`.

- defaults:

  A named list of default arguments for this method. E.g.
  `list(n = 1000L, seed = NULL)`.

## Value

A `method_spec` S7 object.

## See also

[`test_define()`](https://joshuamarie.github.io/statim/reference/test_define.md),
[`via()`](https://joshuamarie.github.io/statim/reference/via.md)

## Examples

``` r
if (FALSE) { # \dontrun{
method_spec(
    "boot",
    method_type = "bootstrap",
    defaults = list(n = 1000L, seed = NULL)
)
} # }
```
