# Define a test implementation

`test_define()` declares a single implementation of a hypothesis test.
Multiple `test_define` objects are passed to
[`HTEST_FN()`](https://joshuamarie.github.io/statim/reference/HTEST_FN.md)
via `defs`. This is the main extension point for adding new tests or
engines.

## Usage

``` r
test_define(
  model_type = character(0),
  impl_class = character(0),
  impl = NULL,
  compatible_params = character(0),
  eval_claim = NULL
)
```

## Arguments

- model_type:

  A string matching the primary class of the model ID this
  implementation handles. E.g. `"x_by"`, `"pairwise"`.

- impl_class:

  A string naming the implementation class. E.g. `"ttest_two"`. Used in
  the S3 class vector of the result.

- impl:

  An
  [`agendas()`](https://joshuamarie.github.io/statim/reference/agendas.md)
  object declaring all implementations.

- compatible_params:

  A character vector of `ParamDef` types this implementation can
  interpret via `write_claim()`. E.g. `c("MU")`.

- eval_claim:

  A function with signature `function(self, claim)` that interprets a
  `ClaimDef` for this implementation. `NULL` if `write_claim()` is not
  supported.

## Value

A `test_define` S7 object.

## See also

[`agendas()`](https://joshuamarie.github.io/statim/reference/agendas.md),
[`baseline()`](https://joshuamarie.github.io/statim/reference/baseline.md),
[`variant()`](https://joshuamarie.github.io/statim/reference/variant.md),
[`HTEST_FN()`](https://joshuamarie.github.io/statim/reference/HTEST_FN.md)
