# Execution context for test implementations

`infer_context()` constructs the execution context passed as `self` into
the `run` function of a
[`test_define()`](https://joshuamarie.github.io/statim/reference/test_define.md)
object. It is built once inside
[`conclude()`](https://joshuamarie.github.io/statim/reference/conclude.md)
or the eager path of
[`HTEST_FN()`](https://joshuamarie.github.io/statim/reference/HTEST_FN.md)
and never exposed to the end user.

## Usage

``` r
infer_context(
  processed,
  args = list(),
  extractors = list(),
  fun_args = NULL,
  claims = NULL,
  method_args = NULL
)
```

## Arguments

- processed:

  A named list returned by
  [`model_processor()`](https://joshuamarie.github.io/statim/reference/model-processor.md).
  Contains the subsetted data and resolved variable names for the model
  ID.

- args:

  A named list of test arguments supplied by the user via `...` in the
  test function, or via
  [`update()`](https://rdrr.io/r/stats/update.html).

- extractors:

  A named list of extractor functions declared in
  [`test_define()`](https://joshuamarie.github.io/statim/reference/test_define.md)
  via the `vars` property. Each function takes `processed` and returns
  the variable for that role.

- fun_args:

  A `fun_args` object from
  [`fun_args()`](https://joshuamarie.github.io/statim/reference/fun_args.md)
  declaring the default values and required status of each test
  argument. `NULL` if the implementation does not declare `fun_args`.

- claims:

  A named list of resolved `ClaimDef` objects from `write_claim()`.
  `NULL` if no claims were declared.

- method_args:

  A named list of method-level arguments supplied via
  [`via()`](https://joshuamarie.github.io/statim/reference/via.md) or
  [`update()`](https://rdrr.io/r/stats/update.html). E.g. `n`, `seed`,
  `engine`.

## Value

An `infer_context` S3 object.

## Details

Extension authors interact with the context exclusively through the
accessor functions
[`ic_pull()`](https://joshuamarie.github.io/statim/reference/infer-context-accessors.md),
[`ic_arg()`](https://joshuamarie.github.io/statim/reference/infer-context-accessors.md),
[`ic_method_arg()`](https://joshuamarie.github.io/statim/reference/infer-context-accessors.md),
[`ic_name()`](https://joshuamarie.github.io/statim/reference/infer-context-accessors.md),
and
[`ic_claim()`](https://joshuamarie.github.io/statim/reference/infer-context-accessors.md).
Direct access to the list fields is not part of the public API and may
change without notice.

## See also

[`ic_pull()`](https://joshuamarie.github.io/statim/reference/infer-context-accessors.md),
[`ic_arg()`](https://joshuamarie.github.io/statim/reference/infer-context-accessors.md),
[`ic_method_arg()`](https://joshuamarie.github.io/statim/reference/infer-context-accessors.md),
[`test_define()`](https://joshuamarie.github.io/statim/reference/test_define.md),
[`conclude()`](https://joshuamarie.github.io/statim/reference/conclude.md)
