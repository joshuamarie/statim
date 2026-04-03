# Define a test implementation

`test_define()` declares a single implementation of a hypothesis test.
Multiple `test_define` objects are passed to
[`HTEST_FN()`](https://joshuamarie.github.io/statim/reference/HTEST_FN.md)
via `default_defs`. This is the main extension point for adding new
tests or engines.

## Usage

``` r
test_define(
  model_type = character(0),
  impl_class = character(0),
  engine = "default",
  method = NULL,
  fun_args = NULL,
  compatible_params = character(0),
  vars = list(),
  run = function() NULL,
  eval_claim = NULL,
  print = NULL
)
```

## Arguments

- model_type:

  A string matching the primary class of the model ID this
  implementation handles. E.g. `"x_by"`, `"pairwise"`.

- impl_class:

  A string naming the implementation class. E.g. `"ttest_two"`. Used in
  the S3 class vector of the result.

- engine:

  A string naming the engine. Defaults to `"default"`. Use a different
  string to register an alternative engine, e.g. `"cpp"`.

- method:

  A `method_spec` object declaring the method variant and its default
  arguments. `NULL` for classical implementations.

- fun_args:

  A `fun_args` object from
  [`fun_args()`](https://joshuamarie.github.io/statim/reference/fun_args.md)
  declaring the default values and required status of each test
  argument. `NULL` if the implementation does not use
  [`ic_arg()`](https://joshuamarie.github.io/statim/reference/infer-context-accessors.md).

- compatible_params:

  A character vector of `ParamDef` types this implementation can
  interpret via `write_claim()`. E.g. `c("MU")`.

- vars:

  A named list of extractor functions. Each function takes `processed`
  and returns the variable for that role. E.g.:
  `list(x = function(p) p$x_data[[1]])`.

- run:

  A function with signature `function(self)` where `self` is an
  `infer_context` object. Contains the full implementation logic. Use
  [`ic_pull()`](https://joshuamarie.github.io/statim/reference/infer-context-accessors.md),
  [`ic_arg()`](https://joshuamarie.github.io/statim/reference/infer-context-accessors.md),
  [`ic_method_arg()`](https://joshuamarie.github.io/statim/reference/infer-context-accessors.md)
  to access data and args.

- eval_claim:

  A function with signature `function(self, claim)` that interprets a
  `ClaimDef` for this implementation. `NULL` if `write_claim()` is not
  supported.

- print:

  A function with signature `function(x, ...)` that formats the result
  for printing. `NULL` falls back to `print(x$data)`.

## Value

A `test_define` S7 object.

## See also

[`HTEST_FN()`](https://joshuamarie.github.io/statim/reference/HTEST_FN.md),
[`method_spec()`](https://joshuamarie.github.io/statim/reference/method_spec.md),
[`via()`](https://joshuamarie.github.io/statim/reference/via.md),
[`conclude()`](https://joshuamarie.github.io/statim/reference/conclude.md)

## Examples

``` r
if (FALSE) { # \dontrun{
new_htest_fn = test_define(
    model_type = "x_by",
    impl_class = "mytest_two",
    engine = "default",
    vars = list(
        x = function(p) p$x_data[[1]],
        group = function(p) p$group_data[[1]]
    ),
    run = function(self) {
        grp = as.character(ic_pull(self, "group"))
        resp = ic_pull(self, "x")
        # implementation logic
    },
    print = function(x, ...) {
        print(x$data)
        invisible(x)
    }
)
} # }
```
