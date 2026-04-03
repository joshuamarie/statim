# Build a hypothesis test function

`HTEST_FN()` is a developer-interface function, a constructor for
user-facing test functions like
[`TTEST()`](https://joshuamarie.github.io/statim/reference/TTEST.md). It
returns a function with a consistent signature that routes to the
correct implementation based on the model ID and method variant.

## Usage

``` r
HTEST_FN(cls, defs, .name)
```

## Arguments

- cls:

  A string naming the test class, e.g. `"ttest"`.

- defs:

  A list of `test_define` objects declaring the implementations.

- .name:

  A string used as the test title in output.

## Value

A function with signature `function(.model, .data, ..., .extra_defs)`.

## See also

[`test_define()`](https://joshuamarie.github.io/statim/reference/test_define.md),
[`prepare_test()`](https://joshuamarie.github.io/statim/reference/prepare-test.md),
[`via()`](https://joshuamarie.github.io/statim/reference/via.md),
[`conclude()`](https://joshuamarie.github.io/statim/reference/conclude.md)

## Examples

``` r
if (FALSE) { # \dontrun{
MY_TEST = HTEST_FN(
    cls = "mytest",
    defs = list(my_def_two),
    .name = "My Test"
)
} # }
```
