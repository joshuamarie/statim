# H-test definition modifiers

A family of functions for managing globally registered
[test_define](https://joshuamarie.github.io/statim/reference/test_define.md)
objects across all
[HTEST_FN](https://joshuamarie.github.io/statim/reference/HTEST_FN.md)-based
functions (e.g.
[TTEST](https://joshuamarie.github.io/statim/reference/TTEST.md)) for
the duration of the session.

## Usage

``` r
add_htest_defs(defs, origin = c("user", "package"))

get_htest_defs(cls = NULL)

clear_htest_defs(cls = NULL)
```

## Arguments

- defs:

  A
  [test_define](https://joshuamarie.github.io/statim/reference/test_define.md)
  object or a list of
  [test_define](https://joshuamarie.github.io/statim/reference/test_define.md)
  objects to be referenced globally. Each element must be a valid S7
  `test_define` instance — passing anything else raises an error.

- origin:

  Must be one of `"user"` (default) or `"package"`. Controls the origin
  tag attached to each registered def. Use `"package"` inside
  `.onLoad()` hook to protect defs from being wiped by
  `clear_htest_defs()`, which only removes `"user"`-originated defs by
  default. Passing `"package"` in interactive code is discouraged.

- cls:

  A string naming the test class to clear (e.g. `"ttest"`). When `NULL`
  (default), all globally registered definitions are cleared.

## Value

- `add_htest_defs()` and `clear_htest_defs()` return `NULL` invisibly,
  called for their side effects on `htest_opts_global$defs`.

- `get_htest_defs()` returns a list of
  [test_define](https://joshuamarie.github.io/statim/reference/test_define.md)
  objects, or an empty list if none have been registered for the given
  `cls`.

## Functions

- `add_htest_defs()`: Registers one or more
  [test_define](https://joshuamarie.github.io/statim/reference/test_define.md)
  objects into the global H-test store. The `cls` key is derived
  automatically from each def's `impl_class` prefix (e.g.
  `"ttest_permute_rfast"` routes into `"ttest"`), scoping it to the
  correct
  [HTEST_FN](https://joshuamarie.github.io/statim/reference/HTEST_FN.md)-based
  function.

- `get_htest_defs()`: Returns the list of
  [test_define](https://joshuamarie.github.io/statim/reference/test_define.md)
  objects currently registered under the given `cls` key. Primarily used
  internally by
  [`HTEST_FN()`](https://joshuamarie.github.io/statim/reference/HTEST_FN.md)
  but exported for inspection and testing.

- `clear_htest_defs()`: Resets the global H-test store, either fully or
  scoped to a specific `cls`. Only `"user"`-originated defs are removed
  — defs registered with `"package"` origin via `.onLoad()` are always
  preserved. Subsequent calls to
  [HTEST_FN](https://joshuamarie.github.io/statim/reference/HTEST_FN.md)-based
  functions will fall back to built-in and package-registered
  definitions only.

## Precedence

Globally registered defs sit between built-in definitions and per-call
overrides. The full priority order, from lowest to highest, is:

1.  Built-in defs (declared inside
    [HTEST_FN](https://joshuamarie.github.io/statim/reference/HTEST_FN.md))

2.  Global defs registered via `add_htest_defs()`

3.  Per-call defs passed via `.extra_defs`

When two defs share the same key (`model_type::method::engine`), the
higher-priority entry wins at lookup time.

## See also

[`HTEST_FN()`](https://joshuamarie.github.io/statim/reference/HTEST_FN.md),
[`test_define()`](https://joshuamarie.github.io/statim/reference/test_define.md)

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
my_def = test_define(
    model_type = "x_by",
    engine = "custom",
    # ...
)

add_htest_defs(my_def)

# my_def is now available in a current environment
# no `.extra_defs` needed
TTEST(x_by(extra, group), sleep)

# Inspect what is registered under "ttest"
get_htest_defs("ttest")

# Clear only ttest-scoped defs
clear_htest_defs("ttest")

# Clear everything
clear_htest_defs()
} # }
# }
```
