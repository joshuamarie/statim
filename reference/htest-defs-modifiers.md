# Add or replace variants on a test function

A family of functions for managing globally registered
[`variant()`](https://joshuamarie.github.io/statim/reference/variant.md)
objects across all
[HTEST_FN](https://joshuamarie.github.io/statim/reference/HTEST_FN.md)-based
functions for the duration of the session.

## Usage

``` r
plug_variant(test, name, impl, origin = c("user", "package"))

swap_variant(test, name, impl, origin = c("user", "package"))

clear_htest_defs(cls = NULL)
```

## Arguments

- test:

  A test function built with
  [`HTEST_FN()`](https://joshuamarie.github.io/statim/reference/HTEST_FN.md),
  e.g. [TTEST](https://joshuamarie.github.io/statim/reference/TTEST.md).

- name:

  A string naming the variant to add or replace.

- impl:

  A
  [`variant()`](https://joshuamarie.github.io/statim/reference/variant.md)
  object.

- origin:

  One of `"user"` (default, session-scoped) or `"package"` (permanent,
  intended for `.onLoad()`).

- cls:

  A string naming the test class to clear (e.g. `"ttest"`). When `NULL`
  (default), all globally registered variants are cleared.

## Value

`plug_variant()`, `swap_variant()`, and `clear_htest_defs()` return
`NULL` invisibly, called for their side effects. `get_htest_defs()`
returns a list of
[`test_define()`](https://joshuamarie.github.io/statim/reference/test_define.md)
objects, or an empty list if none have been registered for the given
`cls`.

## Functions

- `plug_variant()`: Adds a new named
  [`variant()`](https://joshuamarie.github.io/statim/reference/variant.md)
  to an existing test function. Hard-errors if the name already exists
  or if name is `"default"`.

- `swap_variant()`: Replaces an existing named
  [`variant()`](https://joshuamarie.github.io/statim/reference/variant.md).
  Hard-errors if the name does not exist or if name is `"default"`.

- `clear_htest_defs()`: Resets globally registered variants, either
  fully or scoped to a specific `cls`. Only `"user"`-originated entries
  are removed — `"package"` entries are always preserved.

## Precedence

Globally registered variants sit between built-in definitions and the
pipeline. The full priority order, from lowest to highest, is:

1.  Built-in variants (declared inside
    [HTEST_FN](https://joshuamarie.github.io/statim/reference/HTEST_FN.md))

2.  Global variants registered via `plug_variant()` or `swap_variant()`

## See also

[`HTEST_FN()`](https://joshuamarie.github.io/statim/reference/HTEST_FN.md),
[`test_define()`](https://joshuamarie.github.io/statim/reference/test_define.md),
[`variant()`](https://joshuamarie.github.io/statim/reference/variant.md)
