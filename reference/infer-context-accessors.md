# Access data and arguments inside a test implementation

These functions provide access to variables, arguments, and claims
inside the `run` function of a
[`test_define()`](https://joshuamarie.github.io/statim/reference/test_define.md)
object. The `self` parameter in `run` is an `infer_context` object —
pass it as `x` to these functions.

## Usage

``` r
ic_pull(x, role)

ic_name(x, role)

ic_arg(x, name, default = NULL)

ic_method_arg(x, name, default = NULL)

ic_claim(x, name)
```

## Arguments

- x:

  An `infer_context` object passed as `self` inside `run`.

- role:

  A string naming the variable role declared in `vars`. E.g. `"x"`,
  `"group"`.

- name:

  A string naming the argument or claim.

- default:

  A fallback value if the argument was not supplied.

## Value

- `ic_pull()` — a vector

- `ic_name()` — a string

- `ic_arg()` — the argument value or `default`

- `ic_method_arg()` — the method argument value or `default`

- `ic_claim()` — a `ClaimDef` object or `NULL`

## See also

[`test_define()`](https://joshuamarie.github.io/statim/reference/test_define.md),
[`method_spec()`](https://joshuamarie.github.io/statim/reference/method_spec.md),
[`via()`](https://joshuamarie.github.io/statim/reference/via.md),
[`conclude()`](https://joshuamarie.github.io/statim/reference/conclude.md)

## Examples

``` r
if (FALSE) { # \dontrun{
test_new_def = test_define(
    model_type = "x_by",
    impl_class = "test_new_def_in_two",
    vars = list(
        x = function(p) p$x_data[[1]],
        group = function(p) p$group_data[[1]]
    ),
    run = function(self) {
        grp = as.character(ic_pull(self, "group"))
        resp = ic_pull(self, "x")
        n = ic_method_arg(self, "n")
        ci = ic_arg(self, ".ci", 0.95)
    }
)
} # }
```
