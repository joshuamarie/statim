# Declare arguments for a test implementation

`fun_args()` declares the arguments accepted by a
[`test_define()`](https://joshuamarie.github.io/statim/reference/test_define.md)
`run` function, along with their default values. Pass the result to the
`fun_args` property of
[`test_define()`](https://joshuamarie.github.io/statim/reference/test_define.md).

## Usage

``` r
fun_args(...)
```

## Arguments

- ...:

  Named arguments with defaults (`name = value`) or one-sided formulas
  for required arguments (`~name`).

## Value

A `fun_args` object — a named list where each element carries `name`,
`default`, and `required` fields.

## Details

Arguments are declared in one of two ways:

- `name = value` — argument with a default value

- `~name` — required argument with no default

Declared defaults are used by
[`ic_arg()`](https://joshuamarie.github.io/statim/reference/infer-context-accessors.md)
as fallbacks when the user does not supply a value. Required arguments
cause
[`ic_arg()`](https://joshuamarie.github.io/statim/reference/infer-context-accessors.md)
to error if not supplied.

## See also

[`test_define()`](https://joshuamarie.github.io/statim/reference/test_define.md),
[`ic_arg()`](https://joshuamarie.github.io/statim/reference/infer-context-accessors.md)

## Examples

``` r
# all with defaults
fun_args(.paired = TRUE, .mu = 0, .alt = "two.sided", .ci = 0.95)
#> $.paired
#> $.paired$name
#> [1] ".paired"
#> 
#> $.paired$default
#> [1] TRUE
#> 
#> $.paired$required
#> [1] FALSE
#> 
#> 
#> $.mu
#> $.mu$name
#> [1] ".mu"
#> 
#> $.mu$default
#> [1] 0
#> 
#> $.mu$required
#> [1] FALSE
#> 
#> 
#> $.alt
#> $.alt$name
#> [1] ".alt"
#> 
#> $.alt$default
#> [1] "two.sided"
#> 
#> $.alt$required
#> [1] FALSE
#> 
#> 
#> $.ci
#> $.ci$name
#> [1] ".ci"
#> 
#> $.ci$default
#> [1] 0.95
#> 
#> $.ci$required
#> [1] FALSE
#> 
#> 
#> attr(,"class")
#> [1] "fun_args"

# mixed — .ci has a default, .paired is required
fun_args(.ci = 0.95, ~.paired)
#> $.ci
#> $.ci$name
#> [1] ".ci"
#> 
#> $.ci$default
#> [1] 0.95
#> 
#> $.ci$required
#> [1] FALSE
#> 
#> 
#> $.paired
#> $.paired$name
#> [1] ".paired"
#> 
#> $.paired$default
#> NULL
#> 
#> $.paired$required
#> [1] TRUE
#> 
#> 
#> attr(,"class")
#> [1] "fun_args"

# used inside test_define()
if (FALSE) { # \dontrun{
new_def = test_define(
    model_type = "x_by",
    impl_class = "new_def_in_two",
    fun_args = fun_args(
        .paired = TRUE,
        .mu = 0,
        .alt = "two.sided",
        .ci = 0.95
    ),
    vars = list(
        x = function(p) p$x_data[[1]],
        group = function(p) p$group_data[[1]]
    ),
    run = function(self) {
        paired = ic_arg(self, ".paired")
        ci = ic_arg(self, ".ci")
    }
)
} # }
```
