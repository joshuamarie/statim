# Inline multiple expressions in a model ID

`inlines()` is the multi-expression analogue of
[`c()`](https://rdrr.io/r/base/c.html) for inline data. Where
`c(x1, x2)` selects multiple variables or columns by name from a data
frame or the calling environment, `inlines()` accepts raw expressions —
vectors, function calls, or any R expression — and evaluates them
immediately at model definition time.

## Usage

``` r
inlines(...)
```

## Arguments

- ...:

  Named or unnamed expressions. If named, the supplied name becomes the
  variable name in the processed output. Unnamed elements are auto-named
  by their role and position: `xv1`, `xv2`, ... under role `"x"`;
  `grpv1`, `grpv2`, ... under role `"group"`; `pv1`, `pv2`, ... inside
  [`pairwise()`](https://joshuamarie.github.io/statim/reference/pairwise.md).
  Names can be mixed freely — unnamed elements take an auto-name based
  on their position regardless of whether other elements are named.

## Value

A named list of quosures. Intended for use inside model ID functions
such as
[`x_by()`](https://joshuamarie.github.io/statim/reference/x_by.md) and
[`rel()`](https://joshuamarie.github.io/statim/reference/rel.md); not
typically called on its own.

## Details

Use `inlines()` when your data does not live in a data frame. For a
single inline expression, use [`I()`](https://rdrr.io/r/base/AsIs.html)
instead. Take note that `inlines()` does not return an evaluated value,
only a naked and unevaluated expression.

## See also

[`I()`](https://rdrr.io/r/base/AsIs.html) for a single inline
expression,
[`x_by()`](https://joshuamarie.github.io/statim/reference/x_by.md),
[`rel()`](https://joshuamarie.github.io/statim/reference/rel.md),
[`pairwise()`](https://joshuamarie.github.io/statim/reference/pairwise.md)

## Examples

``` r
# Named inline expressions — names appear in output
x_by(
    inlines(x1 = rnorm(30), x2 = rnorm(30)),
    I(grp = rep(c("a", "b"), each = 15))
)
#> -- Model Definition ------------------------------------------------------------ 
#> 
#> Model ID : x_by 
#> Args : <inlines> | <inline> 

# Unnamed — auto-named as xv1, xv2 under role "x"
x_by(
    inlines(rnorm(30), rnorm(30)),
    I(rep(c("a", "b"), each = 15))
)
#> -- Model Definition ------------------------------------------------------------ 
#> 
#> Model ID : x_by 
#> Args : <inlines> | <inline> 

# Mixed — named elements keep their name, unnamed get auto-names
x_by(
    inlines(x1 = rnorm(30), rnorm(30)),
    I(rep(c("a", "b"), each = 15))
)
#> -- Model Definition ------------------------------------------------------------ 
#> 
#> Model ID : x_by 
#> Args : <inlines> | <inline> 

# Contrast with c() — selects existing variables by name
x1 = rnorm(30)
x2 = rnorm(30)
grp = rep(c("a", "b"), each = 15)
x_by(c(x1, x2), grp)
#> -- Model Definition ------------------------------------------------------------ 
#> 
#> Model ID : x_by 
#> Args : x1, x2 | grp 
```
