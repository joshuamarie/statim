# 'Variable compared by groups' model mapping

Use this when you want to compare `x` by `group`.

## Usage

``` r
x_by(x, group)
```

## Arguments

- x:

  The response variable. A bare name,
  [`c()`](https://rdrr.io/r/base/c.html) of bare names, a tidyselect
  helper (requires `data`), or `I(expr)` for inline data.

- group:

  The grouping variable. Same rules as `x`.

## Value

An `x_by` / `model_id` S3 object.

## Examples

``` r
# bare names (resolved from environment or data)
x_by(extra, group)
#> -- Model Definition ------------------------------------------------------------ 
#> 
#> Model ID : x_by 
#> Args : extra | group 

# inline data
x_by(I(rnorm(30)), I(rep(c("a", "b"), each = 15)))
#> -- Model Definition ------------------------------------------------------------ 
#> 
#> Model ID : x_by 
#> Args : <inline> | <inline> 

# named inline
x_by(I(score = rnorm(30)), I(grp = rep(c("a", "b"), each = 15)))
#> -- Model Definition ------------------------------------------------------------ 
#> 
#> Model ID : x_by 
#> Args : <inline> | <inline> 
```
