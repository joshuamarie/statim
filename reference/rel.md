# 'Relationship between two variables' model mapping

Use this when you want to define the relationship between two variables.

## Usage

``` r
rel(x, resp)
```

## Arguments

- x:

  The predictor variable. A bare name,
  [`c()`](https://rdrr.io/r/base/c.html) of bare names, a tidyselect
  helper (requires `data`), or `I(expr)` for inline data.

- resp:

  The response variable. Same rules as `x`.

## Value

A `rel` / `model_id` S3 object.

## Examples

``` r
rel(speed, dist)
#> -- Model Definition ------------------------------------------------------------ 
#> 
#> Model ID : rel 
#> Args : speed ; dist 
```
