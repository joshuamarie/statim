# 'Pairs between variables' model mapping

Use this when you want to define all pairwise combinations of a set of
variables.

## Usage

``` r
pairwise(..., direction = "lt")
```

## Arguments

- ...:

  Bare variable names, tidyselect helpers (requires `data`), or
  `I(expr)` for inline data.

- direction:

  A string controlling which pairs are kept. One of `"lt"` (default),
  `"lteq"`, `"gt"`, `"gteq"`, `"eq"`, `"neq"`, or `"all"`.

## Value

A `pairwise` / `model_id` S3 object.

## Examples

``` r
pairwise(a, b, c)
#> -- Model Definition ------------------------------------------------------------ 
#> 
#> Model ID : pairwise 
#> Args : a, b, c 
pairwise(I(rnorm(30)), I(rnorm(30)), I(rnorm(30)))
#> -- Model Definition ------------------------------------------------------------ 
#> 
#> Model ID : pairwise 
#> Args : <inline>, <inline>, <inline> 
```
