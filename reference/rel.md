# 'Relationship between two variables' model mapping

Use this when you want to define the relationship between two variables.

## Usage

``` r
rel(x, resp)
```

## Arguments

- x:

  The predictor variable (bare name).

- resp:

  The response variable (bare name).

## Value

A `rel` / `model_id` S3 object.

## Examples

``` r
rel(speed, dist)
#> $x
#> <quosure>
#> expr: ^speed
#> env:  0x55b177cc5b48
#> 
#> $resp
#> <quosure>
#> expr: ^dist
#> env:  0x55b177cc5b48
#> 
#> attr(,"class")
#> [1] "rel"      "model_id"
```
