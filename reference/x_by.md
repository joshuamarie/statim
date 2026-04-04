# 'Variable compared by groups' model mapping

Use this when you want to compare `x` by `group`.

## Usage

``` r
x_by(x, group)
```

## Arguments

- x:

  The response variable (bare name).

- group:

  The grouping variable (bare name).

## Value

An `x_by` / `model_id` S3 object.

## Examples

``` r
x_by(extra, group)
#> [[1]]
#> <quosure>
#> expr: ^extra
#> env:  0x55ee15d7fb50
#> 
#> [[2]]
#> <quosure>
#> expr: ^group
#> env:  0x55ee15d7fb50
#> 
#> attr(,"class")
#> [1] "x_by"     "model_id"
```
