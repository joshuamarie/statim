# 'Pairs between variables' model mapping

Use this when you want to define all pairwise combinations of a set of
variables.

## Usage

``` r
pairwise(..., direction = "lt")
```

## Arguments

- ...:

  Bare variable names to pair up.

- direction:

  A string controlling which pairs are kept. One of `"lt"` (default,
  strict lower-triangle), `"lteq"`, `"gt"`, `"gteq"`, `"eq"`, `"neq"`,
  or `"all"`.

## Value

A `pairwise` / `model_id` S3 object.

## Examples

``` r
pairwise(a, b, c)
#> $args
#> $args$dots
#> c(~a, ~b, ~c)
#> 
#> $args$dots_quos
#> <list_of<quosure>>
#> 
#> [[1]]
#> <quosure>
#> expr: ^a
#> env:  0x55b57cbe6c70
#> 
#> [[2]]
#> <quosure>
#> expr: ^b
#> env:  0x55b57cbe6c70
#> 
#> [[3]]
#> <quosure>
#> expr: ^c
#> env:  0x55b57cbe6c70
#> 
#> 
#> 
#> $direction
#> [1] "lt"
#> 
#> attr(,"class")
#> [1] "pairwise" "model_id"
pairwise(a, b, c, direction = "all")
#> $args
#> $args$dots
#> c(~a, ~b, ~c)
#> 
#> $args$dots_quos
#> <list_of<quosure>>
#> 
#> [[1]]
#> <quosure>
#> expr: ^a
#> env:  0x55b57cbe6c70
#> 
#> [[2]]
#> <quosure>
#> expr: ^b
#> env:  0x55b57cbe6c70
#> 
#> [[3]]
#> <quosure>
#> expr: ^c
#> env:  0x55b57cbe6c70
#> 
#> 
#> 
#> $direction
#> [1] "all"
#> 
#> attr(,"class")
#> [1] "pairwise" "model_id"
```
