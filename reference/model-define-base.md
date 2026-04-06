# Model define constructor

`define_model()` captures a model ID and optional data into a
`def_model` object that can be passed into
[`prepare_test()`](https://joshuamarie.github.io/statim/reference/prepare-test.md).

## Usage

``` r
define_model(.x, ...)

# S3 method for class 'model_id'
define_model(.x, data = parent.frame(), ...)

# S3 method for class 'data.frame'
define_model(.x, to_analyze, ...)
```

## Arguments

- .x:

  A model ID object from
  [`x_by()`](https://joshuamarie.github.io/statim/reference/x_by.md),
  [`rel()`](https://joshuamarie.github.io/statim/reference/rel.md),
  [`pairwise()`](https://joshuamarie.github.io/statim/reference/pairwise.md),
  or a formula — **or** a data frame when using the data-first pipe
  style.

- ...:

  Currently unused.

- data:

  A data frame. When called on a model-ID object this defaults to
  [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html), resolving
  bare variable names against the calling environment. When calling on a
  data frame, pass the model ID as `to_analyze`.

- to_analyze:

  A model ID or formula (only used in the `define_model.data.frame`
  method).

## Value

A `def_model` S3 object containing `model_id` and `processed`.

## Examples

``` r
# model-ID first
define_model(x_by(extra, group), sleep)
#> 
#> -- Model Definition ------------------------------------------------------------ 
#> 
#> Model ID : x_by 
#> Args : extra | group 
#> Other info:
#>     x_vars : 1 
#>     by_vars : 1 
#> Variables :
#>     extra : <dbl [20]> 
#>     group : <fct [20]> 
#> 

# data-frame first (pipe-friendly)
sleep |> define_model(x_by(extra, group))
#> 
#> -- Model Definition ------------------------------------------------------------ 
#> 
#> Model ID : x_by 
#> Args : extra | group 
#> Other info:
#>     x_vars : 1 
#>     by_vars : 1 
#> Variables :
#>     extra : <dbl [20]> 
#>     group : <fct [20]> 
#> 
```
