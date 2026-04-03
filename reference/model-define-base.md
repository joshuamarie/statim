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
#> $model_id
#> [[1]]
#> <quosure>
#> expr: ^extra
#> env:  0x55b17529d4a8
#> 
#> [[2]]
#> <quosure>
#> expr: ^group
#> env:  0x55b17529d4a8
#> 
#> attr(,"class")
#> [1] "x_by"     "model_id"
#> 
#> $processed
#> $processed$x_data
#>    extra
#> 1    0.7
#> 2   -1.6
#> 3   -0.2
#> 4   -1.2
#> 5   -0.1
#> 6    3.4
#> 7    3.7
#> 8    0.8
#> 9    0.0
#> 10   2.0
#> 11   1.9
#> 12   0.8
#> 13   1.1
#> 14   0.1
#> 15  -0.1
#> 16   4.4
#> 17   5.5
#> 18   1.6
#> 19   4.6
#> 20   3.4
#> 
#> $processed$group_data
#>    group
#> 1      1
#> 2      1
#> 3      1
#> 4      1
#> 5      1
#> 6      1
#> 7      1
#> 8      1
#> 9      1
#> 10     1
#> 11     2
#> 12     2
#> 13     2
#> 14     2
#> 15     2
#> 16     2
#> 17     2
#> 18     2
#> 19     2
#> 20     2
#> 
#> 
#> attr(,"class")
#> [1] "def_model"

# data-frame first (pipe-friendly)
sleep |> define_model(x_by(extra, group))
#> $model_id
#> [[1]]
#> <quosure>
#> expr: ^extra
#> env:  0x55b17529d4a8
#> 
#> [[2]]
#> <quosure>
#> expr: ^group
#> env:  0x55b17529d4a8
#> 
#> attr(,"class")
#> [1] "x_by"     "model_id"
#> 
#> $processed
#> $processed$x_data
#>    extra
#> 1    0.7
#> 2   -1.6
#> 3   -0.2
#> 4   -1.2
#> 5   -0.1
#> 6    3.4
#> 7    3.7
#> 8    0.8
#> 9    0.0
#> 10   2.0
#> 11   1.9
#> 12   0.8
#> 13   1.1
#> 14   0.1
#> 15  -0.1
#> 16   4.4
#> 17   5.5
#> 18   1.6
#> 19   4.6
#> 20   3.4
#> 
#> $processed$group_data
#>    group
#> 1      1
#> 2      1
#> 3      1
#> 4      1
#> 5      1
#> 6      1
#> 7      1
#> 8      1
#> 9      1
#> 10     1
#> 11     2
#> 12     2
#> 13     2
#> 14     2
#> 15     2
#> 16     2
#> 17     2
#> 18     2
#> 19     2
#> 20     2
#> 
#> 
#> attr(,"class")
#> [1] "def_model"
```
