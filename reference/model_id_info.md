# Extract metadata from a model ID

`model_id_info()` extracts a consistent metadata structure from a model
ID object. When `processed` is supplied, variable previews and
count-based metadata are included in the result.

## Usage

``` r
model_id_info(model_id, processed = NULL)

# S3 method for class 'x_by'
model_id_info(model_id, processed = NULL)

# S3 method for class 'rel'
model_id_info(model_id, processed = NULL)

# S3 method for class 'pairwise'
model_id_info(model_id, processed = NULL)

# S3 method for class 'formula'
model_id_info(model_id, processed = NULL)
```

## Arguments

- model_id:

  A model ID object from
  [`x_by()`](https://joshuamarie.github.io/statim/reference/x_by.md),
  [`rel()`](https://joshuamarie.github.io/statim/reference/rel.md),
  [`pairwise()`](https://joshuamarie.github.io/statim/reference/pairwise.md),
  or a formula.

- processed:

  A named list returned by
  [`model_processor()`](https://joshuamarie.github.io/statim/reference/model-processor.md),
  or `NULL`. When `NULL`, count-based fields in `other_info` and `vars`
  are omitted.

## Value

A list with fields:

- `model_type`:

  A string naming the primary model ID class.

- `args`:

  A formatted string summarising the model's arguments.

- `other_info`:

  A named list of model-type-specific metadata.

- `vars`:

  A list of lists with `name` and `preview` fields. Only present when
  `processed` is supplied.

## Examples

``` r
# without processed — no vars, no counts
model_id_info(x_by(extra, group))
#> $model_type
#> [1] "x_by"
#> 
#> $args
#> [1] "extra | group"
#> 
#> $other_info
#> list()
#> 

# with processed — includes vars and counts
dm = define_model(x_by(extra, group), sleep)
model_id_info(dm$model_id, dm$processed)
#> $model_type
#> [1] "x_by"
#> 
#> $args
#> [1] "extra | group"
#> 
#> $other_info
#> $other_info$x_vars
#> [1] 1
#> 
#> $other_info$by_vars
#> [1] 1
#> 
#> 
#> $vars
#> $vars[[1]]
#> $vars[[1]]$name
#> [1] "extra"
#> 
#> $vars[[1]]$preview
#> [1] "<dbl [20]>"
#> 
#> 
#> $vars[[2]]
#> $vars[[2]]$name
#> [1] "group"
#> 
#> $vars[[2]]$preview
#> [1] "<fct [20]>"
#> 
#> 
#> 
```
