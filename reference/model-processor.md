# Model evaluator

A function for development use to extract the information in model IDs.

## Usage

``` r
model_processor(x, ...)

# S3 method for class 'formula'
model_processor(x, data = NULL, ...)

# S3 method for class 'x_by'
model_processor(x, data = NULL, ...)

# S3 method for class 'rel'
model_processor(x, data = NULL, ...)

# S3 method for class 'pairwise'
model_processor(x, data = NULL, ...)
```

## Arguments

- x:

  The model IDs to be extracted.

- ...:

  Currently unused; passed through for S3 method compatibility.

- data:

  The given data frame when supplied. It can be a `NULL` or a missing
  argument
