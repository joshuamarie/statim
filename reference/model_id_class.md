# Attach a model-ID class to an object

Attach a model-ID class to an object

## Usage

``` r
model_id_class(obj, clss)
```

## Arguments

- obj:

  A list representing the model ID payload.

- clss:

  A string giving the primary class name.

## Value

`obj` with `class` set to `c(clss, "model_id")`.
