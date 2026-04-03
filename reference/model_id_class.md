# Attach a model-ID class to an object

Low-level constructor used by
[`x_by()`](https://joshuamarie.github.io/statim/reference/x_by.md),
[`rel()`](https://joshuamarie.github.io/statim/reference/rel.md), and
[`pairwise()`](https://joshuamarie.github.io/statim/reference/pairwise.md).
Extension authors can use this to register custom model-ID types.

## Usage

``` r
model_id_class(obj, clss)
```

## Arguments

- obj:

  A list representing the model ID payload.

- clss:

  A string giving the primary class name (e.g. `"x_by"`).

## Value

`obj` with `class` set to `c(clss, "model_id")`.
