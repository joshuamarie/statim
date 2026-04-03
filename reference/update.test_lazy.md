# Recalibrate arguments from the main pipeline

This function is ideal to transmute and modify the parameters being used
in the test under the pipeline.

## Usage

``` r
# S3 method for class 'test_lazy'
update(object, ...)
```

## Examples

``` r
sleep |>
    define_model(extra ~ group) |>
    prepare_test(TTEST) |>
    update(.paired = TRUE) |>
    conclude()
#> Error in find_def(.x$test_spec$lookup, model_type = model_type, method_name = method_name,     engine = engine): No implementation found for "formula::::default".

```
