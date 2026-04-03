# Eager execution of the tests

This eagerly executes the test

## Usage

``` r
run_htest(defs, args, cls, model_id, .data, .name)
```

## Details

You are allowed to run an H-test function, e.g. through
`TTEST(extra ~ group, sleep)`, eagerly. Under the hood, `defs` contains
the list of implementations, construct a dictionary of implemented
functions with `build_lookup()`, then match it with `find_def()`. The
impl. being look-up is saved as `def`, and it's `S7` class, not `S3` nor
`S4`.

Since this eagerly executes the test, it won't try to rely on
[`define_model()`](https://joshuamarie.github.io/statim/reference/model-define-base.md)
to process the `model_id` being defined. It has to be processed directly
(thus, `processed = model_processor(model_id, data = .data)` on the
third line of code).

Internally, the context of the test is then lookup by `infer_context` R6
class, because this intends to pass on the arguments being used from the
implementation. Then, use the constructed context under `def@run()` to
execute the test you want to perform.

Save the raw output into a new class, under `new_htest()` typically.

The `.name` will be the name of the test, and it's optional actually.
