# Package index

## High Level API

Main functions for interactive use

### Model definition

Verbs that describe the model you want to analyze

- [`define_model()`](https://joshuamarie.github.io/statim/reference/model-define-base.md)
  : Model define constructor

### Model IDs

Mappers to shape the model you want to describe. Formula also allowed.

- [`x_by()`](https://joshuamarie.github.io/statim/reference/x_by.md)
  [`` `%by%` ``](https://joshuamarie.github.io/statim/reference/x_by.md)
  : 'Variable compared by groups' model mapping
- [`rel()`](https://joshuamarie.github.io/statim/reference/rel.md) :
  'Relationship between two variables' model mapping
- [`pairwise()`](https://joshuamarie.github.io/statim/reference/pairwise.md)
  : 'Pairs between variables' model mapping

### Multiple Inline Codes

Analogue to [`I()`](https://rdrr.io/r/base/AsIs.html), but only captures
the expression and accepts multiple inline codes.

- [`inlines()`](https://joshuamarie.github.io/statim/reference/inlines.md)
  : Inline multiple expressions in a model ID

### H-test pipeline

Verbs that build and execute the test pipeline

- [`prepare_test()`](https://joshuamarie.github.io/statim/reference/prepare-test.md)
  : Lazily prepare a single test
- [`via()`](https://joshuamarie.github.io/statim/reference/via.md) :
  Recalibrate the test method variant
- [`update(`*`<test_lazy>`*`)`](https://joshuamarie.github.io/statim/reference/update.test_lazy.md)
  : Recalibrate arguments from the main pipeline
- [`conclude()`](https://joshuamarie.github.io/statim/reference/conclude.md)
  : Execute a lazy test pipeline

### H-test executioner

Eager execution of a prepared test

- [`TTEST()`](https://joshuamarie.github.io/statim/reference/TTEST.md) :
  T-Test
- [`CORTEST()`](https://joshuamarie.github.io/statim/reference/CORTEST.md)
  : Correlation Test

## Low-level/Implementation API

Functions for building and extending test implementations

### Model ID helpers

Build new model ID mappers

- [`model_id_class()`](https://joshuamarie.github.io/statim/reference/model_id_class.md)
  : Attach a model-ID class to an object
- [`model_processor()`](https://joshuamarie.github.io/statim/reference/model-processor.md)
  : Model evaluator
- [`model_id_info()`](https://joshuamarie.github.io/statim/reference/model_id_info.md)
  : Extract metadata from a model ID

### Implementation containers

Declare how a test runs

- [`agendas()`](https://joshuamarie.github.io/statim/reference/agendas.md)
  : Collect implementations for a test definition
- [`baseline()`](https://joshuamarie.github.io/statim/reference/baseline.md)
  : Declare the canonical implementation of a test
- [`variant()`](https://joshuamarie.github.io/statim/reference/variant.md)
  : Declare an alternative implementation of a test

### Test definition

Register a new test implementation

- [`test_define()`](https://joshuamarie.github.io/statim/reference/test_define.md)
  : Define a test implementation
- [`HTEST_FN()`](https://joshuamarie.github.io/statim/reference/HTEST_FN.md)
  : Build a hypothesis test function

### Session-scoped variant management

Add or replace variants on existing test functions

- [`plug_variant()`](https://joshuamarie.github.io/statim/reference/htest-defs-modifiers.md)
  [`swap_variant()`](https://joshuamarie.github.io/statim/reference/htest-defs-modifiers.md)
  [`clear_htest_defs()`](https://joshuamarie.github.io/statim/reference/htest-defs-modifiers.md)
  : Add or replace variants on a test function
