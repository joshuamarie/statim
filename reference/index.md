# Package index

## High Level API

Main functions for interactive uses

### Model definition construction verb

A verb that describes the model you want to analyze

- [`define_model()`](https://joshuamarie.github.io/statim/reference/model-define-base.md)
  : Model define constructor

### Model IDs

Basically mappers to shape the model you want to describe. Formula also
allowed.

- [`x_by()`](https://joshuamarie.github.io/statim/reference/x_by.md) :
  'Variable compared by groups' model mapping
- [`rel()`](https://joshuamarie.github.io/statim/reference/rel.md) :
  'Relationship between two variables' model mapping
- [`pairwise()`](https://joshuamarie.github.io/statim/reference/pairwise.md)
  : 'Pairs between variables' model mapping

### Multiple Inline Codes

Analogue to [`I()`](https://rdrr.io/r/base/AsIs.html), but only captures
the expression, not evaluated, and accepts multiple inline codes.

- [`inlines()`](https://joshuamarie.github.io/statim/reference/inlines.md)
  : Inline multiple expressions in a model ID

### H-test configuration

Verbs that define the test you want to execute

- [`via()`](https://joshuamarie.github.io/statim/reference/via.md) :
  Recalibrate the test method variant
- [`through()`](https://joshuamarie.github.io/statim/reference/through.md)
  : Set the computational engine for a test pipeline
- [`update(`*`<test_lazy>`*`)`](https://joshuamarie.github.io/statim/reference/update.test_lazy.md)
  : Recalibrate arguments from the main pipeline

### H-test executioner

Execute the prepared test pipeline

- [`conclude()`](https://joshuamarie.github.io/statim/reference/conclude.md)
  : Execute a lazy test pipeline

## Grammatical verbs

Functions that signifies “grammar of statistical inference”

### H-test pipeline

Functions to express the grammar for H-test pipeline

- [`prepare_test()`](https://joshuamarie.github.io/statim/reference/prepare-test.md)
  : Lazily prepare a single test

## Low Level API

Main functions to interact with ‘statim’ API

### Model ID helpers

You can make new mappers with these functions

- [`model_id_class()`](https://joshuamarie.github.io/statim/reference/model_id_class.md)
  : Attach a model-ID class to an object
- [`model_processor()`](https://joshuamarie.github.io/statim/reference/model-processor.md)
  : Model evaluator
- [`model_id_info()`](https://joshuamarie.github.io/statim/reference/model_id_info.md)
  : Extract metadata from a model ID

### Inference context interactors

Functions to interact objects from the pipeline

- [`ic_pull()`](https://joshuamarie.github.io/statim/reference/infer-context-accessors.md)
  [`ic_name()`](https://joshuamarie.github.io/statim/reference/infer-context-accessors.md)
  [`ic_arg()`](https://joshuamarie.github.io/statim/reference/infer-context-accessors.md)
  [`ic_method_arg()`](https://joshuamarie.github.io/statim/reference/infer-context-accessors.md)
  [`ic_claim()`](https://joshuamarie.github.io/statim/reference/infer-context-accessors.md)
  : Access data and arguments inside a test implementation

### Parameter definers

Functions to define function for inference’s parameters

- [`fun_args()`](https://joshuamarie.github.io/statim/reference/fun_args.md)
  : Declare arguments for a test implementation
- [`method_spec()`](https://joshuamarie.github.io/statim/reference/method_spec.md)
  : Declare a method variant for a test implementation

### H-test storage

Acts like a storage room to store the metadata of H-test definitions

- [`test_define()`](https://joshuamarie.github.io/statim/reference/test_define.md)
  : Define a test implementation
- [`HTEST_FN()`](https://joshuamarie.github.io/statim/reference/HTEST_FN.md)
  : Build a hypothesis test function

## Battery-ready H-test functions

Functions used for Hypothesis testing

- [`TTEST()`](https://joshuamarie.github.io/statim/reference/TTEST.md) :
  T-Test
- [`CORTEST()`](https://joshuamarie.github.io/statim/reference/CORTEST.md)
  : Correlation test
