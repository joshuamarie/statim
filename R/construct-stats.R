#' Main foundation for inferential statistics
#'
#' This function is a developer-interface function, a constructor for
#' user-facing test functions like [HTEST_FN()]. It returns a function with
#' a consistent signature that routes to the correct implementation based
#' on the model ID and method variant.
#'
#' @param cls A string naming the test class, e.g. `"ttest"`.
#' @param defs A list of `test_define` objects declaring the implementations.
#' @param .name A string used as the test title in output.
#' @param spec_class Base class of the type of statistical inference. Must be an S7.
#'
#' @return A function with signature `function(.model, .data, ...)`.
#'
#' @seealso [test_define()], [prepare_test()], [via()], [conclude()]
#'
#' @export
STAT_CONSTRUCTOR = function(cls, defs, .name, spec_class) {
    force(cls)
    force(defs)
    force(.name)
    force(spec_class)

    fn = function(.model = NULL, .data = NULL, ...) {
        # dots = list(...)
        build_stat(
            cls = cls,
            # args = dots,
            args = list(...),
            defs = defs,
            model_id = .model,
            .data = .data,
            .name = .name,
            spec_class = spec_class
        )
    }
    attr(fn, "cls") = cls
    fn
}

build_stat = function(defs, args, cls, model_id, .data, .name, spec_class) {
    if (!is.null(model_id)) {
        run_stat(defs, args, cls, model_id, .data, .name)
    } else {
        lookup = build_lookup(defs)
        defer_stat(lookup, args, cls, defs, .name, spec_class)
    }
}

run_stat = function(defs, args, cls, model_id, .data, .name) {
    lookup = build_lookup(defs)
    def = find_def(lookup, model_type = get_model_type(model_id))
    processed = model_processor(model_id, data = .data)

    out_raw = inject_and_run(
        impl = def@impl$base,
        processed = processed,
        args = args
    )

    stat_infer_spec(
        out_raw,
        impl_cls = def@impl_class,
        stat_cls = cls,
        print_fn = def@impl$base@print,
        name = .name
    )
}

defer_stat = function(lookup, args, cls, defs, .name, spec_class) {
    spec_class(
        defs = defs,
        args = args,
        cls = cls,
        name = .name,
        lookup = lookup
    )
}

model_type_name = function(model_type) {
    if (inherits(model_type, "S7_class")) return(model_type@name)
    if (inherits(model_type, "S7_S3_class")) return(model_type$class[[1]])
    cli::cli_abort("Cannot extract a name from {.arg model_type}.")
}

build_lookup = function(defs) {
    keys = vapply(defs, function(d) model_type_name(d@model_type), character(1))
    defs_rev = rev(defs)
    keys_rev = rev(keys)
    lookup = rlang::set_names(defs_rev, keys_rev)
    lookup[!duplicated(names(lookup))]
}

find_def = function(lookup, model_type) {
    lookup[[model_type]] %||% cli::cli_abort(
        "No implementation found for model type {.val {model_type}}."
    )
}

# new_stat_infer = function(res, impl_cls, stat_cls, print_fn, .name) {
#     stat_infer_spec(
#         data = res,
#         impl_cls = impl_cls,
#         stat_cls = stat_cls,
#         print_fn = print_fn,
#         name = .name
#     )
# }

stat_infer_spec = S7::new_class(
    "stat_infer_spec",
    properties = list(
        data = S7::new_property(class = S7::class_any),
        impl_cls = S7::new_property(class = S7::class_character),
        stat_cls = S7::new_property(class = S7::class_character),
        print_fn = S7::new_property(default = NULL),
        name = S7::new_property(class = S7::class_character, default = "")
    )
)

get_model_type = function(model_id) {
    cls = S7::S7_class(model_id)
    if (!is.null(cls)) cls@name else class(model_id)[[1]]
}

S7::method(print, stat_infer_spec) = function(x, ...) {
    print_fn = x@print_fn
    if (!is.null(print_fn)) {
        print_fn(x, ...)
    } else {
        print(x@data)
    }
    invisible(x)
}

#' @keywords internal
test_spec = S7::new_class(
    "test_spec",
    properties = list(
        defs = S7::new_property(class = S7::class_list),
        args = S7::new_property(class = S7::class_list),
        cls = S7::new_property(class = S7::class_character),
        name = S7::new_property(class = S7::class_character),
        lookup = S7::new_property(class = S7::class_list)
    )
)

#' @keywords internal
model_spec = S7::new_class(
    "model_spec",
    properties = list(
        defs = S7::new_property(class = S7::class_list),
        args = S7::new_property(class = S7::class_list),
        cls = S7::new_property(class = S7::class_character),
        name = S7::new_property(class = S7::class_character),
        lookup = S7::new_property(class = S7::class_list)
    )
)
