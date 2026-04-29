#' Build a hypothesis test function
#'
#' `HTEST_FN()` is a developer-interface function, a constructor for
#' user-facing test functions like [TTEST()]. It returns a function with
#' a consistent signature that routes to the correct implementation based
#' on the model ID and method variant.
#'
#' @param cls A string naming the test class, e.g. `"ttest"`.
#' @param defs A list of `test_define` objects declaring the implementations.
#' @param .name A string used as the test title in output.
#'
#' @return A function with signature `function(.model, .data, ...)`.
#'
#' @seealso [test_define()], [prepare_test()], [via()], [conclude()]
#'
#' @export
HTEST_FN = function(cls, defs, .name) {
    force(cls)
    force(defs)
    force(.name)

    fn = function(.model = NULL, .data = NULL, ...) {
        # all_defs = c(defs, get_htest_defs(cls))
        build_htest(
            cls = cls,
            args = list(...),
            # defs = all_defs,
            defs = defs,
            model_id = .model,
            .data = .data,
            .name = .name
        )
    }
    attr(fn, "cls") = cls
    fn
}

build_htest = function(defs, args, cls, model_id, .data = NULL, .name) {
    if (!is.null(model_id)) {
        run_htest(defs, args, cls, model_id, .data, .name)
    } else {
        lookup = build_lookup(defs)
        defer_htest(lookup, args, cls, defs, .name)
    }
}

run_htest = function(defs, args, cls, model_id, .data, .name) {
    lookup = build_lookup(defs)
    def = find_def(lookup, model_type = class(model_id)[[1]])
    processed = model_processor(model_id, data = .data)

    out_raw = inject_and_run(
        impl = def@impl$base,
        processed = processed,
        args = args
    )

    out = new_htest(
        out_raw,
        impl_cls = def@impl_class,
        test_cls = cls,
        print_fn = def@impl$base@print
    )
    out$name = .name
    out
}

defer_htest = function(lookup, args, cls, defs, .name) {
    out = list(
        defs = defs,
        args = args,
        cls = cls,
        name = .name,
        lookup = lookup
    )
    class(out) = "test_spec"
    out
}

build_lookup = function(defs) {
    keys = vapply(defs, function(d) d@model_type, character(1))
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

new_htest = function(res, impl_cls, test_cls, print_fn = NULL) {
    out = list(data = res)
    class(out) = c(impl_cls, test_cls, "htest_spec")
    attr(out, "print_fn") = print_fn
    out
}

#' @export
print.htest_spec = function(x, ...) {
    print_fn = attr(x, "print_fn")
    if (!is.null(print_fn)) {
        print_fn(x, ...)
    } else {
        print(x$data)
    }
    invisible(x)
}
