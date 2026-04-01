#' Eager execution of the tests
#'
#' @description
#' This eagerly executes the test
#'
#' @details
#' You are allowed to run an H-test function, e.g. through `TTEST(extra ~ group, sleep)`,
#' eagerly. Under the hood, `defs` contains the list of implementations, construct a dictionary of
#' implemented functions with `build_lookup()`, then match it with `find_def()`. The impl. being
#' look-up is saved as `def`, and it's `S7` class, not `S3` nor `S4`.
#'
#' Since this eagerly executes the test, it won't try to rely on `define_model()` to process
#' the `model_id` being defined. It has to be processed directly (thus, `processed = model_processor(model_id, data = .data)`
#' on the third line of code).
#'
#' Internally, the context of the test is then lookup by `infer_context` R6 class, because
#' this intends to pass on the arguments being used from the implementation. Then, use the
#' constructed context under `def@run()` to execute the test you want to perform.
#'
#' Save the raw output into a new class, under `new_htest()` typically.
#'
#' The `.name` will be the name of the test, and it's optional actually.
#'
#' @keywords internal
#' @rdname eager-exec-test
run_htest = function(defs, args, cls, model_id, .data, .name) {
    lookup = build_lookup(defs)
    def = find_def(
        lookup,
        model_type = class(model_id)[[1]],
        method_name = ""
    )
    processed = model_processor(model_id, data = .data)

    context = infer_context(
        processed = processed,
        args = args,
        extractors = def@vars,
        claims = NULL,
        fun_args = def@fun_args,
        method_args = list()
    )

    out_raw = def@run(context)
    out = new_htest(
        out_raw,
        impl_cls = def@impl_class,
        test_cls = cls,
        def = def
    )
    out$name = .name
    out
}

#' Deferred execution of the tests
#'
#' @description
#' This is looked up under the main pipeline.
#'
#' @keywords internal
#' @noRd
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

build_htest = function(defs, args, cls, model_id, .data = NULL, .name) {
    if (!is.null(model_id)) {
        run_htest(defs, args, cls, model_id, .data, .name)
    } else {
        lookup = build_lookup(defs)
        defer_htest(lookup, args, cls, defs, .name)
    }
}

#' H-test specs S3 class vector
#'
#' This wraps raw results of the tests
#'
#' @keywords internal
#' @noRd
new_htest = function(res, impl_cls, test_cls, def = NULL) {
    out = list(data = res)
    class(out) = c(impl_cls, test_cls, "htest_spec")
    attr(out, "print_fn") = if (!is.null(def)) def@print else NULL
    out
}

build_lookup = function(defs) {
    keys = vapply(defs, function(d) {
        method_name = if (is.null(d@method)) "" else d@method@method_name
        paste0(d@model_type, "::", method_name, "::", d@engine)
    }, character(1))
    rlang::set_names(defs, keys)
}

find_def = function(lookup, model_type, method_name = "", engine = "default") {
    key = paste0(model_type, "::", method_name, "::", engine)
    lookup[[key]] %||% cli::cli_abort(
        "No implementation found for {.val {key}}."
    )
}

#' @export
print.htest_spec = function(x, ...) {
    def_print = attr(x, "print_fn")
    if (!is.null(def_print)) {
        def_print(x, ...)
    } else {
        print(x$data)
    }
    invisible(x)
}

#' Build a hypothesis test function
#'
#' `HTEST_FN()` is a developer-interface function, a constructor for user-facing
#' test functions like [TTEST()]. It returns a function with a consistent
#' signature that routes to the correct implementation based on the model ID
#' and method variant.
#'
#' @param cls A string naming the test class, e.g. `"ttest"`.
#' @param defs A list of `test_define` objects declaring the implementations.
#' @param .name A string used as the test title in output.
#'
#' @return A function with signature
#'   `function(.model, .data, ..., .extra_defs)`.
#'
#' @seealso [test_define()], [prepare_test()], [via()], [conclude()]
#'
#' @examples
#' \dontrun{
#' MY_TEST = HTEST_FN(
#'     cls = "mytest",
#'     defs = list(my_def_two),
#'     .name = "My Test"
#' )
#' }
#'
#' @export
HTEST_FN = function(cls, defs, .name) {
    force(cls)
    force(defs)
    force(.name)

    function(.model = NULL, .data = NULL, ..., .extra_defs = list()) {
        build_htest(
            cls = cls,
            args = list(...),
            defs = c(defs, .extra_defs),
            model_id = .model,
            .data = .data,
            .name = .name
        )
    }
}

#' Declare arguments for a test implementation
#'
#' `fun_args()` declares the arguments accepted by a [test_define()] `run`
#' function, along with their default values. Pass the result to the
#' `fun_args` property of [test_define()].
#'
#' Arguments are declared in one of two ways:
#' - `name = value` — argument with a default value
#' - `~name` — required argument with no default
#'
#' Declared defaults are used by [ic_arg()] as fallbacks when the user
#' does not supply a value. Required arguments cause [ic_arg()] to error
#' if not supplied.
#'
#' @param ... Named arguments with defaults (`name = value`) or one-sided
#'   formulas for required arguments (`~name`).
#'
#' @return A `fun_args` object — a named list where each element carries
#'   `name`, `default`, and `required` fields.
#'
#' @seealso [test_define()], [ic_arg()]
#'
#' @examples
#' # all with defaults
#' fun_args(.paired = TRUE, .mu = 0, .alt = "two.sided", .ci = 0.95)
#'
#' # mixed — .ci has a default, .paired is required
#' fun_args(.ci = 0.95, ~.paired)
#'
#' # used inside test_define()
#' \dontrun{
#' new_def = test_define(
#'     model_type = "x_by",
#'     impl_class = "new_def_in_two",
#'     fun_args = fun_args(
#'         .paired = TRUE,
#'         .mu = 0,
#'         .alt = "two.sided",
#'         .ci = 0.95
#'     ),
#'     vars = list(
#'         x = function(p) p$x_data[[1]],
#'         group = function(p) p$group_data[[1]]
#'     ),
#'     run = function(self) {
#'         paired = ic_arg(self, ".paired")
#'         ci = ic_arg(self, ".ci")
#'     }
#' )
#' }
#'
#' @export
fun_args = function(...) {
    dots = rlang::enquos(...)

    out = lapply(seq_along(dots), function(i) {
        nm = names(dots)[[i]]
        q = dots[[i]]
        expr = rlang::quo_get_expr(q)

        if (rlang::is_formula(expr) && is.null(rlang::f_lhs(expr))) {
            list(
                name = rlang::as_label(rlang::f_rhs(expr)),
                default = NULL,
                required = TRUE
            )
        } else {
            list(
                name = nm,
                default = rlang::eval_tidy(q),
                required = FALSE
            )
        }
    })

    names(out) = vapply(out, `[[`, character(1), "name")
    class(out) = "fun_args"
    out
}
