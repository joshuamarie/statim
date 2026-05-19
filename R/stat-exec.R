#' Execute a lazy pipeline
#'
#' `conclude()` is the terminal step of the pipeline. It resolves the
#' method variant, runs the implementation, and returns a `cld_exec` object.
#'
#' @param .x A `test_lazy` or `model_lazy` object produced by
#'   [prepare_test()] or [prepare_model()] (optionally followed by [via()]).
#' @param ... Currently unused.
#'
#' @return A `cld_exec` S3 object.
#'
#' @seealso [prepare_test()], [prepare_model()], [via()]
#'
#' @examples
#' sleep |>
#'     define_model(x_by(extra, group)) |>
#'     prepare_test(TTEST) |>
#'     conclude()
#'
#' sleep |>
#'     define_model(x_by(extra, group)) |>
#'     prepare_test(TTEST) |>
#'     via("boot", n = 2000) |>
#'     conclude()
#'
#' mtcars |>
#'     define_model(rel(mpg, wt)) |>
#'     prepare_model(LINEAR_REG) |>
#'     conclude()
#'
#' @name conclude
#' @export
conclude = S7::new_generic("conclude", ".x")
# conclude = function(.x, ...) {
#     UseMethod("conclude")
# }

S7::method(conclude, test_lazy) = function(.x, ...) {
    model_type = if (inherits(.x@model_id, "formula")) {
        "formula"
    } else {
        S7::S7_class(.x@model_id)@name
    }
    def = find_def(.x@test_spec@lookup, model_type = model_type)

    method_name = .x@recalibrate_spec$method_name

    impl = resolve_impl(
        method_name = method_name,
        def = def,
        model_type = model_type,
        cls = .x@test_spec@cls,
        global_variants = htest_opts_global$variants
    )

    all_args = utils::modifyList(
        .x@test_spec@args,
        .x@recalibrate_spec$args %||% list()
    )

    out_raw = inject_and_run(
        impl = impl,
        processed = .x@processed,
        args = all_args,
        claims = .x@claims
    )

    wrap_exec(
        out_raw,
        def = def,
        impl = impl,
        stat_cls = .x@test_spec@cls,
        stat_name = .x@test_spec@name,
        method_name = method_name,
        model_id = .x@model_id,
        processed = .x@processed,
        data_name = .x@data_name %||% ""
    )
}

S7::method(conclude, model_lazy) = function(.x, ...) {
    model_type = S7::S7_class(.x@model_id)@name
    def = find_def(.x@model_spec@lookup, model_type = model_type)

    method_name = .x@recalibrate_spec$method_name

    impl = resolve_impl(
        method_name = method_name,
        def = def,
        model_type = model_type,
        cls = .x@model_spec@cls,
        global_variants = list()
    )

    all_args = utils::modifyList(
        .x@model_spec@args,
        .x@recalibrate_spec$args %||% list()
    )

    out_raw = inject_and_run(
        impl = impl,
        processed = .x@processed,
        args = all_args
    )

    wrap_exec(
        out_raw,
        def = def,
        impl = impl,
        stat_cls = .x@model_spec@cls,
        stat_name = .x@model_spec@name,
        method_name = method_name,
        model_id = .x@model_id,
        processed = .x@processed,
        data_name = .x@data_name %||% ""
    )
}

resolve_impl = function(method_name, def, model_type, cls, global_variants) {
    if (is.null(method_name)) return(def@impl$base)

    global_entries = global_variants[[cls]] %||% list()
    global_match = Filter(function(e) identical(e$name, method_name), global_entries)

    def@impl$variants[[method_name]] %||%
        global_match[[1]]$impl %||%
        cli::cli_abort(c(
            "No variant {.val {method_name}} registered for model type {.val {model_type}}.",
            "i" = "Available variant{?s}: {.val {names(def@impl$variants)}}."
        ))
}

wrap_exec = function(
    out_raw, def, impl, stat_cls, stat_name,
    method_name, model_id, processed, data_name
) {
    cld_exec(
        data = out_raw,
        impl_cls = def@impl_class,
        stat_cls = stat_cls,
        print_fn = impl@print,
        name = stat_name,
        cld_meta = list(
            model_id = model_id,
            processed = processed,
            stat_name = stat_name,
            method = method_name %||% "default",
            data_name = data_name %||% ""
        )
    )
}

cld_exec = S7::new_class(
    "cld_exec",
    parent = stat_infer_spec,
    properties = list(
        cld_meta = S7::new_property(class = S7::class_list)
    )
)

S7::method(print, cld_exec) = function(x, ...) {
    meta = x@cld_meta
    info = model_id_info(meta$model_id, meta$processed)

    cat("\n")
    cat(cli::rule(left = "Model", line = "="), "\n\n")
    cat("Model ID :", info$model_type, "\n")
    cat("Args :", info$args, "\n")
    if (length(info$other_info) > 0L) {
        for (nm in names(info$other_info)) {
            cat("   ", nm, ":", info$other_info[[nm]], "\n")
        }
    }
    if (nzchar(meta$data_name)) {
        cat("Data     :", meta$data_name, "\n")
    }

    stat_label = if (identical(meta$method, "default")) {
        meta$stat_name
    } else {
        paste0(meta$stat_name, " \u00b7 ", meta$method)
    }
    cat("\n")
    cat(cli::rule(left = stat_label, line = "="), "\n\n")

    print_fn = x@print_fn
    if (!is.null(print_fn)) {
        print_fn(x, ...)
    } else {
        print(x@data)
    }

    invisible(x)
}

# #' @export
# print.cld_exec = function(x, ...) {
#     meta = x$cld_meta
#     info = model_id_info(meta$model_id, meta$processed)
#
#     cat("\n")
#     cat(cli::rule(left = "Model", line = "="), "\n\n")
#     cat("Model ID :", info$model_type, "\n")
#     cat("Args     :", info$args, "\n")
#     if (length(info$other_info) > 0L) {
#         for (nm in names(info$other_info)) {
#             cat("   ", nm, ":", info$other_info[[nm]], "\n")
#         }
#     }
#     if (!is.null(meta$data_name) && nzchar(meta$data_name)) {
#         cat("Data     :", meta$data_name, "\n")
#     }
#
#     stat_label = if (identical(meta$method, "default")) {
#         meta$stat_name
#     } else {
#         paste0(meta$stat_name, " \u00b7 ", meta$method)
#     }
#     cat("\n")
#     cat(cli::rule(left = stat_label, line = "="), "\n\n")
#
#     print(x$result)
#
#     invisible(x)
# }
