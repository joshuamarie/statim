#' Add or remove variant implementations on a test or model function
#'
#' @description
#' These are **developer-interface** functions intended for package authors
#' extending the `statim` framework with new method variants.
#'
#' `add_variant()` is used as the left-hand side of the `%<-%` operator
#' to register a [variant()] for a stat function and model type.
#' `"default"` is frozen and cannot be added.
#'
#' `remove_variant()` removes a previously registered `"user"`-originated
#' variant. `"package"`-level entries are self-cleaning: they exist for the
#' duration the registering package is loaded and vanish when it is unloaded.
#'
#' @param obj A test or model function built with [HTEST_FN()] or [MODEL_FN()]
#'   (e.g. `TTEST`). Used to scope the registry key.
#' @param model_type An S7 model ID class (e.g. `x_by`, `S7::class_formula`).
#' @param name A string naming the variant to add.
#' @param origin One of `"user"` (default, session-scoped) or `"package"`
#'   (load-scoped, intended for `.onLoad()`).
#'
#' @return An `add_variant_call` object, consumed by `%<-%`.
#'
#' @seealso [stat_define()], [variant()], [agendas()], [model_processor()]
#'
#' @examples
#' # Add a bootstrap variant for x_by (user level).
#' # .proc$x_data[[1]] is the response vector; .proc$group_data is the
#' # grouping data frame. See ?model_processor for all available keys.
#' add_variant(TTEST, x_by, "another_boot") %<-% variant(
#'     fn = function(.proc, .n = 1000L) {
#'         x = .proc$x_data[[1]]
#'         grp = as.character(.proc$group_data[[1]])
#'         lvls = unique(grp)
#'         x1 = x[grp == lvls[[1]]]
#'         x2 = x[grp == lvls[[2]]]
#'         boot_fn = function(d, i) mean(d[i, 1]) - mean(d[i, 2])
#'         b = boot::boot(data.frame(x1, x2), boot_fn, R = .n)
#'         boot::boot.ci(b, type = "perc")
#'     }
#' )
#'
#' # Remove it, returning to the original slate
#' remove_variant(TTEST, x_by, "another_boot")
#'
#' # Package level (inside .onLoad())
#' add_variant(TTEST, x_by, "another_boot", origin = "package") %<-% variant(
#'     fn = function(.proc, .n = 1000L) { ... }
#' )
#'
#' @name add-variant
#' @export
add_variant = function(obj, model_type, name, origin = c("user", "package")) {
    structure(
        list(obj = obj, model_type = model_type, name = name, origin = match.arg(origin)),
        class = "add_variant_call"
    )
}

#' @keywords internal
add_variant_register = function(lhs, rhs) {
    obj = lhs$obj
    model_type = lhs$model_type
    name = lhs$name
    origin = lhs$origin

    stat_cls = attr(obj, "cls") %||% cli::cli_abort(
        "{.arg obj} must be a function built with {.fn HTEST_FN} or {.fn MODEL_FN}."
    )
    is_model_id_class = inherits(model_type, "S7_class") && identical(model_type@parent, model_id)
    is_formula_class = identical(model_type, S7::class_formula)
    if (!is_model_id_class && !is_formula_class) {
        cli::cli_abort(
            "{.arg model_type} must be a class inheriting from {.cls model_id}, or {.code S7::class_formula}."
        )
    }
    if (identical(name, "default")) {
        cli::cli_abort(
            "{.val default} is frozen and cannot be added via {.fn add_variant}."
        )
    }
    if (!S7::S7_inherits(rhs, variant)) {
        cli::cli_abort("{.arg value} must be a {.cls variant} object.")
    }

    key = variant_registry_key(stat_cls, model_type_name(model_type))
    variant_registry[[key]][[name]] = list(impl = rhs, origin = origin)
    invisible(NULL)
}

#' @rdname add-variant
#' @export
remove_variant = function(obj, model_type, name) {
    stat_cls = attr(obj, "cls") %||% cli::cli_abort(
        "{.arg obj} must be a function built with {.fn HTEST_FN} or {.fn MODEL_FN}."
    )
    if (!inherits(model_type, "S7_class")) {
        cli::cli_abort(
            "{.arg model_type} must be an S7 class (e.g. {.cls x_by}, {.code S7::class_formula})."
        )
    }
    if (identical(name, "default")) {
        cli::cli_abort(
            "{.val default} is frozen and cannot be removed via {.fn remove_variant}."
        )
    }

    key = variant_registry_key(stat_cls, model_type_name(model_type))
    entries = variant_registry[[key]] %||% list()

    if (is.null(entries[[name]])) {
        cli::cli_abort(c(
            "Variant {.val {name}} is not registered for {.cls {stat_cls}} / {.cls {model_type_name(model_type)}}.",
            "i" = "Registered variant{?s}: {.val {names(entries)}}."
        ))
    }
    if (!identical(entries[[name]]$origin, "user")) {
        cli::cli_abort(
            "Variant {.val {name}} is {.val package}-scoped and cannot be removed manually."
        )
    }

    variant_registry[[key]][[name]] = NULL
    invisible(NULL)
}

#' @keywords internal
#' @noRd
variant_registry_key = function(stat_cls, model_type) {
    paste0(stat_cls, "_", model_type)
}

#' @keywords internal
#' @noRd
variant_registry = new.env(parent = emptyenv())

resolve_impl = function(method_name, def, model_type, cls, global_variants) {
    if (is.null(method_name)) return(def@impl$base)

    key = variant_registry_key(cls, model_type)
    registered = variant_registry[[key]][[method_name]]$impl

    global_entries = global_variants[[cls]] %||% list()
    global_match = Filter(function(e) identical(e$name, method_name), global_entries)

    def@impl$variants[[method_name]] %||%
        registered %||%
        global_match[[1]]$impl %||%
        cli::cli_abort(c(
            "No variant {.val {method_name}} registered for model type {.val {model_type}}.",
            "i" = "Available variant{?s}: {.val {names(def@impl$variants)}}."
        ))
}
