#' Extract metadata from a model ID
#'
#' `model_id_info()` extracts a consistent metadata structure from a model ID
#' object. When `processed` is supplied, variable previews and count-based
#' metadata are included in the result.
#'
#' @param .model_id A model ID object from [x_by()], [rel()], [pairwise()],
#'   or a formula.
#' @param processed A named list returned by [model_processor()], or `NULL`.
#'   When `NULL`, count-based fields in `other_info` and `vars` are omitted.
#' @param ... Currently unused.
#'
#' @return A `class_model_inform` S7 object with fields:
#' \describe{
#'   \item{`model_id`}{The original model ID object.}
#'   \item{`model_type`}{Derived from the class name of `model_id`.}
#'   \item{`args`}{A formatted string summarising the model's arguments.
#'     Defaults to `"<?>"` for unregistered subclasses.}
#'   \item{`other_info`}{A named list of model-type-specific metadata.
#'     Empty for unregistered subclasses.}
#'   \item{`vars`}{A list of lists with `name` and `preview` fields.
#'     Empty for unregistered subclasses or when `processed` is `NULL`.}
#' }
#'
#' @examples
#' # without processed — no vars, no counts
#' model_id_info(x_by(extra, group))
#'
#' # with processed — includes vars and counts
#' dm = define_model(x_by(extra, group), sleep)
#' model_id_info(dm@model_id, dm@processed)
#'
#' @export
model_id_info = S7::new_generic(
    "model_id_info",
    ".model_id",
    function(.model_id, processed = NULL, ...) S7::S7_dispatch()
)

#' Output class for model ID metadata
#'
#' `class_model_inform` is the S7 output class returned by [model_id_info()].
#' `model_type` is derived automatically from the stored `model_id` object.
#' All other properties default to empty / unknown values, which are filled in
#' by registered [model_id_info()] methods for known subclasses.
#'
#' @format NULL
#' @usage NULL
#'
#' @export
class_model_inform = S7::new_class(
    name = "class_model_inform",
    properties = list(
        model_id = S7::new_property(class = model_id),
        model_type = S7::new_property(
            class = S7::class_character,
            getter = function(self) S7::S7_class(self@model_id)@name
        ),
        args = S7::new_property(class = S7::class_character, default = "<?>"),
        other_info = S7::new_property(class = S7::class_list, default = list()),
        vars = S7::new_property(class = S7::class_list, default = list())
    )
)

# Fallback: any unregistered model_id subclass.
# model_type is derived via the getter; args lists known property names
# as a breadcrumb for the developer.
S7::method(model_id_info, model_id) = function(.model_id, processed = NULL, ...) {
    prop_names = names(S7::S7_class(.model_id)@properties)
    args = if (length(prop_names)) paste(prop_names, collapse = ", ") else "<?>"

    class_model_inform(
        model_id = .model_id,
        args = args
    )
}

S7::method(model_id_info, x_by) = function(.model_id, processed = NULL, ...) {
    x_lbl = format_quo_label(.model_id@x)
    g_lbl = format_quo_label(.model_id@group)

    other_info = list()
    vars = list()

    if (!is.null(processed)) {
        other_info = list(
            x_vars = ncol(processed$x_data),
            by_vars = ncol(processed$group_data)
        )
        vars = vars_preview(
            c(as.list(processed$x_data), as.list(processed$group_data))
        )
    }

    class_model_inform(
        model_id = .model_id,
        args = paste0(x_lbl, " | ", g_lbl),
        other_info = other_info,
        vars = vars
    )
}

S7::method(model_id_info, rel) = function(.model_id, processed = NULL, ...) {
    x_lbl = format_quo_label(.model_id@x)
    r_lbl = format_quo_label(.model_id@resp)

    other_info = list()
    vars = list()

    if (!is.null(processed)) {
        other_info = list(
            x_vars = ncol(processed$x_data),
            resp_vars = ncol(processed$resp_data)
        )
        vars = vars_preview(
            c(as.list(processed$x_data), as.list(processed$resp_data))
        )
    }

    class_model_inform(
        model_id = .model_id,
        args = paste0(x_lbl, " ; ", r_lbl),
        other_info = other_info,
        vars = vars
    )
}

S7::method(model_id_info, pairwise) = function(.model_id, processed = NULL, ...) {
    lbls = vapply(.model_id@dots_quos, format_quo_label, character(1))

    other_info = list(direction = .model_id@direction)
    vars = list()

    if (!is.null(processed)) {
        other_info$n_pairs = length(processed$pairs)
        vars = vars_preview(as.list(processed$data))
    }

    class_model_inform(
        model_id = .model_id,
        args = paste(lbls, collapse = ", "),
        other_info = other_info,
        vars = vars
    )
}

S7::method(model_id_info, prop) = function(.model_id, processed = NULL, ...) {
    class_model_inform(
        model_id = .model_id,
        args = paste0(.model_id@x, " / ", .model_id@n),
        other_info = list(
            x = .model_id@x,
            n = .model_id@n
        ),
        vars = list(
            list(name = "x", preview = "<constant>"),
            list(name = "n", preview = "<constant>")
        )
    )
}

S7::method(model_id_info, S7::class_formula) = function(.model_id, processed = NULL, ...) {
    data = processed$data %||% NULL
    trms = stats::terms(.model_id, data = data)
    lhs_vars = all.vars(rlang::f_lhs(.model_id))
    rhs_vars = attr(trms, "term.labels")

    other_info = list(
        left_var = length(lhs_vars),
        right_var = length(rhs_vars)
    )
    vars = list()

    if (!is.null(processed)) {
        all_vars = c(lhs_vars, rhs_vars)
        avail = all_vars[all_vars %in% names(processed$data)]
        vars = vars_preview(as.list(processed$data[, avail, drop = FALSE]))
    }

    class_model_inform(
        model_id = .model_id,
        args = deparse(.model_id),
        other_info = other_info,
        vars = vars
    )
}

format_quo_label = function(quo) {
    cl = classify_quo(quo)
    switch(
        cl$type,
        ":symbol" = as.character(cl$expr),
        ":c_call" = paste(
            vapply(as.list(cl$expr[-1]), as.character, character(1)),
            collapse = ", "
        ),
        ":i_call" = "<inline>",
        ":inlines_call" = "<inlines>",
        ":tidyselect" = deparse(cl$expr),
        rlang::as_label(quo)
    )
}

vars_preview = function(cols) {
    lapply(seq_along(cols), function(i) {
        val = cols[[i]]
        list(
            name = names(cols)[[i]],
            preview = paste0("<", pillar::type_sum(val), " [", length(val), "]>")
        )
    })
}
