#' Extract metadata from a model ID
#'
#' `model_id_info()` extracts a consistent metadata structure from a model ID
#' object. When `processed` is supplied, variable previews and count-based
#' metadata are included in the result.
#'
#' @param model_id A model ID object from [x_by()], [rel()], [pairwise()], or
#'   a formula.
#' @param processed A named list returned by [model_processor()], or `NULL`.
#'   When `NULL`, count-based fields in `other_info` and `vars` are omitted.
#'
#' @return A list with fields:
#' \describe{
#'   \item{`model_type`}{A string naming the primary model ID class.}
#'   \item{`args`}{A formatted string summarising the model's arguments.}
#'   \item{`other_info`}{A named list of model-type-specific metadata.}
#'   \item{`vars`}{A list of lists with `name` and `preview` fields.
#'     Only present when `processed` is supplied.}
#' }
#'
#' @examples
#' # without processed — no vars, no counts
#' model_id_info(x_by(extra, group))
#'
#' # with processed — includes vars and counts
#' dm = define_model(x_by(extra, group), sleep)
#' model_id_info(dm$model_id, dm$processed)
#'
#' @export
model_id_info = function(model_id, processed = NULL) {
    UseMethod("model_id_info")
}

#' @rdname model_id_info
#' @export
model_id_info.x_by = function(model_id, processed = NULL) {
    quos = unclass(model_id)
    x_lbl = format_quo_label(quos$x)
    g_lbl = format_quo_label(quos$group)

    out = list(
        model_type = "x_by",
        args = paste0(x_lbl, " | ", g_lbl),
        other_info = list()
    )

    if (!is.null(processed)) {
        out$other_info = list(
            x_vars = ncol(processed$x_data),
            by_vars = ncol(processed$group_data)
        )
        out$vars = vars_preview(
            c(as.list(processed$x_data), as.list(processed$group_data))
        )
    }

    out
}

#' @rdname model_id_info
#' @export
model_id_info.rel = function(model_id, processed = NULL) {
    quos = unclass(model_id)
    x_lbl = format_quo_label(quos$x)
    r_lbl = format_quo_label(quos$resp)

    out = list(
        model_type = "rel",
        args = paste0(x_lbl, " ; ", r_lbl),
        other_info = list()
    )

    if (!is.null(processed)) {
        out$other_info = list(
            x_vars = ncol(processed$x_data),
            resp_vars = ncol(processed$resp_data)
        )
        out$vars = vars_preview(
            c(as.list(processed$x_data), as.list(processed$resp_data))
        )
    }

    out
}

#' @rdname model_id_info
#' @export
model_id_info.pairwise = function(model_id, processed = NULL) {
    dots_quos = model_id$args$dots_quos
    lbls = vapply(dots_quos, format_quo_label, character(1))

    out = list(
        model_type = "pairwise",
        args = paste(lbls, collapse = ", "),
        other_info = list(
            direction = model_id$direction
        )
    )

    if (!is.null(processed)) {
        out$other_info$n_pairs = length(processed$pairs)
        out$vars = vars_preview(as.list(processed$data))
    }

    out
}

#' @rdname model_id_info
#' @export
model_id_info.formula = function(model_id, processed = NULL) {
    f = model_id$formula
    trms = terms(f)
    lhs_vars = all.vars(rlang::f_lhs(f))
    rhs_vars = attr(trms, "term.labels")

    out = list(
        model_type = "formula",
        args = deparse(f),
        other_info = list(
            left_var = length(lhs_vars),
            right_var = length(rhs_vars)
        )
    )

    if (!is.null(processed)) {
        out$vars = vars_preview(
            as.list(processed$data[, processed$vars, drop = FALSE])
        )
    }

    out
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
