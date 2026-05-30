#' Recalibrate the method variant
#'
#' `via()` switches a lazy pipeline to an alternative method variant and
#' merges user-supplied arguments with the variant's declared defaults.
#' Works for both `test_lazy` and `model_lazy` pipelines.
#'
#' @param .x A `test_lazy` or `model_lazy` object.
#' @param .method A string naming the method variant. Must match a named
#'   [variant()] in the [agendas()] of the matched [stat_define()].
#'   E.g. `"boot"`, `"permute"`, `"permute_rfast"`.
#' @param ... Named arguments forwarded to the variant.
#'
#' @return The modified lazy object with `recalibrate_spec` populated.
#'
#' @seealso [conclude()], [stat_define()]
#'
#' @examples
#' sleep |>
#'     define_model(x_by(extra, group)) |>
#'     prepare_test(TTEST) |>
#'     via("boot", n = 2000) |>
#'     conclude()
#'
#' sleep |>
#'     define_model(x_by(extra, group)) |>
#'     prepare_test(TTEST) |>
#'     via("permute", n = 999L) |>
#'     conclude()
#'
#' @export
via = S7::new_generic("via", dispatch_args = c(".x", ".method"))

S7::method(via, list(test_lazy, S7::class_character)) = function(.x, .method, ...) {
    model_type = if (inherits(.x@model_id, "formula")) {
        "formula"
    } else {
        S7::S7_class(.x@model_id)@name
    }
    def = find_def(.x@test_spec@lookup, model_type = model_type)

    cls = .x@test_spec@cls
    key = variant_registry_key(cls, model_type)
    registry_names = names(variant_registry[[key]] %||% list())
    available = c(names(def@impl$variants), registry_names)

    if (.method %notin% available) {
        cli::cli_abort(c(
            "No variant {.val {(.method)}} registered for model type {.val {model_type}}.",
            "i" = "Available variant{?s}: {.val {available}}."
        ))
    }

    .x@recalibrate_spec = list(method_name = .method, args = list(...))
    .x
}

S7::method(via, list(model_lazy, S7::class_character)) = function(.x, .method, ...) {
    model_type = if (inherits(.x@model_id, "formula")) {
        "formula"
    } else {
        S7::S7_class(.x@model_id)@name
    }
    def = find_def(.x@model_spec@lookup, model_type = model_type)

    available = names(def@impl$variants)

    if (.method %notin% available) {
        cli::cli_abort(c(
            "No variant {.val {(.method)}} registered for model type {.val {model_type}}.",
            "i" = "Available variant{?s}: {.val {available}}."
        ))
    }

    .x@recalibrate_spec = list(method_name = .method, args = list(...))
    .x
}
