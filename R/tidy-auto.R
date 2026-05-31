#' @export
auto_tidy = S7::new_generic("auto_tidy", "x")

S7::method(auto_tidy, S7::class_any) = function(x, impl_cls = NULL, ...) {
    cli::cli_abort(
        "No {.fn auto_tidy} method for {.cls {class(x)[[1]]}}."
    )
}

S7::method(auto_tidy, S7::new_union(lm_object, glm_object)) = function(x, ...) {
    tibble::tibble(x@coefficients)
}
