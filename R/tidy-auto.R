#' Automatically tidy a statistical result
#'
#' `auto_tidy()` is the protocol generic for tidying result objects produced
#' by `fn` in [baseline()] and [variant()]. It is called automatically by
#' [tidy()] when the result stored in `cld_exec@data` is a [class_stat_infer]
#' subclass.
#'
#' Register a method on your result class to participate in the protocol:
#'
#' ```r
#' example_out = S7::new_class("example_out", parent = class_stat_infer)
#'
#' S7::method(auto_tidy, example_out) = function(x, ...) {
#'     tibble::tibble(...)
#' }
#' ```
#'
#' When a variant's `fn` returns the same result class as `baseline`, it
#' inherits `auto_tidy()` automatically via S7's parent chain. When it
#' returns a subclass, it can override selectively:
#'
#' ```r
#' new_boot_class = S7::new_class("new_boot_class", parent = example_out)
#'
#' # override only for boot
#' # everything else inherited from `example_out`
#' S7::method(auto_tidy, new_boot_class) = function(x, ...) {
#'     tibble::tibble(...)
#' }
#' ```
#'
#' If no `auto_tidy()` method is found and no [making_tidy()] entry exists,
#' [tidy()] falls back to an informative error.
#'
#' @param x A [class_stat_infer] subclass object, typically `cld_exec@data`.
#' @param ... Currently unused. Passed to the dispatched method.
#'
#' @return A tibble.
#'
#' @seealso [tidy()], [making_tidy()], [method_tidy()], [class_stat_infer]
#'
#' @export
auto_tidy = S7::new_generic("auto_tidy", "x")

S7::method(auto_tidy, S7::class_any) = function(x, ...) {
    cli::cli_abort(c(
        "No {.fn auto_tidy} method for {.cls {class(x)[[1]]}}.",
        "i" = "Implement {.fn auto_tidy} on your result class, or register",
        "i" = "a tidy method via {.fn making_tidy}."
    ))
}

S7::method(auto_tidy, S7::new_union(lm_object, glm_object)) = function(x, ...) {
    tibble::tibble(x@coefficients)
}
