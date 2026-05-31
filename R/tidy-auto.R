#' Automatically tidy a statistical result
#'
#' A generic fallback tidier for S7 result objects produced by `{statim}`
#' pipelines. Called automatically by [tidy()] when no method has been
#' registered via [making_tidy()]. Can also be called directly.
#'
#' Dispatch is based on the class of `x`, typically the object stored in
#' `cld_exec@data`. When multiple stat functions share the same result class
#' (e.g. both `LINEAR_REG` and a variant producing an [lm_object]), they
#' share the same `auto_tidy()` output without needing separate registrations.
#'
#' @param x A statistical result object, such as [lm_object] or [glm_object].
#' @param ... Currently unused. Passed to the dispatched method.
#'
#' @return A tibble of coefficients or other primary results, depending on
#'   the class of `x`.
#'
#' @seealso [tidy()], [making_tidy()], [method_tidy()]
#'
#' @examples
#' fit = cars |>
#'     define_model(dist ~ speed) |>
#'     prepare_model(LINEAR_REG) |>
#'     conclude()
#'
#' # called directly
#' auto_tidy(fit@data)
#'
#' # called implicitly via tidy() when no method is registered
#' tidy(fit)
#'
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
