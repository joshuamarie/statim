#' Base class for all statistical result objects
#'
#' `class_stat_infer` is the base abstract S7 class for all result objects returned
#' by `fn` in [baseline()] and [variant()]. Concrete result classes like
#' [class_lm_object] inherit from it.
#'
#' Inheriting from `class_stat_infer` is the contract that enables automatic
#' dispatch for [auto_tidy()], and future `auto_plot()` and `auto_export()`
#' generics, without any manual registration via [making_tidy()].
#'
#' @section Protocol:
#' Inheriting from `class_stat_infer` is the contract that enables automatic
#' dispatch for [auto_tidy()], and future `auto_plot()` and `auto_export()`
#' generics, without any manual registration via [making_tidy()].
#'
#' When `fn` in [baseline()] or [variant()] returns a `class_stat_infer`
#' subclass, [tidy()] calls [auto_tidy()] on it automatically. Register a
#' method on your result class to participate:
#'
#' ```r
#' example_out = S7::new_class("example_out", parent = class_stat_infer)
#'
#' S7::method(auto_tidy, example_out) = function(x, ...) {
#'     # return something
#' }
#' ```
#'
#' @section Variant inheritance:
#' A variant whose `fn` returns the same result class as `baseline` inherits
#' all `auto_*()` methods for free via S7's parent chain. A variant that
#' returns a subclass overrides only what it needs — everything else
#' inherits automatically:
#'
#' ```r
#' my_result_boot = S7::new_class("my_result_boot", parent = my_result)
#'
#' # only auto_tidy() differs
#' # all other auto_*() inherited from my_result
#' S7::method(auto_tidy, my_result_boot) = function(x, ...) {
#'     tibble::tibble(...)
#' }
#' ```
#'
#' @section Class hierarchy:
#' The built-in hierarchy is:
#'
#' ```
#' class_stat_infer
#'     ├── anova_able
#'     │       └── class_lm_object
#'     └── <your-own-output-class>
#'             └── <your-own-subclass>
#' ```
#'
#' Downstream packages can extend the hierarchy further by using any
#' `class_stat_infer` subclass as a `parent` in `S7::new_class()`.
#'
#' @seealso [baseline()], [variant()], [auto_tidy()], [class_lm_object]
#'
#' @export
class_stat_infer = S7::new_class("class_stat_infer", abstract = TRUE)
