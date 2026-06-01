#' Base class for all statistical result objects
#'
#' `class_stat_infer` is the base S7 class for all result objects returned
#' by `fn` in [baseline()] and [variant()]. Concrete result classes like
#' [lm_object] inherit from it.
#'
#' Inheriting from `class_stat_infer` is the contract that enables automatic
#' dispatch for [auto_tidy()], and future `auto_plot()` and `auto_export()`
#' generics, without any manual registration via [making_tidy()].
#'
#' @section Protocol:
#' When `fn` in [baseline()] or [variant()] returns a `class_stat_infer`
#' subclass, the following generics dispatch automatically on `conclude()`:
#'
#' - [auto_tidy()], which is called by [tidy()]
#'
#' Register methods on your result class:
#'
#' ```r
#' example_out = S7::new_class("example_out", parent = class_stat_infer)
#'
#' S7::method(auto_tidy, example_out) = function(x, ...) {
#'     # return something
#' }
#' ```
#'
#' @section Variants:
#' A variant whose `fn` returns the same result class as `baseline` inherits
#' all `auto_*()` methods for free via S7's parent chain. A variant that
#' returns a subclass overrides only what it needs.
#'
#' Set `check_sic_s7 = TRUE` in [baseline()] or [variant()] to verify at
#' runtime that `fn` returns a `class_stat_infer` subclass.
#'
#' @seealso [baseline()], [variant()], [auto_tidy()], [lm_object]
#'
#' @export
class_stat_infer = S7::new_class("class_stat_infer")
