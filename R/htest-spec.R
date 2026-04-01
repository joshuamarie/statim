#' Define a test implementation
#'
#' `test_define()` declares a single implementation of a hypothesis test.
#' Multiple `test_define` objects are passed to [HTEST_FN()] via `default_defs`.
#' This is the main extension point for adding new tests or engines.
#'
#' @param model_type A string matching the primary class of the model ID
#'   this implementation handles. E.g. `"x_by"`, `"pairwise"`.
#' @param impl_class A string naming the implementation class.
#'   E.g. `"ttest_two"`. Used in the S3 class vector of the result.
#' @param engine A string naming the engine. Defaults to `"default"`.
#'   Use a different string to register an alternative engine, e.g. `"cpp"`.
#' @param method A `method_spec` object declaring the method variant and
#'   its default arguments. `NULL` for classical implementations.
#' @param fun_args A `fun_args` object from [fun_args()] declaring the default
#'   values and required status of each test argument. `NULL` if the
#'   implementation does not use [ic_arg()].
#' @param compatible_params A character vector of `ParamDef` types this
#'   implementation can interpret via `write_claim()`. E.g. `c("MU")`.
#' @param vars A named list of extractor functions. Each function takes
#'   `processed` and returns the variable for that role. E.g.:
#'   `list(x = function(p) p$x_data[[1]])`.
#' @param run A function with signature `function(self)` where `self` is
#'   an `infer_context` object. Contains the full implementation logic.
#'   Use [ic_pull()], [ic_arg()], [ic_method_arg()] to access data and args.
#' @param eval_claim A function with signature `function(self, claim)`
#'   that interprets a `ClaimDef` for this implementation. `NULL` if
#'   `write_claim()` is not supported.
#' @param print A function with signature `function(x, ...)` that formats
#'   the result for printing. `NULL` falls back to `print(x$data)`.
#'
#' @return A `test_define` S7 object.
#'
#' @seealso `HTEST_FN()`, `method_spec()`, [via()], [conclude()]
#'
#' @examples
#' \dontrun{
#' new_htest_fn = test_define(
#'     model_type = "x_by",
#'     impl_class = "mytest_two",
#'     engine = "default",
#'     vars = list(
#'         x = function(p) p$x_data[[1]],
#'         group = function(p) p$group_data[[1]]
#'     ),
#'     run = function(self) {
#'         grp = as.character(ic_pull(self, "group"))
#'         resp = ic_pull(self, "x")
#'         # implementation logic
#'     },
#'     print = function(x, ...) {
#'         print(x$data)
#'         invisible(x)
#'     }
#' )
#' }
#' @export
test_define = S7::new_class(
    "test_define",
    properties = list(
        model_type = S7::class_character,
        impl_class = S7::class_character,
        engine = S7::new_property(class = S7::class_character, default = "default"),
        method = S7::new_property(default = NULL),
        fun_args = S7::new_property(default = NULL),
        compatible_params = S7::new_property(default = character(0)),
        vars = S7::new_property(default = list()),
        run = S7::class_function,
        eval_claim = S7::new_property(default = NULL),
        print = S7::new_property(default = NULL)
    )
)

method_spec_cons = S7::new_class(
    "method_spec",
    properties = list(
        method_name = S7::class_character,
        method_type = S7::class_character,
        defaults = S7::new_property(class = S7::class_list, default = list())
    )
)

#' Declare a method variant for a test implementation
#'
#' `method_spec()` declares a named method variant and its default arguments
#' for use in [test_define()]. Pass the result to the `method` property.
#'
#' @param name A string naming the method. E.g. `"boot"`, `"permute"`.
#'   Must match the `.method` argument passed to [via()].
#' @param method_type A string naming the method type. E.g. `"bootstrap"`,
#'   `"replicate"`, `"bayes"`.
#' @param defaults A named list of default arguments for this method.
#'   E.g. `list(n = 1000L, seed = NULL)`.
#'
#' @return A `method_spec` S7 object.
#'
#' @seealso [test_define()], [via()]
#'
#' @examples
#' \dontrun{
#' method_spec(
#'     "boot",
#'     method_type = "bootstrap",
#'     defaults = list(n = 1000L, seed = NULL)
#' )
#' }
#'
#' @export
method_spec = function(name, method_type, defaults = list()) {
    method_spec_cons(
        method_name = name,
        method_type = method_type,
        defaults = defaults
    )
}
