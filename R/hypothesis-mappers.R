#' Declare per-variant claim translators
#'
#' `claim_translate()` holds a named set of translator functions, one per
#' variant name. Use `"default"` for the base (no `via()`) case. At
#' `conclude()` time, the active variant name is used to look up the right
#' translator. If no translator is found for the active variant and a claim
#' is present, an error is raised immediately.
#'
#' @param ... Named translator functions or [map_claim()] objects.
#'
#' @return An object of class `"claim_translate"`.
#'
#' @export
claim_translate = function(...) {
    translators = list(...)
    if (is.null(names(translators)) || any(!nzchar(names(translators)))) {
        cli::cli_abort("All arguments to {.fn claim_translate} must be named.")
    }
    invalid = !vapply(translators, is.function, logical(1))
    if (any(invalid)) {
        cli::cli_abort(
            "All arguments to {.fn claim_translate} must be functions: {.val {names(translators)[invalid]}}."
        )
    }
    structure(translators, class = "claim_translate")
}

#' Build a claim translator from named resolver functions
#'
#' `map_claim()` produces a translator function by mapping impl `fn` formal
#' names to resolver functions. Each resolver receives `(claim, processed)`
#' and returns the value for its argument. Resolvers that only need `claim`
#' can simply ignore `processed`.
#'
#' @param ... Named resolver functions. Names must match formals of the
#'   impl's `fn`. Each resolver has signature `function(claim, processed)`.
#'
#' @return A function of class `"map_claim"` with signature
#'   `function(claim, processed)`.
#'
#' @export
map_claim = function(...) {
    resolvers = list(...)
    if (is.null(names(resolvers)) || any(!nzchar(names(resolvers)))) {
        cli::cli_abort("All arguments to {.fn map_claim} must be named.")
    }
    invalid = !vapply(resolvers, is.function, logical(1))
    if (any(invalid)) {
        cli::cli_abort(
            "All arguments to {.fn map_claim} must be functions: {.val {names(resolvers)[invalid]}}."
        )
    }
    fn = function(claim, processed) {
        args = lapply(resolvers, function(r) r(claim, processed))
        do.call(claim_args, args)
    }
    structure(fn, class = c("map_claim", "function"))
}

#' Chained equality operator for null hypotheses
#'
#' `%=%` declares that all chained population parameters are hypothesized
#' to be equal. Used inside [state_null()] only — it is a syntactic macro
#' and will error if called outside that context.
#'
#' @examples
#' sleep |>
#'     define_model(x_by(extra, group)) |>
#'     prepare_test(ANOVA) |>
#'     state_null(
#'         MU(extra, group == "1") %=%
#'         MU(extra, group == "2")
#'     ) |>
#'     conclude()
#'
#' @export
`%=%` = function(lhs, rhs) {
    cli::cli_abort(
        "{.code %=%} must be used inside {.fn state_null}."
    )
}
