utils::globalVariables(c(".data", "xy"))

inequality = function(a, b, direction = "lteq") {
    op = switch(
        direction,
        "lt" = `<`,
        "lteq" = `<=`,
        "gt" = `>`,
        "gteq" = `>=`,
        "eq" = `==`,
        "neq" = `!=`,
        "all" = function(a, b) TRUE,
        stop("Invalid direction specified.")
    )

    op(a, b)
}

pairs_generator = function(x, direction = "lteq", simplify = TRUE) {
    pairs = tidyr::expand_grid(x = x, y = x) |>
        dplyr::filter(inequality(.data$x, .data$y, direction = {{ direction }}))

    out = if (simplify) {
        pairs |>
            dplyr::rowwise() |>
            dplyr::mutate(xy = list(c(.data$x, .data$y))) |>
            dplyr::pull(xy)
    } else {
        pairs
    }

    out
}
