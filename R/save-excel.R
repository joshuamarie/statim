#' Save statistical output to Excel
#'
#' `save_excel()` is the terminal pipeline step for writing results to an
#' `.xlsx` file. It snapshots the console print output and writes it
#' into an Excel sheet as a formatted monospace report.
#'
#' @param x A `cld_exec` object from `conclude()`.
#' @param file Path to the `.xlsx` file to write.
#' @param sheet Sheet name. Defaults to the test name (e.g. `"T-Test"`).
#'   Truncated to 31 characters (Excel limit).
#' @param overwrite Controls overwrite behaviour when `file` already exists.
#'   One of `"none"` (default, aborts), `"sheet"` (replaces only the matching
#'   sheet), or `"file"` (replaces the entire workbook). When `NULL`, the user
#'   is prompted interactively.
#' @param ... Currently unused.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' \dontrun{
#' sleep |>
#'     define_model(extra ~ group) |>
#'     prepare_test(TTEST) |>
#'     conclude() |>
#'     save_excel("t-test.xlsx")
#'
#' iris |>
#'     define_model(pairwise(1:4, direction = "all")) |>
#'     prepare_test(TTEST) |>
#'     conclude() |>
#'     save_excel("t-test.xlsx", sheet = "t-test-pairwise")
#' }
#'
#' @export
save_excel = S7::new_generic("save_excel", "x", function(x, file, sheet = NULL, overwrite = NULL, ...) S7::S7_dispatch())

S7::method(save_excel, cld_exec) = function(x, file, sheet = NULL, overwrite = NULL, ...) {
    rlang::check_installed("openxlsx2", reason = "to export results to Excel")

    file_exists = file.exists(file)

    meta = x@cld_meta
    sheet = substr(sheet %||% meta$stat_name, 1, 31)

    sheet_exists = file_exists && sheet %in% openxlsx2::wb_get_sheet_names(openxlsx2::wb_load(file))

    if (sheet_exists) {
        overwrite = if (is.null(overwrite)) {
            cli::cli_inform(c(
                "!" = "Sheet {.val {sheet}} already exists in {.path {file}}.",
                "i" = "What would you like to do?",
                " " = "[1] Replace only this sheet",
                " " = "[2] Replace the entire workbook",
                " " = "[3] Abort"
            ))
            choice = readline("Selection: ")
            switch(trimws(choice),
                   "1" = "sheet",
                   "2" = "file",
                   "3" = "none",
                   cli::cli_abort("Invalid selection. Aborting.")
            )
        } else {
            rlang::arg_match(overwrite, c("none", "sheet", "file"))
        }

        if (overwrite == "none") {
            cli::cli_abort("Aborted. File {.path {file}} was not modified.")
        }
    } else if (file_exists) {
        overwrite = "sheet"
    }

    lines = capture.output(print(x))
    lines = gsub("\033\\[[0-9;]*m", "", lines)
    lines = lines[!grepl("^[[:space:]]*[\u250c\u2510\u2514\u2518\u251c\u2524]", lines)]

    wb = if (file_exists && overwrite == "sheet") {
        existing = openxlsx2::wb_load(file)
        if (sheet %in% openxlsx2::wb_get_sheet_names(existing)) {
            existing = openxlsx2::wb_remove_worksheet(existing, sheet = sheet)
        }
        existing
    } else {
        openxlsx2::wb_workbook()
    }

    wb = openxlsx2::wb_add_worksheet(wb, sheet = sheet)

    font_name = "Courier New"
    font_size = 10

    for (i in seq_along(lines)) {
        wb = openxlsx2::wb_add_data(
            wb,
            sheet = sheet,
            x = lines[[i]],
            start_row = i,
            start_col = 1,
            col_names = FALSE
        )
        wb = openxlsx2::wb_add_font(
            wb,
            sheet = sheet,
            dims = openxlsx2::wb_dims(rows = i, cols = 1),
            name = font_name,
            size = font_size,
            bold = grepl("^==", lines[[i]])
        )
    }

    max_chars = max(nchar(lines), na.rm = TRUE)
    col_width = max_chars * 0.9

    wb = openxlsx2::wb_set_col_widths(
        wb,
        sheet = sheet,
        cols = 1,
        widths = col_width
    )
    wb = openxlsx2::wb_freeze_pane(
        wb,
        sheet = sheet,
        first_row = TRUE
    )
    wb = openxlsx2::wb_set_row_heights(
        wb,
        sheet = sheet,
        rows = seq_along(lines),
        heights = 14
    )

    openxlsx2::wb_save(wb, file = file, overwrite = file_exists)
    invisible(x)
}
