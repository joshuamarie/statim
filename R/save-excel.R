#' Save statistical output to Excel
#'
#' `save_excel()` is the terminal pipeline step for writing results to an
#' `.xlsx` file. It snapshots the console print output and writes it
#' into an Excel sheet as a formatted monospace report.
#'
#' @param x A `cld_exec` object from `conclude()`.
#' @param file Path to the `.xlsx` file to write.
#' @param ... Currently unused.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' \dontrun{
#' sleep |>
#'     define_model(extra ~ sleep) |>
#'     prepare_test(TTEST) |>
#'     conclude() |>
#'     save_excel("t-test.xlsx")
#' }
#'
#' @export
save_excel = S7::new_generic("save_excel", "x")

S7::method(save_excel, cld_exec) = function(x, file, ...) {
    rlang::check_installed("openxlsx2", reason = "to export results to Excel")

    lines = capture.output(print(x))
    lines = gsub("\033\\[[0-9;]*m", "", lines)
    lines = lines[!grepl("^[[:space:]]*[┌┐└┘├┤│]", lines)]

    meta = x@cld_meta
    sheet = substr(meta$stat_name, 1, 31)

    wb = openxlsx2::wb_workbook()
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

    # col width derived from the longest line
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

    openxlsx2::wb_save(wb, file = file)
    invisible(x)
}
