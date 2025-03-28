#' Creates UI for a BioTable
#'
#' UI for a BioTable, which includes a table description, column selector, and
#' the table itself. Selecting columns in the column selector automatically
#' updates the columns that are included in the table.
#'
#'
#' @param id id for the module
#' @param all_cols character vector of all columns to list in the selection
#' @param default_cols columns to make visible by default
#'
#' @returns a shiny::tagList
#' @export
#'
#' @examples
tableUI <- function(id, all_cols, default_cols = NULL) {
  table_info_filename <- paste0("text/desc_", id, ".md")

  if (is.null(default_cols)) {
    default_cols <- all_cols
  }

  col_select <- shinyWidgets::virtualSelectInput(
    inputId = shiny::NS(id, "cols"),
    label = "Columns to show",
    choices = all_cols,
    selected = default_cols,
    multiple = TRUE,
    width = "100%",
    dropboxWrapper = "body"
  ) |> shinyhelper::helper(content = id)

  shiny::tagList(shiny::includeMarkdown(table_info_filename),
          col_select,
          DT::DTOutput(shiny::NS(id, "table")))
}

#' Creates server for a BioTable
#'
#' Server function for a BioTable, which filters the table according to columns
#' selected in the UI (tableUI) and optionally according to a row filter which
#' can be shared across elements.
#'
#' @param id id for the module
#' @param data a data frame or similar object accepted by dplyr::filter/dplyr::select
#' @param row_id optional reactive element to use to filter rows
#' @param id_column_name optional column name to use for filtering
#'
#' @returns
#' @export
#'
#' @examples
tableServer <- function(id,
                        data,
                        row_id = shiny::reactive(NULL),
                        id_column_name = NULL) {
  stopifnot(shiny::is.reactive(row_id))
  stopifnot(!shiny::is.reactive(data))
  stopifnot(!shiny::is.reactive(id_column_name))

  shiny::moduleServer(id, function(input, output, session) {
    filtered_data <- shiny::reactive({
      filter_by_row(data, id_column_name, row_id()) |>
        filter_by_column(input$cols)
    })

    output$table <- DT::renderDT({
      cols_to_format <- get_cols_to_format(filtered_data(), pattern = "SE|Beta|FDR|Pvalue|P-value")
      signif_digit_js <- DT::JS(
        "function(row, data) {",
        "for (i = 1; i < data.length; i++) {",
        "if (data[i]<0.001 && data[i] > 0){",
        "$('td:eq('+i+')', row).html(data[i].toExponential(2));",
        "}",
        "}",
        "}"
      )

      dt <- DT::datatable(
        filtered_data(),
        filter = "top",
        extensions = "Buttons",
        options = list(
          dom = 'frtipB',
          buttons = c('csv', 'excel'),
          rowCallback = signif_digit_js
        )
      ) |> DT::formatSignif(cols_to_format, digits = 3)

      if ("Gene" %in% names(filtered_data())) {
        dt <- DT::formatStyle(dt, "Gene", fontStyle = "italic")
      }
      dt
    })

  })
}
