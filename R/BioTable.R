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
