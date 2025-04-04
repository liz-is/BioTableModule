#' Creates UI for a BioTable
#'
#' UI for a BioTable, which includes a table description, column selector, and
#' the table itself. Selecting columns in the column selector automatically
#' updates the columns that are included in the table.
#'
#'
#' @param id id for the module
#' @param md_description logical, whether to include a table description from a
#'   markdown file. The file is expected to be named `desc/{id}.md`. The
#'   directory can be specified using `description_dir`
#' @param description_dir optional, path to directory containing the table
#'   description file
#' @param helper logical, whether to add a helper element using the shinyhelper
#'   package. If TRUE, looks for a file named `{id}.md` in a `helpfiles/`
#'   directory
#'
#' @returns a shiny::tagList
#' @export
#'
#' @examples
tableUI <- function(id, md_description = TRUE, description_dir = "desc", helper = TRUE) {
  table_info_filename <- file.path(description_dir, paste0(id, ".md"))

  col_select <- shinyWidgets::virtualSelectInput(
    inputId = shiny::NS(id, "cols"),
    label = "Columns to show",
    choices = NULL,
    selected = NULL,
    multiple = TRUE,
    width = "100%",
    dropboxWrapper = "body"
  )

  if (helper){
    col_select <- shinyhelper::helper(col_select, content = id)
  }

  tags <- list(col_select, DT::DTOutput(shiny::NS(id, "table")))

  if (md_description) {
    if (file.exists(table_info_filename)){
      tags <- c(list(shiny::includeMarkdown(table_info_filename)), tags)
    } else {
    warning("Description file ", table_info_filename, " not found!")
    }
  }

  shiny::tagList(tags)
}

#' Creates server for a BioTable
#'
#' Server function for a BioTable, which filters the table according to columns
#' selected in the UI (tableUI) and optionally according to a row filter which
#' can be shared across elements.
#'
#' @param id id for the module
#' @param data a data frame or similar object accepted by
#'   dplyr::filter/dplyr::select
#' @param default_cols default columns to show in output
#' @param sci_format_cols columns that should have scientific formatting
#'   applied. By default all numeric values in these columns will be rounded to
#'   three significant digits, and values < 0.001 will have scientific
#'   formatting applied.
#' @param gene_name_cols columns containing gene names, which should be
#'   italicized in the output table
#' @param row_id optional reactive element to use to filter rows
#' @param id_column_name optional column name to use for filtering
#'
#' @returns
#' @export
#'
#' @examples
tableServer <- function(id,
                        data,
                        default_cols = NULL,
                        sci_format_cols = NULL,
                        gene_name_cols = NULL,
                        row_id = shiny::reactive(NULL),
                        id_column_name = NULL) {
  stopifnot(shiny::is.reactive(row_id))
  stopifnot(!shiny::is.reactive(data))
  stopifnot(!shiny::is.reactive(id_column_name))

  if (is.null(default_cols)) {
    default_cols <- colnames(data)
  }
  shinyWidgets::updateVirtualSelect(shiny::NS(id, "cols"),
                                    choices = colnames(data),
                                    selected = default_cols)

  shiny::moduleServer(id, function(input, output, session) {
    filtered_data <- shiny::reactive({
      filter_by_row(data, id_column_name, row_id()) |>
        filter_by_column(input$cols)
    })

    cols_to_format <- get_cols_to_format(data, cols = sci_format_cols)
    signif_digit_js <- DT::JS(
      "function(row, data) {",
      "for (i = 1; i < data.length; i++) {",
      "if (data[i]<0.001 && data[i] > 0){",
      "$('td:eq('+i+')', row).html(data[i].toExponential(2));",
      "}",
      "}",
      "}"
      )

    output$table <- DT::renderDT({
      cols_to_format <- intersect(cols_to_format, colnames(filtered_data()))

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

      if (!is.null(gene_name_cols)) {
        dt <- DT::formatStyle(dt, gene_name_cols, fontStyle = "italic")
      }
      dt
    })

  })
}
