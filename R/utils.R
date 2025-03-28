#' Filter a data frame by row
#'
#' Wrapper for dplyr::filter that takes a column name as a string and filters
#' for rows where that column matches the given `id`. If `id` is NULL or an
#' empty string then performs no filtering.
#'
#' @param data a data frame or similar object accepted by dplyr::filter
#' @param column_name a string
#' @param id NULL or a string
#'
#' @returns a data frame
#' @export
#'
#' @examples
#' filter_by_row(iris, column_name = "Species", id = "setosa")
filter_by_row <- function(data, column_name, id = NULL) {
  if (length(id) == 1 && id == "") {
    id <- NULL
  }

  if (is.null(id)) {
    df <- data
  } else {
    df <- dplyr::filter(data, .data[[column_name]] %in% id)
  }
  return(df)
}

#' Filter a data frame by column
#'
#' Wrapper for dplyr::select that takes vector of a column names as strings and
#' returns a data frame containing only those columns. Retains the same column
#' ordering as the original data frame.
#'
#' @param cols a vector of strings
#' @param data a data frame or similar object accepted by dplyr::filter
#'
#' @returns a data frame
#' @export
#'
#' @examples
#' filter_by_column(iris, cols = c("Sepal.Length", "Sepal.Width", "Species"))
filter_by_column <- function(data, cols = NULL) {
  if (is.null(cols)) {
    df <- data
  } else {
    cols <- names(data)[names(data) %in% cols]

    df <- dplyr::select(data, any_of(cols))
  }
  return(df)
}


#' Find columns matching a pattern
#'
#' @param data a data frame
#' @param pattern a regular expression specifying column names to match
#'
#' @returns a vector of column names
#' @export
#'
#' @examples
#' get_cols_to_format(iris, pattern = "Length|Width")
get_cols_to_format <- function(data, pattern) {
  columns <- names(data)
  columns[stringr::str_detect(columns, pattern = pattern)]
}
