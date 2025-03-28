filter_by_row <- function(data, column_name, id) {
  if (length(id) == 1 && id == "") {
    id <- NULL
  }

  if (is.null(id)) {
    df <- data
  } else {
    df <- data %>%
      dplyr::filter(.data[[column_name]] %in% id)
  }
  return(df)
}


filter_by_column <- function(data, cols) {
  if (is.null(cols)) {
    df <- data
  } else {
    cols <- names(data)[names(data) %in% cols]

    df <- data %>%
      dplyr::select(any_of(cols))
  }
  return(df)
}


get_cols_to_format <- function(data, pattern) {
  columns <- names(data)
  columns[stringr::str_detect(columns, pattern = pattern)]
}
