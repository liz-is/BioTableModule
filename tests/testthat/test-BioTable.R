library(shiny)

test_that("reactive filtered data updates as input changes", {
  row_id <- reactiveVal()
  testServer(tableServer, args = list(data = iris, row_id = row_id, id_column_name = "Species"), {
    expect_equal(filtered_data(), iris)

    session$setInputs(cols = c("Sepal.Length", "Sepal.Width", "Species"))
    expect_equal(filtered_data(), iris[, c("Sepal.Length", "Sepal.Width", "Species")])

    row_id("virginica")
    session$flushReact()
    expect_equal(filtered_data(),
                 iris[iris$Species=="virginica", c("Sepal.Length", "Sepal.Width", "Species")],
                 ignore_attr = TRUE)

  })
})
