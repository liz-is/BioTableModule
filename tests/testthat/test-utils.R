# filter_by_column
test_that("column filtering works", {
  expect_equal(filter_by_column(iris, cols = c("Sepal.Length", "Sepal.Width", "Species")),
               iris[, c("Sepal.Length", "Sepal.Width", "Species")])
})


test_that("column filtering handles NULL", {
  expect_equal(filter_by_column(iris, cols = NULL),
               iris)
})


test_that("column filtering returns original column order", {
  expect_equal(filter_by_column(iris, cols = c("Species", "Sepal.Length", "Sepal.Width")),
               iris[, c("Sepal.Length", "Sepal.Width", "Species")])
})

# get_cols_to_format

test_that("column finding handles NULL", {
  expect_equal(get_cols_to_format(iris, cols = NULL),
               NULL)
})

test_that("column finding handles vector of column names", {
  expect_equal(get_cols_to_format(iris, cols = c("Sepal.Length", "Sepal.Width", "Species")),
               c("Sepal.Length", "Sepal.Width", "Species"))
})

test_that("column finding handles pattern", {
  expect_equal(get_cols_to_format(iris, cols = c("Length|Width")),
               c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))
})


test_that("column finding handles incorrect column name", {
  expect_warning(out <- get_cols_to_format(iris, cols = c("Sepal.Length", "Sepal_Width", "Petal_width")),
                 "Columns not found: Sepal_Width, Petal_width")

  expect_equal(out, "Sepal.Length")
})

# filter_by_row

test_that("row finding handles NULL", {
  expect_equal(filter_by_row(iris, "Species", id = NULL),
               iris)
})

test_that("row finding handles empty string", {
  expect_equal(filter_by_row(iris, "Species", id = ""),
               iris)
})


test_that("row finding works", {
  expect_equal(filter_by_row(iris, "Species", id = "virginica"),
               iris[iris$Species == "virginica",],
               ignore_attr = TRUE)
})
