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

