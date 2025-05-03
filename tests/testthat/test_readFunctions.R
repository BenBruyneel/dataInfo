test_that("readData works", {
  result <- readData(dataframe = datasets::mtcars)
  expect_equal(class(result), "function")
  result <- result()
  expect_equal(class(result), c("dataElement", "R6"))
  expect_identical(result$data, datasets::mtcars)
  expect_length(result[["info"]], 1)
  expect_equal(names(result[["info"]]), "source")
  expect_equal(result[["info"]][["source"]], "data")
})

test_that("readDataFrame works", {
  result <- readDataFrame(dataframe = list(datasets::mtcars, datasets::iris))
  expect_equal(class(result), "function")
  result <- result()
  expect_equal(class(result), "list")
  expect_equal(class(result[[1]]), c("dataElement", "R6"))
  expect_equal(class(result[[1]]), c("dataElement", "R6"))
  expect_identical(result[[1]]$data, datasets::mtcars)
  expect_equal(names(result[[1]]$info), "source")
  expect_equal(result[[1]]$info$source, "data")
  expect_identical(result[[2]]$data, datasets::iris)
  expect_equal(names(result[[2]]$info), "source")
  expect_equal(result[[2]]$info$source, "data")
})

test_that("readCSV works", {
  filename <- tempfile()
  utils::write.csv(datasets::mtcars, file = filename, row.names = FALSE)
  result <- readCSV(filename = filename)()
  unlink(filename)
  expect_equal(class(result), "list")
  expect_equal(class(result[[1]]), c("dataElement", "R6"))
  expect_equal(result[[1]]$info$source, "csv")
  expect_equal(result[[1]]$info$filename, filename)
  expect_equal(colnames(result[[1]]$data), c("x", "y"))
  expect_equal(nrow(result[[1]]$data), 32)
  expect_equal(sum(result[[1]]$data[, 1]), sum(datasets::mtcars[, 1]))
  expect_equal(sum(result[[1]]$data[, 2]), sum(datasets::mtcars[, 2]))
})

test_that("readExcel works", {
  demoExcelFile <- system.file("demoFiles/mtcars.xlsx", package = "XLConnect")
  result <- readExcel(demoExcelFile)
  expect_equal(class(result), "function")
  result <- result()
  expect_equal(class(result), "list")
  expect_equal(class(result[[1]]), c("dataElement", "R6"))
  expect_equal(ncol(result[[1]]$info), 2)
  expect_equal(names(result[[1]]$info), c("source", "filename"))
  expect_equal(result[[1]]$info[["source"]], "xlsx")
  result <- readExcel(
    demoExcelFile,
    columns = 1:ncol(mtcars),
    columnNames = NA,
    rowNames = rownames(datasets::mtcars)
  )()
  expect_identical(result[[1]]$data, datasets::mtcars)
})

test_that("fileInfo works", {
  filename <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".txt")
  writeLines(c("test", "file"), filename)
  result <- fileInfo(filename)()
  unlink(filename)
  expect_equal(result[[1]]$info$filename, filename)
  expect_equal(result[[1]]$data$data, NA)
})

test_that("fileInfo.CSV works", {
  filename <- tempfile()
  utils::write.csv(datasets::mtcars, file = filename, row.names = FALSE)
  result <- fileInfo.CSV(filename = filename)()
  unlink(filename)
  expect_equal(result[[1]]$info$filename, filename)
  expect_equal(result[[1]]$info$description[[1]], colnames(datasets::mtcars))
  expect_equal(result[[1]]$data$data, NA)
})
