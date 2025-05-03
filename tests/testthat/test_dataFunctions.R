test_that("emptyFunction works", {
  expect_equal(emptyFunction(), NA)
})

test_that("dataFunctions works", {
  dataF <- dataFunctions$new()
  expect_equal(class(dataF), c("dataFunctions", "R6"))
  expect_equal(dataF$functions, NA)
  expect_equal(dataF$names, NA)
  dataF$add(theFunction = mean, theName = "mean")
  dataF$add(theFunction = median, theName = "median")
  expect_equal(dataF$names, c("mean", "median"))
  expect_equal(
    dataF$do("mean")(datasets::mtcars$mpg) |> as.character(),
    "20.090625"
  )
  expect_equal(
    dataF$do("median")(datasets::mtcars$mpg) |> as.character(),
    "19.2"
  )
})
