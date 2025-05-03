test_that("dataElement works", {
  theData <- dataElement$new(
    data = datasets::mtcars,
    id = 1,
    name = "mtcars",
    info = list(source = "datasets")
  )
  expect_equal(class(theData), c("dataElement", "R6"))
  expect_equal(dim(theData$info), c(1, 2))
  expect_equal(theData$info$id, 1)
  expect_equal(theData$info$source, "datasets")
  expect_equal(theData$data, datasets::mtcars)
  expect_equal(theData$name, "mtcars")
  theData$dos <- dataFunctions$new()
  theSummary <- function(data, ...) {
    summary(data$data)
  }
  thePlot <- function(data, ...) {
    plot(data$data, ...)
  }
  theData$dos$add(theFunction = theSummary, theName = "summary")
  theData$dos$add(theFunction = thePlot, theName = "plot")
  expect_equal(theData$dos$names, c("summary", "plot"))
  expect_equal(dim(theData$do("summary")), c(6, 11))
  expect_equal(theData$do("summary")[1, 1], "Min.   :10.40  ")
  expect_equal(theData$do("summary")[3, 3], "Median :196.3  ")
})

test_that("dataInfo works", {
  tElement <- list(
    dataElement$new(
      name = "Test 001",
      data = datasets::mtcars,
      info = data.frame(name = c("mtcars"))
    ),
    dataElement$new(
      name = "Test 002",
      data = datasets::iris,
      info = data.frame(name = c("iris"))
    ),
    dataElement$new(
      name = "Test 003",
      data = datasets::volcano,
      info = data.frame(name = c("volcano"))
    )
  )
  tInfo <- dataInfo$new(data = tElement)
  expect_equal(class(tInfo), c("dataInfo", "dataElement", "R6"))
  expect_equal(tInfo$length, 3)
  expect_equal(tInfo$item(id = 1), datasets::mtcars)
  expect_equal(tInfo$data$`Test 001`, datasets::mtcars)
  expect_equal(tInfo$item(id = 2), datasets::iris)
  expect_equal(tInfo$item(id = 3), datasets::volcano)
  expect_equal(tInfo$data[[1]], datasets::mtcars)
  expect_equal(
    tInfo$item.list(id = 1:2),
    list(datasets::mtcars, datasets::iris)
  )
  dimensions <- function(data, index = 1, id = NA) {
    result <- list()
    for (counter in index) {
      result[[length(result) + 1]] <- dim(data$item(index = counter))
      names(result)[length(result)] <- data$names[counter]
    }
    return(result)
  }
  plotData <- function(data, index = 1, id = NA) {
    plot(data$item(index = index, id = id))
  }
  expect_equal(dimensions(data = tInfo), list(`Test 001` = c(32, 11)))
  expect_equal(
    dimensions(data = tInfo, index = 1:tInfo$length),
    list(`Test 001` = c(32, 11), `Test 002` = c(150, 5), `Test 003` = c(87, 61))
  )
  tInfo$dos <- dataFunctions$new(
    functions = list(dimensions),
    names = "dimensions"
  )
  tInfo$dos$add(theFunction = plotData, theName = "plot")
  expect_equal(tInfo$dos$names, c("dimensions", "plot"))
  expect_equal(
    tInfo$do(whichFunction = "dimensions", index = 1),
    list(`Test 001` = c(32, 11))
  )
  expect_equal(
    tInfo$do(whichFunction = "dimensions", index = 1:tInfo$length),
    list(`Test 001` = c(32, 11), `Test 002` = c(150, 5), `Test 003` = c(87, 61))
  )
})

test_that("dataList works", {
  testList <- dataList$new(
    name = "Test",
    dataObjects = list(
      dataInfo$new(
        name = "cars"
      ),
      dataInfo$new(
        name = "USArrests"
      ),
      dataInfo$new(
        name = "Penguins"
      )
    )
  )
  expect_equal(class(testList), c("dataList", "dataInfo", "dataElement", "R6"))
  expect_equal(class(testList$item("cars")), c("dataInfo", "dataElement", "R6"))
  expect_equal(class(testList$data[[2]]), c("dataInfo", "dataElement", "R6"))
  expect_equal(
    class(testList$data$Penguins),
    c("dataInfo", "dataElement", "R6")
  )
  expect_equal(
    testList$info,
    data.frame(
      name = c('cars', 'USArrests', 'Penguins'),
      class = c('dataInfo', 'dataInfo', 'dataInfo'),
      length = c(0, 0, 0)
    )
  )
  testList$item("cars")$add(
    data = list(dataElement$new(
      data = datasets::mtcars,
      info = list(source = "datasets")
    ))
  )
  testList$item("cars")$add(
    data = list(dataElement$new(
      data = datasets::mtcars,
      info = list(source = "datasets", extra = "Once again")
    ))
  )
  testList$data$Penguins$add(
    data = list(dataElement$new(
      data = datasets::penguins,
      info = list(source = "datasets")
    ))
  )
  expect_equal(
    testList$info,
    data.frame(
      name = c('cars', 'USArrests', 'Penguins'),
      class = c('dataInfo', 'dataInfo', 'dataInfo'),
      length = c(2, 0, 1)
    )
  )
  expect_equal(testList$item("cars")$item(id = 1), datasets::mtcars)
  expect_equal(testList$item("cars")$item(id = 2), datasets::mtcars)
  expect_equal(testList$item("cars")$data[[1]], datasets::mtcars)
  expect_equal(testList$data$cars$data[[1]], datasets::mtcars)
  expect_equal(testList$item("Penguins")$item(id = 1), datasets::penguins)
  # create a function for some data analysis
  findMeansetc <- function(
    data,
    index = 1,
    id = NA,
    selectedIsland = "Biscoe",
    selectedSpecies = "Adelie",
    selectedSex = c("female", "male")
  ) {
    data$item(index = index, id = id) %>%
      dplyr::filter(
        island %in% selectedIsland,
        species %in% selectedSpecies,
        sex %in% selectedSex
      ) %>%
      dplyr::group_by(island, species, sex) %>%
      dplyr::summarize(
        meanBill_len = mean(bill_len, na.rm = TRUE),
        sdBill_len = sd(bill_len, na.rm = TRUE),
        count = dplyr::n()
      ) %>%
      dplyr::ungroup()
  }
  result <- findMeansetc(data = testList$data$Penguins)
  expect_equal(
    formatC(result$meanBill_len, digits = 6),
    c("37.3591", "40.5909")
  )
  testList$data$Penguins$dos <- dataFunctions$new(
    functions = list(findMeansetc),
    names = "gather"
  )
  expect_equal(testList$item("Penguins")$dos$names, "gather")
  result <- testList$item("Penguins")$do("gather", id = 1)
  expect_equal(
    formatC(result$meanBill_len, digits = 6),
    c("37.3591", "40.5909")
  )
})
