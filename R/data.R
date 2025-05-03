# ---- dataElement ----

#' @title dataElement
#'
#' @description
#'  R6 class for holding data and related info (metadata). It holds a single
#'  element of data which can be anything, though the original intent was for it
#'  to hold data.frame's and additional meta data. The object is capable of also
#'  holding related functions that can be initiated from the object itself via
#'  the 'do' function. These functions are contained in their own
#'  'dataFunctions' R6 class object.
#'
#'  A data element can have an id (integer) and/or a name (character vector).
#'   These are optional.
#'
#' @examples
#' theData <- dataElement$new(
#' data = datasets::penguins,
#' id = 1,
#' name = "penguins",
#' info = list(source = "datasets")
#' )
#' theData$info
#' theData$data |> head()
#' # use of dataFunctions
#' thePlot <- function(data) {
#' plot(data$data)
#' }
#' theSummary <- function(data) {
#'   summary(data$data)
#' }
#' theData$dos <- dataFunctions$new(functions = list(theSummary, thePlot),
#'                                  names = c("summary","plot"))
#' theData$dos
#' theData$do("summary")[1:6, 3:4]
#' theData$do("plot")
#'
#' @export
dataElement <- R6::R6Class(
  "dataElement",
  private = list(
    # integer value that holds the id. In a more complex object with more than
    # one dataElement, this number should be unique
    id_ = NA,
    # character value, alternative to id_, optional
    name_ = "",
    # data.frame that contains metadata for data_
    info_ = NA,
    # vector, usually a data.frame, holding the data contents
    data_ = NA,
    # vector, either NA or an R6 object
    do_ = NA
  ),
  public = list(
    #' @description initializes the object
    #'
    #' @param id integer value that sets the id of the dataElement
    #' @param name character value that sets the name of the dataElement
    #' @param info specifies the meta data of the dataElement. This is best
    #'  either a list or a data.frame.
    #' @param infoClass function, specifies what to use to transform the info
    #'  argument into (usuallt a data.frame)
    #' @param data data to be stored in the dataElement (usually a data.frame)
    #' @param do a dataFunctions object that sets which functions can be used
    #'  with the dataElement
    initialize = function(
      id = NA,
      name = "",
      info = NA,
      infoClass = as.data.frame,
      data = NA,
      do = NA
    ) {
      self$id <- id
      if (is.na(name)) {
        name <- ""
      }
      self$name <- name
      self$dos <- do
      if (!is.Class(info, "data.frame")) {
        info <- infoClass(info)
      }
      self$info = info
      if (is.Class(data, "function")) {
        self$data = data()
      } else {
        self$data = data
      }
    },
    #' @description For printing purposes: prints the info object
    #'
    #' @param ... no arguments, the function takes care of printing
    print = function() {
      if (!identical(self$info, NA)) {
        print(self$info |> tibble::tibble())
      } else {
        cat(NA)
        cat("\n")
      }
      invisible(self)
    },
    #' @description Function that starts one of the functions in the associated
    #'  dataFunctions object
    #'
    #' @param whichFunction character vector specifying which function to call.
    #'  Note: function needs to be present in the dataFunction object associated
    #'  with the dataElement object
    #' @param ... for passing on parameters to the function to be called
    #'
    #' @note apart from ... the parameter data = self is also passed on in order
    #'  to give the function access to the data in the dataElement object. Be
    #'  careful changing the data parameter as it might affect the calling
    #'  dataElement.
    do = function(whichFunction, ...) {
      if (!identical(self$dos, NA)) {
        if (is.Class(self$dos, "dataFunctions")) {
          return(self$dos$do(whichFunction = whichFunction)(
            data = self,
            ...
          ))
        }
      }
      return(NA)
    }
  ),
  active = list(
    #' @field id gets & sets the id of the object
    id = function(value) {
      if (missing(value)) {
        return(private$id_)
      } else {
        private$id_ <- value
      }
    },
    #' @field name gets & sets the name of the object
    name = function(value) {
      if (missing(value)) {
        return(private$name_)
      } else {
        private$name_ = value
      }
    },
    #' @field info gets & sets the info of the object
    #' @note if the new info contains a column named "id",
    #'  then this will be used to set the id of the object.
    #'  Also: the actual (private) info_ will not contain
    #'  the "id" column
    info = function(value) {
      if (missing(value)) {
        if (is.Class(private$info_, "data.frame")) {
          if (!is.na(self$id)) {
            result <- private$info_ |>
              dplyr::mutate(id = self$id) |>
              dplyr::select(id, dplyr::everything())
            return(result)
          }
        }
        return(private$info_)
      } else {
        if ("id" %in% colnames(value)) {
          private$id_ <- value$id
          private$info_ <- value |> dplyr::select(-id)
        } else {
          private$info_ <- value
        }
      }
    },
    #' @field data get & sets the data of the object
    data = function(value) {
      if (missing(value)) {
        return(private$data_)
      } else {
        private$data_ <- value
      }
    },
    #' @field dos provides access to dataFunctions object associated with the
    #'  dataElement object.
    dos = function(value) {
      if (missing(value)) {
        return(private$do_)
      } else {
        private$do_ <- value
      }
    }
  )
)

# ---- dataInfo ----

#' @title dataInfo
#'
#' @description
#'  R6 class for holding a set of dataElement objects. The original intent was
#'  for all dataElements to be the same. If they are not, it should still work,
#'  though extra care needs to be take to the 'dataFunctions' called via do().
#'
#' @examples
#' tElement <- list(
#'  dataElement$new(
#'    name = "Test 001",
#'    data = datasets::mtcars,
#'    info = data.frame(name = c("mtcars"))
#'  ),
#'  dataElement$new(
#'    name = "Test 002",
#'    data = datasets::iris,
#'    info = data.frame(name = c("iris"))
#'  ),
#'  dataElement$new(
#'    name = "Test 003",
#'    data = datasets::volcano,
#'    info = data.frame(name = c("volcano"))
#'  )
#' )
#' tElement
#' tInfo <- dataInfo$new(data = tElement)
#' tInfo
#' tInfo$length
#' tInfo$add(
#'  data = list(dataElement$new(
#'    name = "Penguins",
#'    data = datasets::penguins,
#'    info = list(name = "penguins", datasource = "datasets")
#'  ))
#' )
#' tInfo
#'
#' # different ways of referring to the actual data
#' tInfo$item(id = 1) |> head()
#' tInfo$data[[1]] |> head()
#' tInfo$data$`Test 001` |> head()
#' tInfo$raw[[1]]
#' tInfo$raw[[1]]$data |> head()
#' tInfo$item.list(id = 1:2)
#' # register & use functions
#' dimensions <- function(data, index = 1, id = NA) {
#'  result <- list()
#'  for (counter in index) {
#'   result[[length(result) + 1]] <- dim(data$item(index = counter))
#'   names(result)[length(result)] <- data$names[counter]
#'  }
#'  return(result)
#' }
#' dimensions(data = tInfo)
#' dimensions(data = tInfo, index = 1:tInfo$length)
#'
#' plotData <- function(data, index = 1, id = NA) {
#'   plot(data$item(index = index, id = id))
#' }
#' plotData(tInfo)
#' tInfo$dos <- dataFunctions$new(
#' functions = list(dimensions, plotData),
#'  names = c("dimensions","plot")
#' )
#'
#' tInfo$do(whichFunction = "dimensions", index = 1)
#' tInfo$do(whichFunction = "dimensions", index = 1:tInfo$length)
#' tInfo$do("plot")
#' tInfo$do("plot", id = 2)
#' tInfo$do("plot", id = 3)
#' tInfo$do("plot", id = 4)
#'
#' @export
dataInfo <- R6::R6Class(
  "dataInfo",
  inherit = dataElement,
  private = list(),
  # Note that the private data_ object is used as a list of the different
  # dataElements, while info_ holds the next id to be given to a new dataElement
  # (preferably via the 'add' function)
  public = list(
    #' @description create a new dataInfo object
    #'
    #' @param name character vector, name of the dataInfo object
    #' @param data list of dataElement objects to be added to new object
    #' @param clone logical value, defines whether the dataElements to be added
    #'  are to be 'cloned'. Default is TRUE to prevent odd situations, option
    #'  may be removed in the future
    #'
    #' @return a new 'dataInfo' object
    initialize = function(name = "", data = list(), clone = TRUE) {
      self$name <- name
      private$data_ <- list()
      private$info_ <- 1
      self$add(data = data, clone = clone)
      invisible(self)
    },
    #' @description
    #' For printing purposes: prints the info of the different dataElements as a
    #'  tibble
    #' @param ... no arguments, the function takes care of printing
    #'
    #' @note no arguments, the function takes care of printing
    print = function(...) {
      if (self$length > 0) {
        print(self$info |> tibble::tibble())
      } else {
        cat(NA)
        cat("\n")
      }
      invisible(self)
    },
    #' @description saves the private data_ list (of dataElement). By default
    #'  the file format is '.rds' (via \code{link[base]{saveRDS}})
    #'
    #' @param path path where to place file, should end in '/'
    #' @param filename prefix to the file name to be added. If not specified,
    #'  then the object's name will be used
    #' @param overwrite logical vector that defines what to do if there is
    #'  already a file with the file name to be used
    #'
    #' @return logical vector TRUE if save was successful (otherwise FALSE)
    save = function(path = "", filename = "", overwrite = TRUE) {
      result <- FALSE
      if (!self$empty) {
        if (!(self$name == "" & filename == "")) {
          fileName <- paste(
            c(
              ifelse(
                stringr::str_detect(path, pattern = "/$"),
                path,
                paste0(path, "/")
              ),
              filename,
              ifelse(filename == "" | self$name == "", "", "-"),
              self$name,
              ".rds"
            ),
            collapse = ""
          )
          if (
            (overwrite) |
              (!overwrite & (!file.exists(fileName)))
          ) {
            saveRDS(private$data_, file = fileName)
            if (file.exists(fileName)) {
              result <- TRUE
            }
          }
        }
      }
      names(result) <- self$name
      return(result)
    },
    #' @description loads the specified file into the private data_ field. The
    #'  data in the file should be a list of dataElement objects. By default
    #'  data is loaded from '.rds' files via the \code{link[base]{readRDS}}
    #'  function
    #'
    #' @param path path where to place file, should end in '/'
    #' @param filename prefix to the file name to be added
    #'
    #' @return logical vector TRUE if load successful,
    load = function(path = "", filename = "") {
      result <- FALSE
      fileName <- paste(
        c(
          ifelse(
            stringr::str_detect(path, pattern = "/$"),
            path,
            paste0(path, "/")
          ),
          ifelse(filename == "", self$name, filename),
          ".rds"
        ),
        collapse = ""
      )
      if (file.exists(fileName)) {
        private$data_ <- readRDS(file = fileName)
        self$index <- length(private$data_) + 1
        result <- TRUE
      }
      names(result) <- self$name
      return(result)
    },
    #' @description checks if the index number provided is larger than 0 and
    #'  lower than or equal to the length of the list of dataElement objects in
    #'  the object. This can be used as a check to prevent out-of-range index
    #'  values
    #'
    #' @param index integer vector to be checked
    #'
    #' @returns logical vector
    #' @export
    validIndex = function(index = NA) {
      if (length(index) == 1) {
        if (!identical(index, NA)) {
          if ((index > 0) & index <= self$length) {
            return(TRUE)
          }
        }
        return(FALSE)
      } else {
        return(purrr::map_lgl(index, ~ self$validIndex(index = .x)))
      }
    },
    #' @description finds index number of the dataElement in the list which
    #'  has a certain id
    #'
    #' @param id integer vector: the id for which the row number is to be found
    #' @param na.rm logical vector determines if NA's should be removed from the
    #'  result
    #'
    #' @return NA or integer vector
    #' @export
    indexFromId = function(id = NA, na.rm = FALSE) {
      if (!self$empty) {
        if (!identical(id, NA)) {
          if (length(id) == 1) {
            result <- which(self$info$id == id)
            if (length(result) == 0) {
              return(NA)
            }
          } else {
            result <- purrr::map_int(id, ~ self$indexFromId(id = .x))
          }
          if (na.rm) {
            result <- result[!is.na(result)]
          }
          return(result)
        }
      }
      return(NA)
    },
    #' @description finds the id in the specified dataElement in the object list
    #'
    #' @param index integer vector: the index of the object, which id is to be
    #'  returned
    #' @param na.rm logical vector determines if NA's should be removed from the
    #'  result
    #'
    #' @return NA or integer vector
    #' @export
    idFromIndex = function(index = NA, na.rm = FALSE) {
      if (!identical(index, NA)) {
        if (length(index) == 1) {
          if (!is.na(index)) {
            if ((index > 0) & (index <= self$length)) {
              return(self$info$id[index])
            }
          } else {
            return(NA)
          }
        } else {
          result <- purrr::map_int(index, ~ self$idFromIndex(index = .x))
          if (na.rm) {
            result <- result[!is.na(result)]
          }
          return(result)
        }
      }
      return(NA)
    },
    #' @description a lot of functions using the object will either give an
    #'  index or an id when working with specific items in the dataElements-list.
    #'  If both are provided, the id is used to get the index. If no id is
    #'  provided, then the index parameter is returned.
    #'
    #' @param index default is 1, only returned when id is NA
    #' @param id default is NA, if not, then used preferentially to return the
    #'  index of the object which has it
    #' @param na.rm logical value determining if NA values are to be removed from
    #'  the result. Default is FALSE
    #'
    #' @returns NA or an integer vector
    #' @export
    getIndex = function(index = 1, id = NA, na.rm = FALSE) {
      if (!identical(id, NA)) {
        index <- self$indexFromId(id = id)
      }
      if (na.rm) {
        index <- index[!is.na(index)]
        if (length(index) == 0) {
          index <- NA
        }
      }
      return(index)
    },
    #' @description add dataElement(s) to the dataInfo object
    #'
    #' @param dataElements function or list of dataElement objects to be added
    #'  to the dataInfo object. If it's a function, then it will be executing
    #'  inside the add() function and should result in a list of dataElement
    #'  objects.
    #' @param clone logical value, defines whether the dataElements to be added
    #'  are to be 'cloned'. Default is TRUE to prevent odd situations, option
    #'  may be removed in the future
    #' @param ... to pass on parameters to the parameter data (only if that is a
    #'  function)
    #' @export
    add = function(dataElements = list(), clone = TRUE, ...) {
      if (!identical(dataElements, NA)) {
        if ((length(dataElements) > 0)) {
          if (is.Class(dataElements, "function")) {
            dataElements <- dataElements(...)
          }
          for (counter in 1:length(dataElements)) {
            if (!identical(dataElements[[counter]], NA)) {
              if (is.Class(dataElements[[counter]], "dataElement")) {
                if (clone) {
                  private$data_[[self$length + 1]] <- dataElements[[
                    counter
                  ]]$clone(
                    deep = TRUE
                  )
                } else {
                  private$data_[[self$length + 1]] <- dataElements[[counter]]
                }
                if (dataElements[[counter]]$name != "") {
                  names(private$data_)[self$length] <- dataElements[[
                    counter
                  ]]$name
                }
                private$data_[[self$length]]$id <- as.integer(private$info_)
                private$info_ <- private$info_ + 1L
              }
            }
          }
        }
      }
      invisible(self)
    },
    #' @description deletes a dataElement item from the (internal) list
    #'
    #' @param index integer vector: indexes of the dataElement list to be deleted.
    #'  Ignored if the argument 'id' is specified
    #' @param id id's that will be deleted from the dataElement list.
    #' @export
    delete = function(index = NULL, id = NULL) {
      if (length(id) > 0) {
        index <- unlist(lapply(id, self$indexFromId))
      }
      index <- index |> na.omit() |> as.integer()
      if (length(index) > 0) {
        toDelete <- as.integer()
        for (counter in 1:length(index)) {
          if (!(index[counter] < 1) | !(index[counter] > self$length)) {
            toDelete <- append(toDelete, index[counter])
          }
        }
        if (length(toDelete) > 0) {
          private$data_ <- private$data_[-toDelete]
        }
      }
      invisible((self))
    },
    #' @description retrieves single dataElement from the dataElement list
    #'
    #' @param index index of the item in the dataElement list to be retrieved.
    #'  Ignored if parameter 'id' is specified
    #' @param id id of item that needs to be retrieved from the dataElement list
    #' @export
    item = function(index = NA, id = NA) {
      if ((is.na(index) & is.na(id)) | self$empty) {
        return(NA)
      }
      if (!identical(id, NA)) {
        index <- self$indexFromId(id = id)
      }
      if (identical(index, NA) | (index < 1) | (index > self$length)) {
        return(NA)
      }
      return(self$data[[index]])
    },
    #' @description retrieves a list of elements from the dataElement list
    #'
    #' @param index indexes of the items in the dataElement list to be retrieved.
    #'  Ignored if parameter 'id' is specified
    #' @param id id's of items that need to be retrieved from the dataElement list
    #' @export
    item.list = function(index = 1:self$length, id = NA) {
      if (self$length < 1) {
        return(NA)
      }
      if (!identical(id, NA)) {
        index = purrr::map_int(id, ~ self$indexFromId(.x))
      }
      return(purrr::map(index, ~ self$item(.x)))
    },
    #' @description executes one of the functions 'registered'. in the dataInfo's
    #'  'dos' object
    #' @param whichFunction character vector: name of the function to be executed
    #' @param index of the item(s) in the dataElement list which are to be be
    #'  used by the function. Ignored if parameter 'id' is specified
    #' @param id id of dataElements which are to be used by the function
    #' @param ... for passing on additional parameters to the function to be
    #'  execute
    #'
    #' @note all functions must take the parameters: data which refers to the
    #'  dataInfo itself, index & id to be able to work on the calling dataInfo
    #'  object. Please note that to prevent dataInfo data modification, the
    #'  dataInfo object should be cloned (deep = TRUE).
    #'
    #' @export
    do = function(whichFunction, index = 1, id = NA, ...) {
      if (!identical(self$dos, NA)) {
        if (is.Class(self$dos, "dataFunctions")) {
          return(self$dos$do(whichFunction = whichFunction)(
            data = self,
            index = index,
            id = id,
            ...
          ))
        }
      }
      return(NA)
    }
  ),
  active = list(
    #' @field info gets & sets the info part of the dataElements in the dataInfo
    #'  object
    info = function(value) {
      if (missing(value)) {
        if (self$length > 0) {
          dplyr::bind_rows(purrr::map(private$data_, ~ .x$info))
        } else {
          return(NA)
        }
      } else {
        if (length(self$data) == nrow(value)) {
          for (counter in 1:nrow(value)) {
            private$data_[[counter]]$info <- value %>%
              dplyr::slice(counter)
          }
          return()
        }
        warning("Invalid new info")
      }
    },
    #' @field data gets & sets the data of the objects in the dataElements list
    data = function(value) {
      if (missing(value)) {
        if (length(private$data_) > 0) {
          return(purrr::map(private$data_, ~ .x$data))
        } else {
          return(NA)
        }
      } else {
        if (self$length == length(value)) {
          for (counter in 1:self$length) {
            private$data_[[counter]]$data <- value[[counter]]
          }
        }
        # private$data_ <- value
      }
    },
    #' @field raw provides direct access to the objects in the dataElements list
    raw = function(value) {
      if (missing(value)) {
        if (length(private$data_) > 0) {
          return(private$data_)
        } else {
          return(NA)
        }
      } else {
        private$data_ <- value
      }
    },
    #' @field length returns the number of dataElements in the object (read only).
    length = function(value) {
      if (missing(value)) {
        return(length(private$data_))
      } else {
        # do nothing, read only
      }
    },
    #' @field index set & gets the current value of the (private) index (info_)
    #'  field
    #'
    #' @note for debugging the code & solving issues. Should not be manually set
    #'  w/o (good) reason. May be removed in the future
    index = function(value) {
      if (missing(value)) {
        return(private$info_)
      } else {
        private$info_ <- value
      }
    },
    #' @field ids gets & sets the id's of the dataElements in the dataInfo object
    ids = function(value) {
      if (missing(value)) {
        if (!self$empty) {
          return(self$info$id)
        } else {
          return(NA)
        }
      } else {
        if (!self$empty) {
          self$info$id <- value
        } else {
          warning("Info object is empty")
        }
      }
    },
    #' @field names gets & sets the names of the dataElements in the dataInfo
    #'  object
    names = function(value) {
      if (missing(value)) {
        if (!self$empty) {
          return(purrr::map_chr(self$raw, ~ .x$name))
        } else {
          return(NA)
        }
      } else {
        if (!self$empty) {
          if (length(value) == self$length) {
            for (counter in 1:self$length) {
              self$raw[[counter]]$name <- value[counter]
            }
            return()
          }
        }
        warning("Empty data object or length data object != length names")
      }
    },
    #' @field empty logical vector, determines if the object is (still) empty
    #'  (read only)
    empty = function(value) {
      if (missing(value)) {
        # if either info or data is emptu than object as
        # such is considered empty
        return(self$length == 0)
      } else {
        # nothing, read only
      }
    }
  )
)

# ---- dataList ----

#' @title dataList
#'
#' @description R6 Class to deal with a group of 'dataInfo' objects in an organized manner
#'
#' @examples
#' testList <- dataList$new(
#' name = "Test",
#' dataObjects = list(
#'   dataInfo$new(
#'   name = "cars"
#'   ),
#'   dataInfo$new(
#'     name = "USArrests"
#'   ),
#'   dataInfo$new(
#'     name = "Penguins"
#'   )
#' )
#' )
#' testList
#' testList$item("cars")
#' # add data to 'cars'
#' testList$item("cars")$add(
#'   data = list(dataElement$new(
#'     data = datasets::mtcars,
#'     info = list(source = "datasets")
#'   ))
#' )
#' testList
#' testList$item("cars")
#' # add data again (as example)
#' testList$item("cars")$add(
#'   data = list(dataElement$new(
#'     data = datasets::mtcars,
#'     info = list(source = "datasets", extra = "Once again")
#'   ))
#' )
#' testList
#' testList$item("cars")
#' testList$data$cars
#' testList$data$cars$item(1) |> head()
#' identical(testList$data$cars$item(1), testList$item("cars")$item(id = 2))
#' testList$data$Penguins$add(
#'   data = list(dataElement$new(
#'     data = datasets::penguins,
#'     info = list(source = "datasets")
#'   ))
#' )
#' testList
#' testList$info
#' testList$data$Penguins
#' testList$data$Penguins$data[[1]] |> head()
#' # create a function for some data analysis
#' findMeansetc <- function(
#'     data,
#'     index = 1,
#'     id = NA,
#'     selectedIsland = "Biscoe",
#'     selectedSpecies = "Adelie",
#'     selectedSex = c("female", "male")
#' ) {
#'   data$item(index = index, id = id) |>
#'     dplyr::filter(
#'       island %in% selectedIsland,
#'       species %in% selectedSpecies,
#'       sex %in% selectedSex
#'     ) |>
#'     dplyr::group_by(island, species, sex) |>
#'     dplyr::summarize(
#'       meanBill_len = mean(bill_len, na.rm = TRUE),
#'       sdBill_len = sd(bill_len, na.rm = TRUE),
#'       count = dplyr::n()
#'     ) |>
#'     dplyr::ungroup()
#' }
#' # different ways of applying the function to the data
#' findMeansetc(data = testList$data$Penguins)
#' findMeansetc(
#'   data = testList$data$Penguins,
#'   selectedIsland = c("Biscoe", "Dream"),
#'   selectedSpecies = c("Adelie"),
#'   selectedSex = "male"
#' )
#' # adding the function to the dataList object
#' # (to the 'Penguins' dataInfo object of course!)
#' testList$data$Penguins$dos <- dataFunctions$new(
#'   functions = list(findMeansetc),
#'   names = "gather"
#' )
#' testList$item("Penguins")$dos
#' testList$item("Penguins")$do("gather", id = 1)
#' testList$item("Penguins")$do(
#'   "gather",
#'   id = 1,
#'   selectedIsland = c("Biscoe", "Dream"),
#'   selectedSpecies = c("Adelie"),
#'   selectedSex = "male"
#' )
#' testList$dos
#' testList$do(whichElement = "Penguins", whichFunction = "gather", id = 1)
#' testList$do(
#'   whichElement = "Penguins",
#'   whichFunction = "gather",
#'   id = 1,
#'   selectedIsland = c("Biscoe", "Dream"),
#'   selectedSpecies = c("Adelie"),
#'   selectedSex = "male"
#' )
#'
#' @export
dataList <- R6::R6Class(
  "dataList",
  inherit = dataInfo,
  private = list(),
  # Note that the data_ object is used as a list of the different dataInfo,
  #  while info_ is not used at this time.
  public = list(
    #' @description create a new dataList object
    #'
    #' @param name character vector, name of the dataList object
    #' @param dataObjects list of dataInfo objects to be added to new object
    #' @param clone logical value, defines whether the dataElements to be added
    #'  are to be 'cloned'. Default is TRUE to prevent odd situations, option
    #'  may be removed in the future
    #'
    #' @return a new 'dataList' object
    #' @export
    initialize = function(name = "", dataObjects = NA, clone = TRUE) {
      private$name_ <- name
      private$data_ <- list()
      if (!identical(dataObjects, NA)) {
        for (counter in 1:length(dataObjects)) {
          self$add(dataObject = dataObjects[[counter]], clone = TRUE)
        }
      }
      invisible(self)
    },
    #' @description
    #' For printing purposes: prints the names of the "data" items present
    #'  and the number of data_ items in each
    #'
    #' @param ... no arguments, the function takes care of printing
    #'
    #' @note no arguments, the function takes care of printing
    #' @export
    print = function(...) {
      nameAddChars <- purrr::map_int(self$names, ~ nchar(.x))
      nameAddChars <- max(nameAddChars) - nameAddChars
      if ((self$length > 0)) {
        for (counter in 1:self$length) {
          cat(paste(
            c(
              " ",
              paste(
                c(self$names[counter], rep(" ", times = nameAddChars[counter])),
                collapse = ""
              ),
              "\t: ",
              self$item(counter, clone = FALSE)$length,
              "\n"
            ),
            collapse = ""
          ))
        }
      } else {
        print(NA)
      }
    },
    #' @description executes one of the functions 'registered'. in of the dataInfo
    #'  objects
    #' @param whichElement character vector: name of the dataInfo object in the
    #'  dataList that needs to be used
    #' @param whichFunction character vector: name of the function to be executed.
    #'  The function should be available in the dataInfo object, otherwise NA will
    #'  be returned
    #' @param index of the item(s) in the dataElement list (of the dataInfo object
    #'  to be used) which are to be beused by the function. Ignored if parameter
    #'  'id' is specified
    #' @param id id of the dataElement(s) (in the dataInfo object to be used)
    #'  which is to be used by the function
    #' @param ... for passing on additional parameters to the function to be
    #'  execute
    #'
    #' @note all functions must take the parameters: data which refers to the
    #'  dataInfo object itself, index & id to be able to work on the calling
    #'  dataInfo object. Please note that to prevent dataInfo data modification,
    #'  the dataInfo object should be cloned (deep = TRUE).
    #'
    #' @export
    do = function(whichElement, whichFunction, index = 1, id = NA, ...) {
      if (whichElement %in% names(self$data)) {
        return(self$data[[whichElement]]$do(
          whichFunction = whichFunction,
          index = index,
          id = id,
          ...
        ))
      } else {
        return(NA)
      }
    },
    #' @description to stop calls to inherited functions. Nothing happens,
    #'  simply returns NA
    #'
    #' @param ... no arguments, the function takes care of printing
    #' @export
    indexFromId = function(...) {
      return(NA)
    },
    #' @description to stop calls to inherited functions. Nothing happens,
    #'  simply returns NA
    #'
    #' @param ... no arguments, the function takes care of printing
    #' @export
    idFromIndex = function(...) {
      return(NA)
    },
    #' @description
    #'  adds an dataInfo object to the data_ list
    #'
    #' @param dataObject data object to be added, must be a descendant class
    #'  of 'dataInfo' (or 'dataInfo' class itself). dataInfo object should have
    #'  a name (not enforced at this moment)
    #' @param clone logical value, defines whether the dataElements to be added
    #'  are to be 'cloned'. Default is TRUE to prevent odd situations, option
    #'  may be removed in the future
    #'
    #' @export
    add = function(dataObject, clone = TRUE) {
      if (is.Class(dataObject, "dataInfo")) {
        if (!(dataObject$name %in% self$names)) {
          if (clone) {
            private$data_[[self$length + 1]] <- dataObject$clone(deep = TRUE)
          } else {
            private$data_[[self$length + 1]] <- dataObject
          }
          names(private$data_)[self$length] <- private$data_[[self$length]]$name
        } else {
          if (self$stopOnFail) {
            stop("Object name already exists")
          } else {
            warning("Object not added, name already exists")
          }
        }
      } else {
        if (self$stopOnFail) {
          stop(
            "dataObject needs to be class 'dataInfo' or descendent and have a valid name"
          )
        } else {
          warning(
            "dataObject not added, it needs to be class 'dataInfo' or descendent and have a valid name"
          )
        }
      }
      invisible(self)
    },
    #' @description
    #'  deletes one of the dataInfo items in the dataList object
    #'
    #' @param index number or name of the dataInfo item to be deleted
    #'
    #' @export
    delete = function(index = NA) {
      if (!identical(index, NA)) {
        if (is.character(index)) {
          if (index %in% names(private$data_)) {
            index <- which(index == names(private$data_))
          }
        }
      }
      if (!((index < 1) | (index > self$length))) {
        private$data_ <- private$data_[-index]
      }
      invisible(self)
    },
    #' @description retrieves one of the dataInfo items
    #'
    #' @param index number or name of the dataInfo item to be retrieved
    #' @param clone logical vector specifying whether the returned item
    #'  should be a clone or not. Default = FALSE
    #'
    #' @returns dataInfo item
    #'
    #' @export
    item = function(index = 1, clone = FALSE) {
      if (identical(index, NA)) {
        return(NA)
      }
      if (is.character(index)) {
        if (index %in% names(private$data_)) {
          index <- which(index == names(private$data_))
        }
      }
      if ((index < 1) | (index > self$length)) {
        return(NA)
      } else {
        if (!clone) {
          return(private$data_[[index]])
        } else {
          return(private$data_[[index]]$clone(deep = TRUE))
        }
      }
    },
    #' @description retrieves one or more of the dataInfo items
    #'
    #' @param index numbers or names of the data item to be retrieved
    #' @param clone logical vector specifying whether the returned items
    #'  should be clones or not. Default = FALSE
    #'
    #' @return list of dataInfo items
    #'
    #' @export
    item.list = function(index = 1:self$length, clone = FALSE) {
      if (self$length < 1) {
        return(NA)
      }
      return(lapply(index, function(x) {
        self$item(index = x, clone = clone)
      }))
    }
  ),
  active = list(
    #' @field ids to stop calls to inherited functions. Nothing happens,
    #'  simply returns NA, read only
    ids = function(value) {
      if (missing(value)) {
        return(NA)
      } else {
        # nothing, read only
      }
    },
    #' @field data provides direct access to the list of dataInfo items
    data = function(value) {
      if (missing(value)) {
        return(self$raw)
      } else {
        private$data_ <- value
      }
    },
    #' @field info returns a dataframe showing the names, classes and number of
    #'  items in each dataInfo element in the dataList object
    info = function(value) {
      if (missing(value)) {
        if (!self$empty) {
          return(data.frame(
            name = self$names,
            class = unname(self$classes),
            length = unname(purrr::map_int(self$data, ~ .x$length))
          ))
        } else {
          return(NA)
        }
      } else {
        # nothing, read only
      }
    },
    #' @field names provides direct access to names of the dataInfo items
    #'  in the dataList object
    names = function(value) {
      if (missing(value)) {
        if (self$length > 0) {
          return(unname(purrr::map_chr(private$data_, ~ .x$name)))
        } else {
          return(NA)
        }
      } else {
        if (self$length == length(value)) {
          names(private$data_) <- value
          for (counter in 1:self$length) {
            private$data_[[counter]]$name <- value[counter]
          }
        }
      }
    },
    #' @field dos returns a list of the dataFunction names of each dataInfo
    #'  elementin the dataList object, read only
    dos = function(value) {
      if (missing(value)) {
        return(purrr::map(self$data, ~ .x$dos))
      } else {
        # do nothing, read-only
      }
    },

    #' @field classes returns the (main) class of all dataInfo items in the
    #'  dataList object, read only
    classes = function(value) {
      if (missing(value)) {
        if (self$length > 0) {
          return(unlist(lapply(private$data_, function(x) {
            class(x)[1]
          })))
        } else {
          return(NA)
        }
      } else {
        # nothing
      }
    },
    #' @field classes.full returns the full class vectors of all dataInfo items
    #'  in the dataList object, read only
    classes.full = function(value) {
      if (missing(value)) {
        if (self$length > 0) {
          return(lapply(private$data_, class))
        } else {
          return(NA)
        }
      } else {
        # nothing
      }
    }
  )
)
