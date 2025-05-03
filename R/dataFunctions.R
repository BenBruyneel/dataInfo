#' @title emptyFunction
#'
#' @description default function that doesn't do anything. Parameters can be
#'  passed on,but nothing is used. Always returns NA. Essentially a 'fake' function.
#'
#'
#' @param ... can be used to pass on (named) arguments. Doesn't matter what is
#'  passed on, nothing is used.
#'
#' @returns NA
#'
#' @examples
#' emptyFunction()
#'
#' @export
emptyFunction <- function(...) {
  return(NA)
}

#' @title R6 class for holding a set of functions
#'
#' @description
#' Basic R6 class that holds functions, that can be used to dynamically add or
#'  change functions for another object. See manual for full example.
#'
#' @examples
#' dataF <- dataFunctions$new()
#' dataF
#' dataF$functions
#' dataF$names
#' dataF$add(theFunction = mean, theName = "mean")
#' dataF$add(theFunction = median, theName = "median")
#' dataF
#' dataF$do("mean")(datasets::mtcars$mpg)
#' dataF$do("median")(datasets::mtcars$mpg)
#' dataF <- dataFunctions$new(
#'   functions = list(mean, plot),
#'   names = c("mean", "plot")
#' )
#' dataF
#' dataF$do("plot")(datasets::mtcars)
#'
#' @export
dataFunctions <- R6::R6Class(
  "dataFunctions",
  private = list(
    # vector of named list of functions stored in object
    functions_ = list()
  ),
  public = list(
    #' @description initializes the object
    #'
    #' @param functions list of functions to be added upon initialization. Please note
    #'  that the class of list elements is checked!
    #' @param names names to be used for the functions in the 'functions' argument
    initialize = function(functions = list(), names = NA) {
      if (length(functions) > 0) {
        if (length(functions) == length(names)) {
          if (
            sum(purrr::map_lgl(functions, ~ is.Class(.x, "function"))) ==
              length(functions)
          ) {
            private$functions_ <- functions
            names(private$functions_) <- names
          } else {
            stop("Error in functions to be put into object")
          }
        } else {
          stop("Length 'functions' != length 'names'")
        }
      }
      invisible(self)
    },
    #' @description
    #' For printing purposes: prints the names of the contained functions
    #'
    #' @param ... no arguments, the function takes care of printing
    print = function(...) {
      if (!identical(self$names, NA)) {
        print(self$names)
      } else {
        cat(NA)
        cat("\n")
      }
    },
    #' @description adds a function to the object
    #'
    #' @param theFunction the function to be stored in the object
    #' @param theName name with which to call function (does not have to be the name
    #'  of the actual function).
    add = function(theFunction = NA, theName = "") {
      if (is.Class(data, "function")) {
        if (theName != "") {
          private$functions_[[self$length + 1]] <- theFunction
          names(private$functions_)[self$length] <- theName
        }
      }
      invisible(self)
    },
    #' @description retrieves a function 'stored' in the object
    #'
    #' @param whichFunction name of the function to be retrieved
    #'
    #' @note if the whichFunction argument is not present in the names of the
    #'  functions in the object the 'empty function
    do = function(whichFunction = NA) {
      if (!identical(whichFunction, NA)) {
        if (is.Class(whichFunction, "character")) {
          if (whichFunction %in% self$names) {
            return(private$functions_[[which(whichFunction == self$names)]])
          }
        } else {
          if ((whichFunction > 0) & (whichFunction <= self$length))
            return(private$functions_[[whichFunction]])
        }
      }
      return(emptyFunction)
    }
  ),
  active = list(
    #' @field length returns the number of functions present in the object, read only
    length = function(value) {
      if (missing(value)) {
        return(length(private$functions_))
      } else {
        # do nothing, read only
      }
    },
    #' @field functions gets & sets the (list of) functions present in the object.
    #'  If data is put into the object it is checked (whether it is a function).
    functions = function(value) {
      if (missing(value)) {
        if (self$length == 0) {
          return(NA)
        } else {
          return(private$functions_)
        }
      } else {
        if (!identical(value, NA)) {
          if (is.Class(value, "list")) {
            if (
              sum(purrr::map_lgl(value, ~ is.Class(.x, "function"))) ==
                length(value)
            ) {
              private$functions_ <- value
              return()
            }
          }
          stop("Error in data to be put into object")
        } else {
          private$functions_ <- list()
        }
      }
    },
    #' @field names gets & sets the names of all the functions
    names = function(value) {
      if (missing(value)) {
        if (self$length > 0) {
          return(names(private$functions_))
        } else {
          return(NA)
        }
      } else {
        if (!identical(value, NA)) {
          if (is.Class(value, "character")) {
            if (length(value) == length(private$functions_)) {
              names(private$functions_) <- value
              return()
            }
          }
        }
        stop("Error in info to be put into object")
      }
    }
  )
)
