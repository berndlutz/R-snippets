#
# Class for simplified work with R data frames, e.g.
# - data structure modification for e.g. dummy encoding
# - calculation of new columns on the fly
# - ...
#

library("R6")

BData <- R6Class(
  "BData",

  public = list(

    # Initialization
    # Parameter:
    # df: R data frame
    initialize = function(df) {
      private$df = df
    },

    # Multiplies data by given value, e.g. for dummy encoding
    # Parameters:
    # values: vector with values, e.g. for observer A, B, C: c("A", "B", "C")
    # valueColName: name of column in which values are present
    createSubsetsByValues = function(values, valueColName) {
      len = length(private$df[[names(private$df)[1]]])
      i = 2
      self$addCol(valueColName, rep(values[1], len))
      dfCopy <- private$df
      while (i <= length(values)) {
        private$df = rbind(private$df, dfCopy)
        self$replaceColValues(rep(values[i], len), valueColName, (len*(i-1)+1))
        i = i + 1
      }
    },

    # Replace values in a column, e.g. for dummy encoding
    replaceColValues = function(values, toColName, toStartRow) {
      i = 0
      for (val in values) {
        private$df[[toColName]][toStartRow+i] = val
        i = i + 1
      }
    },

    # Copies values of an column, e.g. for dummy encoding together with replaceColValues()
    copyColValues = function(colName, start, length) {
      end = start + length - 1
      return(private$df[[colName]][start:end])
    },

    # Creates a new column with a given closure or vector
    # Example use: Generation of a new column based on existing values or simple adding a column based on a vector.
    # Parameters:
    # name: name of new column
    # input: function or vector, e.f. function(df, i){return(df$a[i] + df$b[i])}
    addCol = function(name, input) {
      # by provided data
      if (is.vector(input)) {
        if (self$colExists(name)) {
          return(FALSE)
        }
        else {
          private$df[name] = input
          return(TRUE)
        }
      }
      # by computation via closure
      else if (typeof(input) == "closure") {
        print("......................... #####################")
        i = 1
        rows = self$nrow()
        col = c()
        while (i <= rows) {
          col = append(col, input(self$data(), i))
          i = i + 1
        }

        private$df[name] = col
        return(TRUE)
      }
      else {
        return(FALSE)
      }
    },

    # Deletes a column
    deleteCol = function(names) {
      for (name in names) {
        private$df[name] <- NULL
      }
    },

    # Checks whether a column exists or not
    colExists = function(name) {
      return(name %in% names(private$df))
    },

    # Number of rows in the data frame
    nrow = function() {
      return(nrow(private$df))
    },

    # Returns the R data frame stored in the class
    data = function() {
      return(private$df)
    },

    # Saves the data frame to a files as .csv file, e.g. after generation of a dummy encoding
    save = function(file) {
      write.csv(
        private$df,
        file,
        append = FALSE,
        sep = ";",
        col.names = TRUE,
        row.names = FALSE,
        fileEncoding = "UTF-8"
      )
    }
  ),

  private = list(
    df = FALSE
  ),
)
