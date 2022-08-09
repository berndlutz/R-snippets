# Tests some functions of the BData class

# Sample data with observations of observer B, D and M.
d = data.frame(
  id = c(1, 2, 3),
  v1B = c(22, 21, 23),
  v1D = c(22, 23, 24),
  v1M = c(23, 21, 25)
)

# Expected data structure after running the code below
dExpected = data.frame(
  id = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
  observer = c("B", "B", "B", "D", "D", "D", "M", "M", "M"),
  v1 = c(22, 21, 23, 22, 23, 24, 23, 21, 25),
  test = c(2, 3, 4, 2, 3, 4, 2, 3, 4)
)

# Initialization
dtest = BData$new(d)

# Generates 9 out of 3 rows with the already existing values and
# adds a new column "observer" with values "B" for the first 3 lines,
# "D" for line 4-6 and "M" for line 7-9.
dtest$createSubsetsByValues(c("B", "D", "M"), "observer")

# Adds an empty column
dtest$addCol("v1", c())

# Copy values of row 1-3 from cols "v1B", "v1D" and "v1M" to column "v1"
dtest$replaceColValues(dtest$copyColValues("v1B", 1, 3), "v1", 1)
dtest$replaceColValues(dtest$copyColValues("v1D", 1, 3), "v1", 4)
dtest$replaceColValues(dtest$copyColValues("v1M", 1, 3), "v1", 7)

# Deletes not needed columns
dtest$deleteCol(c("v1B", "v1D", "v1M"))

# Prints the data
dtest$data()

# Creates a new column with colsure/callback named "test" with the value of
# the column "id" + 1 of the same row.
dtest$addCol("test", function(df, i) {
  return(df$id[i]+1)
})

# Checks whether the generated data frame matches the expectation above
identical(dExpected, dtest$data())
