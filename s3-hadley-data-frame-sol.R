library(checkmate)
library(testthat)

new_data.frame <- function(x, row_num, row.names = NULL) {
  assert_list(x)
  
  # check lengths of every single column and stop if not the same
  if (!all(lengths(x) == row_num)) {
    stop("Different column lengths")
  }
  
  # check if column names of type character and unique
  assert_character(names(x), unique = TRUE)
  
  # check if row names of type character and unique, assign default names if necessary
  if (is.null(row.names)) {
    row.names <- 1:row_num
  }
  else {
    stopifnot(length(row.names) == n)
    assert_character(row.names, unique = TRUE)
  }
  
  structure(x,
            class = "data.frame",
            row.names = row.names)
  
}

# simple tests
x <- list(a = c(1,2), b = c("no", "yes"))

test_that("new_data.frame has basic functionality", {
  expect_is(
    new_data.frame(x, row_num = 2),
    "data.frame"
  )
  expect_identical(
    new_data.frame(x, row_num = 2),
    data.frame(a = c(1,2), b = c("no", "yes"))
  )
  expect_identical(attributes(new_data.frame(x, row_num = 2)),
                   attributes(data.frame(a = c(1,2), b = c("no", "yes")))
  )
})


test_that("new_data.frame deals with errors & failures", {
# input is not a list
expect_error(new_data.frame(c(1,2,3), row_num = 2))

# different column lengths
expect_error(new_data.frame(list(a = 1, b = c(2,3)), row_num = 2))

# too few row names
expect_error(new_data.frame(x, row_num = 2, row.names = c("first")))

# row names not unique
expect_error(new_data.frame(x, row_num = 2, row.names = c("first", "first")))
})
