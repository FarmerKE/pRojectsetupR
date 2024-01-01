# tests/testthat/test_dir_file_functions.R

# Load required packages
library(testthat)
library(pRojectsetupR)  # Replace with the actual name of your package

test_that("dir_create_function creates directories correctly", {
  # Create a temporary directory for testing
  test_dir <- tempfile()
  
  # Test creating a new directory
  result1 <- dir_create_function(test_dir)
  expect_equal(result1$success, glue::glue("{test_dir} Directory Created Successfully"))
  
  # Test attempting to create an existing directory
  result2 <- dir_create_function(test_dir)
  expect_equal(result2$error, "Directory Exists")
  
  # Clean up - delete the temporary directory
  unlink(test_dir, recursive = TRUE)
})

test_that("file_create_function creates files correctly", {
  # Create a temporary directory for testing
  test_dir <- tempfile()
  
  # Test creating a new file
  result1 <- file_create_function("test_file.txt", dest = test_dir)
  expect_equal(result1$success, glue::glue("test_file.txt File Created Successfully"))
  
  # Test attempting to create an existing file
  result2 <- file_create_function("test_file.txt", dest = test_dir)
  expect_equal(result2$error, "File Exists")
  
  # Clean up - delete the temporary directory
  unlink(test_dir, recursive = TRUE)
})

# Add more tests for other functions if needed

# Run the tests
test_dir("path/to/yourpackage/tests")
