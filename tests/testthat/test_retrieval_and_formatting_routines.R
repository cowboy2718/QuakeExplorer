library(QuakeExplorer)
library(readr)
library(dplyr)
library(lubridate)
context("importing and formatting data functions")
results <- read_delim('results.csv', delim = '\t')


test_that("Import function returns a dataframe",{
  results<-import_quake_data("results.csv")
  expect_is(results,"tbl_df")
  
})

test_that("Data formatting works",{
  test_set <- eq_clean_data(results)
  expect_is(test_set, "tbl_df")
  expect_is(test_set$LATITUDE, "numeric")
  expect_is(test_set$LONGITUDE, "numeric")
})

test_that("letter formatting works",{
  expect_equal(simpleCap("North korea"),
               "North Korea")
})