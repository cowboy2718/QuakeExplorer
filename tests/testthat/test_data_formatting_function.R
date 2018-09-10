context("Test basic cleaning functionality")

library(QuakeExplorer)
library(ggplot2)
library(dplyr)


results <- read_delim("~/Data/Statistical Projects/R Projects/QuakeExplorer/tests/testthat/results.csv", "\t", escape_double = FALSE, trim_ws = TRUE)

any_name_df<-eq_clean_data(results)

test_that("data is being formatted",{
  test_data_set <- eq_clean_data(any_name_df)
  expect_is(test_data_set, "tbl_df")
  expect_is(test_data_set$datevalue, "Date")
  expect_is(test_data_set$LATITUDE, "numeric")
  expect_is(test_data_set$LONGITUDE, "numeric")
  expect_is(test_data_set$EQ_PRIMARY, "numeric")
  expect_equal(length(grep(":", test_data_set$location_name)),0)
})