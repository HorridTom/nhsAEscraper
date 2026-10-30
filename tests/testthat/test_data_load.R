library(nhsAEscraper)
context("Load A&E data files from disk")

skip_if_no_test_data()

test_that("use load_AE_files to load two files",{
  rawDataList <- load_AE_files(directory = 'test-data/')

  expect_equal(length(rawDataList), 2, info = 'Loaded two files')
  expect_is(rawDataList[[1]], "data.frame")
})
