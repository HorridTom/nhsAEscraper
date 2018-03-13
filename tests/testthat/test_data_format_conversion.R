library(nhsAEscraper)
context("Convert imported data to a standard format")

test_that("Superfluous columns correctly deleted",{
  rawDataList <- load_AE_files(directory = 'test-data/')
  rawDataList_conv <- lapply(rawDataList, delete_extra_columns)

  comparison_1 <- compare::compare(rawDataList[[1]], rawDataList_conv[[1]])
  comparison_2 <- compare::compare(rawDataList[[2]], rawDataList_conv[[2]])

  expect_equal(comparison_1$result, TRUE, info = 'does not mess up data with correct columns')
  expect_equal(comparison_2$result, FALSE, info = 'does change data with incorrect columns')

  expect_match(rawDataList_conv[[1]][[1]], "Code", all = FALSE, info = 'correct first column heading present (still correct)')
  expect_match(rawDataList_conv[[2]][[1]], "Code", all = FALSE, info = 'correct first column heading present (after change)')
  expect_match(rawDataList_conv[[1]][[21]], "Number of patients spending >12 hours", all = FALSE, info = 'correct last column heading present (still correct)')
  expect_match(rawDataList_conv[[2]][[21]], "Number of patients spending >12 hours", all = FALSE, info = 'correct last column heading present (after change)')

})
