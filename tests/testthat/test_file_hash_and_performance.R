library(nhsAEscraper)
context("File hashes and performance columns")

test_that("hashSourceFileContents is the md5 hash of the file's contents", {
  f <- tempfile(fileext = ".csv")
  writeLines("a,b\n1,2", f)
  expect_equal(nhsAEscraper:::hash_file_contents(f),
               unname(tools::md5sum(f)))

  # Same name, different contents gives a different hash
  h1 <- nhsAEscraper:::hash_file_contents(f)
  writeLines("a,b\n1,3", f)
  expect_false(identical(h1, nhsAEscraper:::hash_file_contents(f)))
})

skip_if_no_test_data()

test_that("Perf_Typ1 and Perf_All are calculated from the counts", {
  AE_data <- getAE_data(update_data = FALSE, directory = 'test-data/')

  expect_equal(AE_data$Perf_Typ1,
               ifelse(AE_data$Att_Typ1 > 0,
                      (AE_data$Att_Typ1 - AE_data$Att_Typ1_Br) / AE_data$Att_Typ1,
                      NA_real_))
  expect_equal(AE_data$Perf_All,
               ifelse(AE_data$Att_All > 0,
                      (AE_data$Att_All - AE_data$Att_All_Br) / AE_data$Att_All,
                      NA_real_))
})
