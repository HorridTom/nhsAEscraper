library(nhsAEscraper)
context("Retrieve urls of A&E data files")


# Offline tests: these read a saved example page, test-data/ae-page-example.html.

example_page <- paste0("file://", normalizePath("test-data/ae-page-example.html"))

test_that("getAEdata_page_urls_monthly finds the monthly provider files on a page",{
  urls <- getAEdata_page_urls_monthly(example_page)

  expect_equal(urls,
               c("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2016/06/April-2016-AE-by-provider-1Ez1q.xls",
                 "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2016/07/May-2016-AE-by-provider-7Ov6K.xls",
                 "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/June-2025-AE-by-provider-3euC4-1.xlsx"))
})

test_that("getAEdata_urls_monthly adds the revised 2019 files only for the 2019-20 page",{
  revised_2019 <- c("Monthly-October-2019-revised-210720-qm5hG.xls",
                    "Monthly-September-2019-revised-210720-L48uy.xls")

  urls <- getAEdata_urls_monthly(list(example_page))
  expect_false(any(basename(urls) %in% revised_2019))

  page_2019_20 <- file.path(tempdir(), "ae-attendances-and-emergency-admissions-2019-20.html")
  file.copy(normalizePath("test-data/ae-page-example.html"), page_2019_20, overwrite = TRUE)
  urls <- getAEdata_urls_monthly(list(example_page, paste0("file://", page_2019_20)))
  expect_equal(sum(basename(urls) %in% revised_2019), 2)
  expect_equal(anyDuplicated(urls), 0)
})


# Live tests: these read NHS England's website, so they check that the website
# still works with the package, as well as the package itself. They are skipped
# without an internet connection and on CRAN.

test_that("the live 2016-17 page gives its 12 monthly files",{
  skip_if_offline()
  url_16_17 <- paste0("https://www.england.nhs.uk/statistics/statistical-work-areas/",
                      "ae-waiting-times-and-activity/statistical-work-areasae-waiting",
                      "-times-and-activityae-attendances-and-emergency-admissions-2016-17/")

  AE_data_urls_16_17 <- getAEdata_urls_monthly(list(url_16_17))
  expect_equal(length(AE_data_urls_16_17), 12, info = '12 data file urls on 2016-2017 page')
})

test_that("the live pages give file urls, with the revised 2019 files once",{
  skip_if_offline()
  urls_ae <- getAEdata_urls_monthly()

  expect_is(urls_ae, "character")
  expect_true(all(grepl('^https://www.england.nhs.uk/statistics/wp-content/uploads/sites/', urls_ae)))
  expect_equal(anyDuplicated(urls_ae), 0)
  expect_equal(sum(grepl("Monthly-(September|October)-2019-revised", urls_ae)), 2)
})
