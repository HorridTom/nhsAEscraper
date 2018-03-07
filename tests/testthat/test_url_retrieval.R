library(nhsAEscraper)
context("Retrieve urls of A&E data files")

test_that("getAEdata_urls_monthly returns character vector",{
  urls_ae <- getAEdata_urls_monthly()
  expect_is(urls_ae, "character")
})
