# The tests that read A&E files use two NHS England files in test-data/:
# August-2015-AE-by-provider-783w5.xls, and an earlier version of the
# January 2018 file, January-2018-AE-by-provider-BFNU4-v2.xls, whose layout
# has the extra "less than 4 hours" columns that delete_extra_columns()
# removes. These files are not kept in the repository. The August 2015 file can
# still be downloaded from NHS England; this version of the January 2018 file is
# no longer published, so copies are kept by the maintainer.
#
# skip_if_no_test_data() skips the rest of a test file when either file is
# missing, so that the tests show as skipped rather than failed.

test_data_files <- c("August-2015-AE-by-provider-783w5.xls",
                     "January-2018-AE-by-provider-BFNU4-v2.xls")

skip_if_no_test_data <- function() {
  missing_files <- test_data_files[!file.exists(file.path("test-data", test_data_files))]
  if (length(missing_files) > 0) {
    skip(paste0("NHS England test files not found in tests/testthat/test-data/: ",
                paste(missing_files, collapse = ", ")))
  }
}
