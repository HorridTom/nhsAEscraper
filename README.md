
<!-- README.md is generated from README.Rmd. Please edit that file -->
nhsAEscraper
============

NHS England [publishes](https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/) monthly aggregated data on A&E attendance numbers and 4 hour target performance by provider organisation, in the form of MS Excel workbooks. This format does not facilitate statistical analysis, especially over time. This package scrapes the data and organises it into a coherent structure.

The package was developed by the [National Institute for Health Research Collaboration for Leadership in Applied Health Research and Care Northwest London (NIHR CLAHRC NWL)](http://clahrc-northwestlondon.nihr.ac.uk/).

Installation
------------

You can install nhsAEscraper from github with:

``` r
# install.packages("devtools")
devtools::install_github("HorridTom/nhsAEscraper")
```

Scraping the data
-----------------

To download all the data and assemble it into one data frame:

``` r
library(nhsAEscraper)
AE_data <- getAE_data()
str(AE_data)
```

Method
------

The `getAE_data` function goes through the following steps to achieve the end result.

### 1. Create directory to store Excel files

The package works by downloading the monthly Excel files to a local directory, then loading them in, processing them and joining them together. The `directory` argument of `getAE_data` specifies this directory as a file path, which is created (if it does not already exist) within the working directory when `getAE_data` is executed. The default directory is `'/nhsAEscraper/sitreps'`.

### 2. Download Excel files

If the `update_data` argument is `TRUE`, the urls of files to be downloaded are obtained by the `getAEdata_urls_monthly` function. This function takes as argument `url_list` a list of web pages, each of which it searches for html that identifies the Excel data files. If this argument is null, the default is to search all the pages available when the package was last updated.

The Excel data files identified are then downloaded and saved in the specified `directory`.

### 3. Load Excel files

The saved Excel files are loaded in from the `directory` by the `load_AE_files` function. Note here that the package can use two different approaches to identify the month that each file pertains to: i) The month specified in the filename ii) the month specified in a cell within the Excel workbook. Which approach is used is specified by the `use_filename_date` boolean argument to `getAE_data` which is passed through to `load_AE_files`. The default is `FALSE`, i.e. to use the date from within the workbook.

### 4. Standardise formats and check data

Next the individual months' data are checked for known variations in format and standardised to a uniform format, then checked to ensure the results match the standard format. If this fails, the error `'There is a problem with the format of the data in one or more of the files'` will be thrown and execution will terminate. The only way to work around this at present is to remove the offending file from the `directory` before loading, or to manually address the problem between steps (2) and (3) above.

The function `check_format` can be run on one month's data (as `raw_data`) to check it's format is correct, with the `verbose` argument controlling how much detail is returned.

### 5. Clean and join datasets

Finally the monthly data are cleaned, and joined together to from one data frame containing data for all available months and all available providers. This is then returned by `getAE_data`.

License
-------

This package is open source under the Apache License Version 2.0, January 2004
