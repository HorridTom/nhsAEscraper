#' getAE_data
#'
#' @param update_data whether to download files afresh from NHS England website (TRUE)
#' or use existing downloaded files (FALSE)
#' @param directory directory to find existing downloaded files, and to save new downloads
#' @param url_list list of urls (as strings) for the pages to scrape for data files
#' @param use_filename_date if TRUE, take dates from the Excel file's name, if FALSE,
#' take from the date specified inside the sheet
#'
#' @return A data frame containing all the monthly A&E data from the NHS England website.
#' @export
#'
#' @examples
#' AE_data <- getAE_data(directory = file.path('nhsAEscraper','sitreps'))
#' str(AE_data)
getAE_data <- function(update_data = TRUE, directory = file.path('data-raw','sitreps'),
                       url_list = NULL, use_filename_date = FALSE) {

  dir.create(directory, showWarnings = FALSE, recursive = TRUE)

  if(update_data) {
    urls <- getAEdata_urls_monthly(url_list = url_list)
    download_AE_files(urls, directory = directory)
  }
  rawDataList <- load_AE_files(directory = directory, use_filename_date = use_filename_date)

  rawDataList <- lapply(rawDataList, delete_extra_columns)

  if(!all(unlist(lapply(rawDataList, check_format)))) {
    stop('There is a problem with the format of the data in one or more of the files')
  }

  cleanDataList <- lapply(rawDataList, clean_AE_data)

  AE_data <- dplyr::bind_rows(cleanDataList)

  AE_data

}


#' getAEdata_urls_monthly
#'
#' @param url_list list of urls (as strings) for the pages to scrape for data files
#'
#' @return the urls for NHS England A&E data *.xls files from pages in url_list
#' yielding addresses for monthly data from June 2015 to (in principle) present.
#' @export
#'
#' @examples
#' urls <- getAEdata_urls_monthly()
#' head(urls, n = 3)
getAEdata_urls_monthly <- function(url_list = NULL) {

  if(is.null(url_list)) {
    url_15_16 <- "https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/statistical-work-areasae-waiting-times-and-activityae-attendances-and-emergency-admissions-2015-16-monthly-3/"
    url_16_17 <- "https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/statistical-work-areasae-waiting-times-and-activityae-attendances-and-emergency-admissions-2016-17/"
    url_17_18 <- "https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/ae-attendances-and-emergency-admissions-2017-18/"
    url_18_19 <- "https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/ae-attendances-and-emergency-admissions-2018-19/"

    url_list <- list(url_15_16, url_16_17, url_17_18, url_18_19)
  }

  unlist(lapply(url_list,function(x) getAEdata_page_urls_monthly(x)))

}


#' getAEdata_page_urls_monthly
#'
#' @param index_url the url of the page to scrape data files from
#'
#' @return character vector of the urls for NHS England A&E data *.xls files
#' from one of the monthly data index pages
#'
#' @export
#'
#' @examples
#' urls <- getAEdata_page_urls_monthly(paste0('https://www.england.nhs.uk/statistics/',
#' 'statistical-work-areas/ae-waiting-times-and-activity/',
#' 'ae-attendances-and-emergency-admissions-2017-18/'))
#' head(urls, n = 3)
getAEdata_page_urls_monthly <- function(index_url) {

  #Get the html from the index website
  con <- url(index_url, "r")
  html_lines <- readLines(con)

  #Close connection
  close(con)

  #Look for lines that contain the signature part of the url and the signature text
  data_url_lines <- grep("^(?=.*xls)((?!Quarter).)*$",html_lines, perl=TRUE)
  xlsdata_url_lines <- grep("AE-by-provider",html_lines[data_url_lines])
  NHSE_xlsdata_lines <- html_lines[data_url_lines][xlsdata_url_lines]

  #Extract urls from html lines
  starts <- regexpr("http",NHSE_xlsdata_lines)
  ends <- regexpr(".xls",NHSE_xlsdata_lines) + 3
  urls <- substr(NHSE_xlsdata_lines, starts, ends)

  #Return urls
  return(urls)

}


#' download_AE_files
#'
#' @param file_urls list of urls of files to download
#' @param directory location to save files to
#'
#' @return vector of download.file return values
#' @export
#'
#' @examples
#' urls <- getAEdata_page_urls_monthly(paste0('https://www.england.nhs.uk/statistics/',
#' 'statistical-work-areas/ae-waiting-times-and-activity/',
#' 'ae-attendances-and-emergency-admissions-2017-18/'))
#' download_AE_files(urls[1], directory = file.path('nhsAEscraper','sitreps'))
download_AE_files <- function(file_urls, directory) {

  file.remove(
    dir(directory,
        pattern = "*",
        full.names = TRUE)
  )

  f_name_regex <- '/([^/]+)$'

  lapply(file_urls, function(x) {
    fn <- file.path(directory, stringr::str_match(x, f_name_regex)[,2])
    httr::GET(x, httr::write_disk(fn, overwrite = TRUE))
  })

}


#' load_AE_files
#'
#' @param directory path of the directory to load files from
#'
#' @return a list of data frames containing data loaded from files in directory
#' whose name is of the form '\*AE-by-provider\*.xls'
#' @export
#'
#' @examples
#' dataList <- load_AE_files(directory = file.path('nhsAEscraper','sitreps'))
#'
load_AE_files <- function(directory = file.path('data-raw','sitreps'), use_filename_date = TRUE) {

  fileNames <- Sys.glob(file.path(directory,'*AE-by-provider*.xls'))
  dataList <- NULL
  dataList <- lapply(fileNames, function(x) {
    cat(file=stderr(), "Loading: ", x, "\n")
    df <- readxl::read_excel(x, sheet = 1, col_names = FALSE)
    cat(file=stderr(), "Success loaded: ", x, "\n")
    if(use_filename_date) {
      dt_chr <- stringr::str_replace(
        stringr::str_match(x, '/(([0-9A-Za-z]|-)*)-AE-by-provider')[,2], '-', ' '
        )
      df <- df %>% dplyr::mutate(X__2 = ifelse(X__1 == 'Period:', dt_chr, X__2))
    }
    df
    })
  dataList
}


# Tell codetools not to worry about no visible binding for default imported data column names
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("X__1", "X__2", "X__3", "X__4", "X__5", "X__6", "X__7", "X__8",
                           "X__9", "X__10", "X__11", "X__12", "X__13", "X__14", "X__15", "X__16",
                           "X__17", "X__18", "X__19", "X__20", "X__21"))
}


#' clean_AE_data
#'
#' @param raw_data dataframe containing a NHS England A&E Monthly report
#' with a standardised set of columns
#'
#' @return the same data as raw_data, as a rectangular table with header removed,
#' new column names, and correct numerical data types for numerical columns
#' @importFrom magrittr %>%
#'
#'
clean_AE_data <- function(raw_data) {

  data_date <- get_date(raw_data)

  clean_data <- raw_data %>% dplyr::filter(grepl("^[A-Z0-9]+$",X__1))

  clean_data <- clean_data %>% dplyr::select(X__1:X__21) %>%
    dplyr::rename(Prov_Code = X__1,
                                    Region = X__2,
                                    Prov_Name = X__3,
                                    Att_Typ1 = X__4,
                                    Att_Typ2 = X__5,
                                    Att_Typ3 = X__6,
                                    Att_All = X__7,
                                    Att_Typ1_Br = X__8,
                                    Att_Typ2_Br = X__9,
                                    Att_Typ3_Br = X__10,
                                    Att_All_Br = X__11,
                                    Perf_Typ1 = X__12,
                                    Perf_All = X__13,
                                    E_Adm_Typ1 = X__14,
                                    E_Adm_Typ2 = X__15,
                                    E_Adm_Typ34 = X__16,
                                    E_Adm_All_ED = X__17,
                                    E_Adm_Not_ED = X__18,
                                    E_Adm_All = X__19,
                                    E_Adm_4hBr_D = X__20,
                                    E_Adm_12hBr_D = X__21)


  # Explicitly replace Excel 'N/A' with NA_character_
  clean_data <- clean_data %>%
    dplyr::mutate(Perf_Typ1 = dplyr::case_when(Perf_Typ1 == 'N/A' ~ NA_character_,
                                               Perf_Typ1 == '-' ~ NA_character_,
                                               Perf_Typ1 != 'N/A' ~ Perf_Typ1))
  clean_data <- clean_data %>%
    dplyr::mutate(Perf_All = dplyr::case_when(Perf_Typ1 == 'N/A' ~ NA_character_,
                                              Perf_Typ1 == '-' ~ NA_character_,
                                              Perf_Typ1 != 'N/A' ~ Perf_Typ1))

  clean_data <- clean_data %>% dplyr::mutate_at(dplyr::vars(dplyr::starts_with("Att_")), dplyr::funs(as.numeric)) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with("Perf_")), dplyr::funs(as.numeric)) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with("E_Adm_")), dplyr::funs(as.numeric))

  clean_data <- clean_data %>% tibble::add_column(Month_Start = data_date, .after = 3)

  clean_data
}


#' check_format
#'
#' @param raw_data a data frame containing A&E provider data
#' for one month, from the NHS England website
#' @param verbose control level of detail returned
#'
#' @return boolean indicating whether data frame is in correct format
#' for analysis. Length 1 if verbose = FALSE, length 6 if not - in this case
#' each element pertains to a specific aspect of raw_data that is checked, respectively:
#' 1 - column 1 contains the heading Code
#' 2 - column 2 contains the heading Region
#' 3 - column 3 contains the heading Name
#' 4 - column 4 contains the heading A&E attendances
#' 5 - column 8 contains the heading A&E attendances > 4 hours from arrival to admission
#' 6 - column 14 contains the heading Emergency Admissions
#' Note that the 5th element allows the words greater than as well as the symbol.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   dataList <- load_AE_files(directory = 'nhsAEscraper/sitreps/')
#'   check_format(dataList[[1]], verbose = TRUE)
#' }
check_format <- function(raw_data, verbose = FALSE) {

  format_status <- logical()

  format_status[1] <- nrow(raw_data %>% dplyr::filter(X__1 == "Code")) == 1
  format_status[2] <- nrow(raw_data %>% dplyr::filter(X__2 == "Region")) == 1
  format_status[3] <- nrow(raw_data %>% dplyr::filter(X__3 == "Name")) == 1
  format_status[4] <- nrow(raw_data %>% dplyr::filter(X__4 == "A&E attendances")) == 1
  format_status[5] <- nrow(raw_data %>% dplyr::filter(grepl("A&E attendances > 4 hours from arrival to admission",X__8)|grepl("A&E attendances greater than 4 hours from arrival to admission",X__8))) == 1
  format_status[6] <- nrow(raw_data %>% dplyr::filter(X__14 == "Emergency Admissions")) == 1


  if (verbose) {
    format_status
  } else {
      all(format_status)
    }
}


#' get_date
#'
#' @param raw_data a data frame containing A&E provider data
#' for one month, from the NHS England website
#'
#' @return the period (month) that this data pertains to
#' @export
#'
#' @examples
#' dataList <- load_AE_files(directory = 'nhsAEscraper/sitreps/')
#' get_date(dataList[[1]])
#'
get_date <- function(raw_data) {
  #Find the cell specifying the period and extract the text
  date_chr <- raw_data %>% dplyr::filter(X__1 == "Period:") %>%
    dplyr::pull(X__2)
  lubridate::myd(paste(date_chr,'1st',sep=' '), tz = "Europe/London")

}


#' delete_extra_columns
#'
#' @param df a data frame containing A&E provider data
#' for one month, from the NHS England website
#'
#' @return df with superfluous columns removed
#'
delete_extra_columns <- function(df) {
  format_type_x <- nrow(df %>% dplyr::filter(grepl("A&E attendances less than 4 hours from arrival to admission",X__8))) == 1
  if(!format_type_x) return(df)
  df <- df %>% dplyr::select(-c(X__8,X__9,X__10,X__11))
  colnames(df) <- paste('X__',c(1:ncol(df)),sep='')
  df
}
