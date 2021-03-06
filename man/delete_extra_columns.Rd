% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scrape_AE_data_Eng.R
\name{delete_extra_columns}
\alias{delete_extra_columns}
\title{delete_extra_columns}
\usage{
delete_extra_columns(df, country = "England")
}
\arguments{
\item{df}{a data frame containing A&E provider data
for one month, from either the NHS England or NHS Scotland website}

\item{country}{the country ("England" or "Scotland") whose AE data is
to have columns removed}
}
\value{
df with superfluous columns removed
}
\description{
delete_extra_columns
}
