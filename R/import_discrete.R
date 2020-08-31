#' Import discrete bottle data from file
#'
#' Imports discrete bottle data from a .csv file. The file format is expected
#'     to be the one produced by \code{download_discrete()}. If you're having
#'     issues loading data, it may be because you have edited the file in
#'     Excel. Try downloading a clean .csv file and not opening it in Excel
#'     prior to loading in R.
#'
#' @usage import_discrete(fname)
#'
#' @param fname The filename to import, as a string
#'
#' @return A tibble with imported data and clean column names
#'
#' @export
#'
#' @importFrom readr read_tsv col_datetime col_double col_integer col_character col_skip cols
#'
#' @importFrom dplyr rename
#'
#' @importFrom lubridate year month day
#'
#' @import tibble
#'
#' @examples
#'
#' file <- system.file("extdata", "test_discrete_data.csv", package = "kcmarine")
#' data <- import_discrete(file)

import_discrete <- function(fname) {
  options(warn=-1)
  temp = readr::read_csv(fname, col_names = TRUE,
                         col_types = readr::cols(
                           SampleId = readr::col_integer(),
                           ParmId = readr::col_integer(),
                           ParmDisplayName = readr::col_character(),
                           Value = readr::col_double(),
                           OverrideValue = readr::col_double(),
                           Units = readr::col_character(),
                           Mdl = readr::col_double(),
                           Rdl = readr::col_double(),
                           QualityId = readr::col_integer(),
                           QfrCode = readr::col_character(),
                           TextValue = readr::col_character(),
                           WorkNum = readr::col_character(),
                           CollectDate = readr::col_character(),
                           CollectDateTime = readr::col_character(),
                           LabSampleNum = readr::col_character(),
                           Depth = readr::col_double(),
                           SampleParmsReleased = readr::col_logical(),
                           IsMarine = readr::col_logical(),
                           Locator = readr::col_character(),
                           SiteId = readr::col_integer(),
                           SiteTypeId = col_integer(),
                           LimsProjectNum = col_character()
                         ))
  options(warn=1)

  CollectDate <- strptime(temp$CollectDate, "%Y-%m-%d")
  if (is.na(CollectDate[1])) {
    CollectDate <- strptime(temp$CollectDate, "%m/%d/%Y")
  }
  temp$CollectDate <- CollectDate

  CollectDateTime <- as.POSIXct(temp$CollectDateTime,
                                format = "%Y-%m-%dT%H:%M:%SZ",
                                tz = "America/Los_Angeles")
  temp$CollectDateTime <- CollectDateTime

  return(temp)
}
