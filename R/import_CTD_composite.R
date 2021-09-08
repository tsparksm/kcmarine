#' Import CTD composite data from file
#'
#' Imports CTD data composite from a tab-delimited .txt file or
#'     a .csv file. The format imported by this script is the binned
#'     data composite stored in the shared drive, either the "*_all"
#'     or "*_monthly" files with any bin width. Currently this data
#'     is only available in the internal CTD data repository. These
#'     data do not include any "bad" or "questionable" data.
#'
#' @usage import_CTD_composite(fname)
#'
#' @param fname The filename to import, as a string
#'
#' @return A tibble with imported data and clean column names
#'
#' @export
#'
#' @importFrom lubridate year month day
#'
#' @importFrom stringr str_detect
#'
#' @importFrom readr read_tsv read_csv cols col_character col_double
#'
#' @importFrom dplyr rename
#'
#' @import tibble
#'
#' @examples
#'
#' file <- system.file("extdata", "test_CTD_composite_data.csv", package = "kcmarine")
#' data <- import_CTD_composite(file)

import_CTD_composite <- function(fname) {
  if (stringr::str_detect(fname, ".txt")) {
    CTD_data <- readr::read_tsv(fname,
                         col_types = readr::cols(
                           Locator = readr::col_character(),
                           Date = readr::col_date(),
                           Year = readr::col_integer(),
                           Month = readr::col_integer(),
                           Depth = readr::col_double(),
                           Bin = readr::col_character(),
                           BinDepth = readr::col_double(),
                           Chlorophyll = readr::col_double(),
                           Density = readr::col_double(),
                           DO = readr::col_double(),
                           SigmaTheta = readr::col_double(),
                           Light_Transmission = readr::col_double(),
                           PAR = readr::col_double(),
                           Surface_PAR = readr::col_double(),
                           Salinity = readr::col_double(),
                           Temperature = readr::col_double(),
                           Turbidity = readr::col_double(),
                           NO23 = readr::col_double(),
                           Chlorophyll_Anomaly = readr::col_double(),
                           Density_Anomaly = readr::col_double(),
                           DO_Anomaly = readr::col_double(),
                           SigmaTheta_Anomaly = readr::col_double(),
                           Light_Transmission_Anomaly = readr::col_double(),
                           PAR_Anomaly = readr::col_double(),
                           Surface_PAR_Anomaly = readr::col_double(),
                           Salinity_Anomaly = readr::col_double(),
                           Temperature_Anomaly = readr::col_double(),
                           Turbidity_Anomaly = readr::col_double(),
                           NO23_Anomaly = readr::col_double()
                         ))
  } else if (stringr::str_detect(fname, ".csv")) {
    CTD_data <- readr::read_csv(fname,
                                col_types = readr::cols(
                                  Locator = readr::col_character(),
                                  Date = readr::col_date(),
                                  Year = readr::col_integer(),
                                  Month = readr::col_integer(),
                                  Depth = readr::col_double(),
                                  Bin = readr::col_character(),
                                  BinDepth = readr::col_double(),
                                  Chlorophyll = readr::col_double(),
                                  Density = readr::col_double(),
                                  DO = readr::col_double(),
                                  SigmaTheta = readr::col_double(),
                                  Light_Transmission = readr::col_double(),
                                  PAR = readr::col_double(),
                                  Surface_PAR = readr::col_double(),
                                  Salinity = readr::col_double(),
                                  Temperature = readr::col_double(),
                                  Turbidity = readr::col_double(),
                                  NO23 = readr::col_double(),
                                  Chlorophyll_Anomaly = readr::col_double(),
                                  Density_Anomaly = readr::col_double(),
                                  DO_Anomaly = readr::col_double(),
                                  SigmaTheta_Anomaly = readr::col_double(),
                                  Light_Transmission_Anomaly = readr::col_double(),
                                  PAR_Anomaly = readr::col_double(),
                                  Surface_PAR_Anomaly = readr::col_double(),
                                  Salinity_Anomaly = readr::col_double(),
                                  Temperature_Anomaly = readr::col_double(),
                                  Turbidity_Anomaly = readr::col_double(),
                                  NO23_Anomaly = readr::col_double()
                                ))
  } else {
    stop("Check CTD file input format")
  }

  return(CTD_data)
}
