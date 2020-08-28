#' Import CTD data from file
#'
#' Imports CTD data (with all parameters) from a tab-delimited .txt file or
#'     a .csv file. The internal CTD website downloads data either as a .txt
#'     file or as a ".xls" file, which is actually just a .txt file in
#'     disguise. This function uses these data files in their original
#'     formats without any external manipulation of the columns or headers.
#'     Make sure to download ALL parameters, or else the function will not
#'     work correctly. And I repeat: do not change anything in the files
#'     before importing! When in doubt, download new files. This function
#'     will rename the columns to be nicely formatted for use in R.
#'
#'     If you're having trouble and receiving an error about the date format,
#'     try re-downloading the data and NOT editing it in Excel prior to
#'     loading in R. Excel messes up the datetime formatting.
#'
#' @usage import_CTD(fname)
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
#' file <- system.file("extdata", "test_CTD_data.txt", package = "kcmarine")
#' data <- import_CTD(file)

import_CTD <- function(fname) {
  if (stringr::str_detect(fname, ".txt") | stringr::str_detect(fname, ".xls")) {
    CTD_data <- readr::read_tsv(fname,
                         col_types = readr::cols(
                           "Locator" = readr::col_character(),
                           "Sampledate" = readr::col_character(),
                           "Depth" = readr::col_double(),
                           "Updown" = readr::col_character(),
                           "Cast Notes" = readr::col_character(),
                           "Chlorophyll, Field (mg/m^3)" = readr::col_double(),
                           "CH_Qual" = readr::col_character(),
                           "Density, Field (Kg/m^3)" = readr::col_double(),
                           "DN_Qual" = readr::col_character(),
                           "Dissolved Oxygen, Field (mg/l ws=2)" = readr::col_double(),
                           "DO_Qual" = readr::col_character(),
                           "Sigma Density, Field (Kg/m^3)" = readr::col_double(),
                           "SD_Qual" = readr::col_character(),
                           "Light Transmission (%)" = readr::col_double(),
                           "LT_Qual" = readr::col_character(),
                           "Light Intensity (PAR), Field (umol/sm2)" = readr::col_double(),
                           "PA_Qual" = readr::col_character(),
                           "Surface Light Intensity (PAR), Field (umol/sm2)" =
                             readr::col_double(),
                           "PAS_Qual" = readr::col_character(),
                           "Salinity, Field (PSS)" = readr::col_double(),
                           "SA_Qual" = readr::col_character(),
                           "Sample Temperature, Field (deg C)" = readr::col_double(),
                           "ST_Qual" = readr::col_character(),
                           "Turbidity, Field (NTU)" = readr::col_double(),
                           "TB_Qual" = readr::col_character(),
                           "Nitrite + Nitrate Nitrogen, Field (mg/L)" =
                             readr::col_double(),
                           "NO23_Qual" = readr::col_character()
                         ))
  } else if (stringr::str_detect(fname, ".csv")) {
    CTD_data <- readr::read_csv(fname,
                         col_types = readr::cols(
                           "Locator" = readr::col_character(),
                           "Sampledate" = readr::col_character(),
                           "Depth" = readr::col_double(),
                           "Updown" = readr::col_character(),
                           "Cast Notes" = readr::col_character(),
                           "Chlorophyll, Field (mg/m^3)" = readr::col_double(),
                           "CH_Qual" = readr::col_character(),
                           "Density, Field (Kg/m^3)" = readr::col_double(),
                           "DN_Qual" = readr::col_character(),
                           "Dissolved Oxygen, Field (mg/l ws=2)" = readr::col_double(),
                           "DO_Qual" = readr::col_character(),
                           "Sigma Density, Field (Kg/m^3)" = readr::col_double(),
                           "SD_Qual" = readr::col_character(),
                           "Light Transmission (%)" = readr::col_double(),
                           "LT_Qual" = readr::col_character(),
                           "Light Intensity (PAR), Field (umol/sm2)" = readr::col_double(),
                           "PA_Qual" = readr::col_character(),
                           "Surface Light Intensity (PAR), Field (umol/sm2)" =
                             readr::col_double(),
                           "PAS_Qual" = readr::col_character(),
                           "Salinity, Field (PSS)" = readr::col_double(),
                           "SA_Qual" = readr::col_character(),
                           "Sample Temperature, Field (deg C)" = readr::col_double(),
                           "ST_Qual" = readr::col_character(),
                           "Turbidity, Field (NTU)" = readr::col_double(),
                           "TB_Qual" = readr::col_character(),
                           "Nitrite + Nitrate Nitrogen, Field (mg/L)" =
                             readr::col_double(),
                           "NO23_Qual" = readr::col_character()
                         ))
  } else {
    stop("Check CTD file input format")
  }

  # Excel does stupid things to datetimes. Ugh.
  temp <- strptime(CTD_data$Sampledate, "%m/%d/%Y %I:%M:%S %p")
  if (is.na(temp[1])) {
    temp <- strptime(CTD_data$Sampledate, "%m/%d/%Y %H:%M")
  }
  CTD_data$Sampledate <- temp

  CTD_data$Date <- as.Date(paste(lubridate::month(CTD_data$Sampledate), "/",
                                 lubridate::day(CTD_data$Sampledate), "/",
                                 lubridate::year(CTD_data$Sampledate), sep = ""),
                           "%m/%d/%Y")
  CTD_data <- dplyr::rename(CTD_data, CastNotes = "Cast Notes",
                     Chlorophyll = "Chlorophyll, Field (mg/m^3)",
                     Chlorophyll_Qual = "CH_Qual",
                     Density = "Density, Field (Kg/m^3)",
                     Density_Qual = "DN_Qual",
                     DO = "Dissolved Oxygen, Field (mg/l ws=2)",
                     SigmaTheta = "Sigma Density, Field (Kg/m^3)",
                     SigmaTheta_Qual = "SD_Qual",
                     Light_Transmission = "Light Transmission (%)",
                     Light_Qual = "LT_Qual",
                     PAR = "Light Intensity (PAR), Field (umol/sm2)",
                     PAR_Qual = "PA_Qual",
                     Surface_PAR =
                       "Surface Light Intensity (PAR), Field (umol/sm2)",
                     Surface_PAR_Qual = "PAS_Qual",
                     Salinity = "Salinity, Field (PSS)",
                     Salinity_Qual = "SA_Qual",
                     Temperature = "Sample Temperature, Field (deg C)",
                     Temperature_Qual = "ST_Qual",
                     Turbidity = "Turbidity, Field (NTU)",
                     Turbidity_Qual = "TB_Qual",
                     NO23 = "Nitrite + Nitrate Nitrogen, Field (mg/L)")

  return(CTD_data)
}
