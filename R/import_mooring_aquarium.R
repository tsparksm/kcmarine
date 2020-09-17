#' Import Seattle Aquarium mooring data from file
#'
#' Imports Seattle Aquarium mooring data
#'     from a .txt file. Data is expected to contain ALL the parameters
#'     available for download from the public mooring website
#'     (https://green2.kingcounty.gov/marine-buoy/). Columns and header must
#'     be the same as the original data download. When in doubt, download a
#'     new file! Again, make sure you download ALL the parameters. The
#'     function cleans up the column names for easy use in R, and adds a
#'     simple date column.
#'
#'     If you're having trouble and receiving an error about the date format,
#'     try re-downloading the data and NOT editing it in Excel prior to
#'     loading in R. Excel messes up the datetime formatting.
#'
#' @usage import_mooring_aquarium(fname)
#'
#' @param fname The filename to import, as a string
#'
#' @return A tibble with imported data and clean column names
#'
#' @export
#'
#' @importFrom readr read_tsv col_datetime col_double col_integer col_character col_skip
#'
#' @importFrom dplyr rename
#'
#' @importFrom lubridate year month day tz
#'
#' @import tibble
#'
#' @examples
#'
#' file <- system.file("extdata", "test_mooring_aquarium_data.txt", package = "kcmarine")
#' data <- import_mooring_aquarium(file)

import_mooring_aquarium <- function(fname) {

  options(warn=-1)
  temp = readr::read_tsv(fname, skip = 49, col_names = TRUE, col_types = list(
    readr::col_datetime(format = "%m/%d/%Y %I:%M:%S %p"),
    readr::col_double(), # depth 1
    readr::col_integer(),
    readr::col_double(), # depth 2
    readr::col_integer(),
    readr::col_double(),  # temperature 1
    readr::col_integer(),
    readr::col_double(),  # temperature 2
    readr::col_integer(),
    readr::col_double(),  # salinity 1
    readr::col_integer(),
    readr::col_double(),  # salinity 2
    readr::col_integer(),
    readr::col_double(),  # DO % 1
    readr::col_double(),  # DO 1
    readr::col_integer(),
    readr::col_double(),  # DO % 2
    readr::col_double(),  # DO 2
    readr::col_integer(),
    readr::col_double(),  # chl 1
    readr::col_integer(),
    readr::col_double(),  # chl 2
    readr::col_integer(),
    readr::col_double(),  # turbidity 1
    readr::col_integer(),
    readr::col_double(),  # turbidity 2
    readr::col_integer(),
    readr::col_double(),  # pH 1
    readr::col_integer(),
    readr::col_double(),  # pH 2
    readr::col_integer(),
    readr::col_double(),  # density 1
    readr::col_integer(),
    readr::col_double(),  # density 2
    readr::col_integer(),
    readr::col_double(),  # air temp
    readr::col_integer(),
    readr::col_double(),  # PAR
    readr::col_integer(),
    readr::col_double(),  # pressure
    readr::col_integer(),
    readr::col_double(),  # rainfall
    readr::col_integer(),
    readr::col_double(),  # humidity
    readr::col_integer(),
    readr::col_double(),  # wind direction
    readr::col_integer(),
    readr::col_double(),  # wind speed
    readr::col_integer(),
    readr::col_double(),  # sonde batt 1
    readr::col_double(),  # sonde batt 2
    readr::col_double(),  # logger batt
    readr::col_character(),  # sonde ID 1
    readr::col_character(),  # sonde ID 2
    readr::col_skip()
  ))
  options(warn=1)

  temp <- dplyr::rename(temp, DateTime = Date)
  temp <- dplyr::rename(temp, DO_Sat_1 = "1_Dissolved_Oxygen_%Sat")
  temp <- dplyr::rename(temp, DO_Sat_2 = "2_Dissolved_Oxygen_%Sat")
  temp <- dplyr::rename(temp, DO_mg_L_1 = "1_Dissolved_Oxygen_mg/L")
  temp <- dplyr::rename(temp, DO_mg_L_2 = "2_Dissolved_Oxygen_mg/L")
  temp <- dplyr::rename(temp, Chlorophyll_Fluorescence_ug_L_1 = "1_Chlorophyll_Fluorescence_ug/L")
  temp <- dplyr::rename(temp, Chlorophyll_Fluorescence_ug_L_2 = "2_Chlorophyll_Fluorescence_ug/L")
  temp <- dplyr::rename(temp, Density_kg_m3_1 = "1_Density_kg/m^3")
  temp <- dplyr::rename(temp, Density_kg_m3_2 = "2_Density_kg/m^3")
  temp <- dplyr::rename(temp, Surf_PAR_umol_s_sqm = "Surf_PAR_umol/s/sqm")
  temp <- dplyr::rename(temp, Rel_Humidity = "Rel_Humidity_%")
  temp <- dplyr::rename(temp, Wind_Speed_m_sec = "Wind_Speed_m/sec")
  temp <- dplyr::rename(temp, Depth_m_1 = "1_Depth_m")
  temp <- dplyr::rename(temp, Water_Temperature_degC_1 = "1_Water_Temperature_degC")
  temp <- dplyr::rename(temp, Salinity_PSU_1 = "1_Salinity_PSU")
  temp <- dplyr::rename(temp, Turbidity_NTU_1 = "1_Turbidity_NTU")
  temp <- dplyr::rename(temp, Sonde_pH_1 = "1_Sonde_pH")
  temp <- dplyr::rename(temp, Depth_m_2 = "2_Depth_m")
  temp <- dplyr::rename(temp, Water_Temperature_degC_2 = "2_Water_Temperature_degC")
  temp <- dplyr::rename(temp, Salinity_PSU_2 = "2_Salinity_PSU")
  temp <- dplyr::rename(temp, Turbidity_NTU_2 = "2_Turbidity_NTU")
  temp <- dplyr::rename(temp, Sonde_pH_2 = "2_Sonde_pH")
  temp <- dplyr::rename(temp, Sonde_Batt_V_1 = "1_Sonde_Batt_V")
  temp <- dplyr::rename(temp, Sonde_ID_1 = "1_Sonde_ID")
  temp <- dplyr::rename(temp, Sonde_Batt_V_2 = "2_Sonde_Batt_V")
  temp <- dplyr::rename(temp, Sonde_ID_2 = "2_Sonde_ID")

  temp <- tibble::add_column(temp, Qual_DO_Sat_1 = temp$Qual_1_DO,
                             .after = "DO_Sat_1")
  temp <- tibble::add_column(temp, Qual_DO_Sat_2 = temp$Qual_2_DO,
                             .after = "DO_Sat_2")

  lubridate::tz(temp$DateTime) <- "America/Los_Angeles"

  temp$Date <- as.Date(paste(lubridate::month(temp$DateTime), "/",
                             lubridate::day(temp$DateTime), "/",
                             lubridate::year(temp$DateTime), sep = ""),
                       "%m/%d/%Y")

  return(temp)
}
