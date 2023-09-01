#' Import Dockton mooring data from file
#'
#' Imports Dockton mooring data from a .txt file.
#'     Data is expected to contain ALL the parameters
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
#' @usage import_mooring_dockton(fname)
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
#' file <- system.file("extdata", "test_mooring_dockton_data.txt", package = "kcmarine")
#' data <- import_mooring_dockton(file)

import_mooring_dockton <- function(fname) {

  options(warn=-1)
  temp = readr::read_tsv(fname, skip = 49, col_names = TRUE, col_types = list(
    readr::col_datetime(format = "%m/%d/%Y %I:%M:%S %p"),
    readr::col_double(),  # depth
    readr::col_integer(),
    readr::col_double(),  # temperature
    readr::col_integer(),
    readr::col_double(),  # salinity
    readr::col_integer(),
    readr::col_double(),  # DO %
    readr::col_double(),  # DO
    readr::col_integer(),
    readr::col_double(),  # chlorophyll
    readr::col_integer(),
    readr::col_double(),  # turbidity
    readr::col_integer(),
    readr::col_double(),  # pH
    readr::col_integer(),
    readr::col_double(),  # density
    readr::col_integer(),
    readr::col_double(),  # air temp
    readr::col_integer(),
    readr::col_double(),  # surface PAR
    readr::col_integer(),
    readr::col_double(),  # air pressure
    readr::col_integer(),
    readr::col_double(),  # rainfall
    readr::col_integer(),
    readr::col_double(),  # humidity
    readr::col_integer(),
    readr::col_double(),  # wind dir
    readr::col_integer(),
    readr::col_double(),  # wind speed
    readr::col_integer(),
    readr::col_double(),  # sonde battery
    readr::col_double(),  # logger battery
    readr::col_character(),  # sonde ID
    readr::col_skip()
  ))
  options(warn=1)

  temp <- dplyr::rename(temp, DateTime = Date)
  temp <- dplyr::rename(temp, DO_Sat = "Dissolved_Oxygen_%Sat")
  temp <- dplyr::rename(temp, DO_mg_L = "Dissolved_Oxygen_mg/L")
  temp <- dplyr::rename(temp, Chlorophyll_Fluorescence_ug_L = "Chlorophyll_Fluorescence_ug/L")
  temp <- dplyr::rename(temp, Density_kg_m3 = "Density_kg/m^3")
  temp <- dplyr::rename(temp, Surf_PAR_umol_s_sqm = "Surf_PAR_umol/s/sqm")
  temp <- dplyr::rename(temp, Rel_Humidity = "Rel_Humidity_%")
  temp <- dplyr::rename(temp, Wind_Speed_m_sec = "Wind_Speed_m/sec")

  temp <- tibble::add_column(temp, Qual_DO_Sat = temp$Qual_DO,
                             .after = "DO_Sat")

  lubridate::tz(temp$DateTime) <- "Etc/GMT-8"

  temp$Date <- as.Date(paste(lubridate::month(temp$DateTime), "/",
                             lubridate::day(temp$DateTime), "/",
                             lubridate::year(temp$DateTime), sep = ""),
                       "%m/%d/%Y")

  return(temp)
}
