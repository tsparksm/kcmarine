#' Download discrete marine data from the Monitoring Portal
#'
#' King County VPN connection required! This function allows you to easily
#'     download any subset of parameters for a given site (or sites) from the
#'     Marine Monitoring Portal. The data will be saved as a .csv file so you
#'     don't have to keep re-downloading it. Speedy and convenient!
#'     Automatically removes bad and missing data with qualifier codes 4 or 9.
#'     Based on code by Curtis DeGasperi.
#'
#' @usage download_discrete(sites, parms_in, fname, include_bad = F)
#'
#' @param sites a character vector with locator codes for stations. These
#'     stations are the ones for which data will be downloaded.
#'
#' @param parms_in a character vector with short parameter names (ParmName)
#'     for which you want data. See \code{discrete_parms}, which is data built
#'     into this package, for a complete list.
#'
#' @param fname a string indicating the filename where the data will be saved.
#'     The filename is relative to the current working directory,
#'     \code{getwd()}.
#'
#' @param include_bad a logical value indicating whether to include the bad
#'     and missing data. Automatically set to FALSE.
#'
#' @export
#'
#' @importFrom XML readHTMLTable
#'
#' @importFrom dplyr filter
#'
#' @importFrom jsonlite fromJSON
#'
#' @importFrom readr write_csv
#'
#' @import tibble
#'
#' @examples
#'
#' sites <- c("NSEX01", "JSUR01", "LSNT01")
#' parms_in <- c("NNN", "DO")
#' fname <- "my_discrete_data.csv"
#'
#' \dontrun{download_discrete(sites, parms_in, fname, include_bad = T)}

download_discrete <- function(sites, parms_in, fname, include_bad = F) {
  # Only download marine data, site type 254
  siteType <- 254

  user <- getPass::getPass(msg='Enter user ID')
  pw <- getPass::getPass(msg='Enter password')

  # Load site data (e.g. latitude, longitude)
  off.webpage <- paste0("http://dnrp-apps2/Monitoring-Portal/Sites?SiteType=",
                        siteType,
                        "&pageSize=1000")
  tt <- RCurl::getURL(off.webpage, userpwd = paste(user, pw, sep = ":"))
  off.sites <- XML::readHTMLTable(tt,
                                  stringsAsFactors = FALSE)[[1]]
  colnames(off.sites)=c("Details","SiteID","SiteName","Locator","Latitude",
                        "Longitude","Shallow","SiteType","Area")
  off.sites <- dplyr::filter(off.sites, Locator %in% sites)

  # Get list of SiteID numbers to download
  locations <- off.sites$SiteID

  # Identify parameters to save
  # A complete list of parms can be found in the attached discrete_parms tibble
  # Use the ParmName (i.e. the short name) in the input (parms_in)
  parameters <- dplyr::filter(discrete_parms, ParmName %in% parms_in)

  # Pull all data between 1970 and present
  MinDate <-"1970-01-01"
  MaxDate <- Sys.Date()

  # Download all the data, one site and parameter at a time
  data <- NULL
  for (loc in locations){
    for (parm in parameters$ParmID){
      option1<-paste0('http://dnrp-apps2/Monitoring-Portal/ReviewServices/SampleParms?MinDate=',
                      MinDate, "&MaxDate=", MaxDate, '&Parm=', parm,
                      '&Site=', loc, '&SiteType=', siteType, '&RowLimit=100000')
      tt <- RCurl::getURL(option1, userpwd = paste(user, pw, sep = ":"))
      data1 <- jsonlite::fromJSON(tt)
      data <- rbind(data, data1)
      Location <- subset(off.sites, off.sites$SiteID==loc)
      Parameter <- subset(parameters, parameters$ParmID==parm)
      print(paste("Processing location ",Location$Locator,
                  " and parameter ", Parameter$DisplayName))
    }
  }

  # Adjust the datetimes
  data$CollectDate<-gsub('/Date\\(','',data$CollectDate)
  data$CollectDate<-gsub(')/','',data$CollectDate)
  data$CollectDate<-as.Date(as.POSIXct(as.numeric(data$CollectDate)/1000,
                                       origin="1970-01-01"))

  data$CollectDateTime<-gsub('/Date\\(','',data$CollectDateTime)
  data$CollectDateTime<-gsub(')/','',data$CollectDateTime)
  data$CollectDateTime<-as.POSIXct(as.numeric(data$CollectDateTime)/1000,
                                   origin="1970-01-01")

  # Replace values with override values
  data$Value <- with(data, ifelse(is.na(OverrideValue),
                                  Value, OverrideValue))

  # Remove bad (9) or missing (4) data if include_bad is FALSE
  if (!include_bad) {
    nt <- nrow(data)
    data <- dplyr::filter(data, !QualityId %in% c(4, 9))
    print(paste("Bad or missing data points removed:", nt - nrow(data)))
  }

  # Save data to a .csv file
  readr::write_csv(data, fname, na = "NA")
}
