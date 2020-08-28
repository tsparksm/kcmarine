#' Data - discrete parameters in the Monitoring Portal
#'
#' Tibble containing the parameter ID numbers (ParmID), short parameter names
#' (ParmName), and long parameter names (DisplayName) for every parameter
#' available in the Monitoring Portal. The function
#' \code{download_discrete()} uses a vector of ParmName to determine
#' what parameters to download from the Monitoring Portal.
#'
#' @docType data
#'
#' @usage data(discrete_parms)
#'
#' @format A tibble with 3 variables and 57 rows
#'
#' @examples
#'
#' data(discrete_parms)
#' nitrogen_only <- discrete_parms[grepl("Nitrogen", discrete_parms$DisplayName),]
#' parms_to_download <- nitrogen_only$ParmName
"discrete_parms"
