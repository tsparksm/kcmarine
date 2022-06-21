#' Identify whether mooring qualifiers are good or bad
#'
#' King County mooring data relies on 3 digit qualifier codes to indicate
#'     whether data is good or bad. The first digit indicates overall quality:
#'     1 = good, 2 = probably good, 3 = suspect, 4 = bad, 9 = missing.
#'     \code{check_mooring} takes a vector of qualifiers as an input, and
#'     returns a vector of the same length with values that indicate whether
#'     each qualifier is good (\code{TRUE}) or bad (\code{FALSE}). For the
#'     purposes of this function, 1 and 2 are good and the rest are bad. This
#'     is particularly useful for filtering out bad data in tibbles. For more
#'     information about qualifier codes, see the \href{https://bit.ly/3bi9Xaz}{mooring website}.
#'
#' @usage check_mooring(quals)
#'
#' @param quals a vector with numeric mooring qualifier codes
#'
#' @return a logical vector indicating good (\code{TRUE}) or bad (\code{FALSE}) qualifier codes
#'
#' @export
#'
#' @examples
#'
#' quals <- c(210, 210, 210, 400, 333)
#' is_good <- check_mooring(quals)

check_mooring <- function(quals) {
  if (any(is.na(quals))) {
    stop("There are NA qualifier codes!")
  }

  ndigits <- floor(log10(quals)) + 1
  if (any(ndigits != 3)) {
    stop("Qualifier codes are not correct length!")
  }

  first_digits <- quals %/% 100
  qual <- ifelse(first_digits < 3,
                 TRUE, FALSE)

  return(good_qual)
}
