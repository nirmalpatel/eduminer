#' Tidy version of \link{ltm}'s LSAT dataset
#'
#' @source R package \link{ltm}
#'
#' @format A data frame with columns:
#' \describe{
#'  \item{testid}{Unique ID for each test, there is only one test in this dataset}
#'  \item{userid}{Unique ID for each user}
#'  \item{itemid}{Unique ID for each item}
#'  \item{response}{User's response to item, 0 or 1}
#' }
#' @examples
#' \dontrun{
#'  LSAT_tidy
#' }
"LSAT_tidy"
