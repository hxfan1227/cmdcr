#' @import data.table
NULL
#' Read the meteorological data.
#' @param fname Character. Full name of the file to be processed.
#' @return A \code{cmdc} object.
#' @export
read_data <- function(fname){
  cmdc <- cmdcr::import_cmdc_data(fname)
  cmdc <- cmdcr::format_data(cmdc)
  cmdc <- cmdcr::set_colnames(cmdc)
  return(cmdc)
}
