#' @import data.table
NULL
#' Read the meteorological data.
#' @param fname Character. Full name of the file to be processed.
#' @param as.data.table Logical. If \code{True}, a data.table will return.
#' @export
read_data <- function(fname){
  data.type <- cmdcr::get_data_type(fname)
  dt <- data.table::fread(fname)
  dt.class <- class(dt)
  class(dt) <- c(data.type, dt.class)
  dt <- cmdcr::format_data(dt)
  return(cmdcr::set_colnames(dt))
}
