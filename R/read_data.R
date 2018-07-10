#' @import data.table
NULL
#' Read the meteorological data.
#' @param fname Character. Full name of the file to be processed.
#' @param as.data.table Logical. If \code{True}, a data.table will return.
#' @export
read_data <- function(fname, as.data.table = T){
  data.type <- cmdcr::get_data_type(fname)
  class(fname) = c(class(fname), data.type)
  if (as.data.table){
    dt <- data.table::fread(fname)
    class(dt) <- c(class(dt), data.type)
    cmdcr::set_colnames(dt)
    return(dt)
  } else{
    return(fname)
  }
}
