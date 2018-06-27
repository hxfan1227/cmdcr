#' @import data.table
NULL
#' Read the file name of the meteorological data.
#' @param fname Character. Full name of the file to be processed.
read_fname <- function(fname){
  data.type <- cmdcr::get_data_type(fname)
  class(fname) = c(class(fname), data.type)
  fname
}
