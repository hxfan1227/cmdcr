#' @import plyr
NULL
#' Combine cmdc data.table by rows or columns
#' @param ... (optional) List of \code{data.table} objects to be combined. 
#' @param dtlist (optional) List of \code{data.table} to be combined.
#' @param dir Character. The directory which contains the cmdc files.
#' @param filepattern. Character. Variable to be combined.
#' @return A \code{data.table}.
#' @export
rbind_cmdc_data <- function(..., 
                            dtlist = NULL,
                            dir, 
                            filepattern = c('PRS', 'TEM', 'WIN', 'GST', 'PRE', 'RHU', 'EVP', 'SSD')
                            ){
  if (missing(dir)){
    dt_list <- c(list(...), dtlist)
  } else {
    fpattern <- match.arg(filepattern)
    flist <- list.files(path = dir, pattern = fpattern, full.names = T)
    dt_list <- plyr::alply(flist, 1, .fun = function(x){cmdcr::read_data(x)$data})
  }
  dt <- plyr::rbind.fill(dt_list)
  dt
}

#' Merge cmdc data.table
#' @rdname rbind_cmdc_data
#' @export
merge_cmdc_data <- function(..., 
                            dtlist = NULL,
                            dir){
  if (missing(dir)){
    dt_list <- c(list(...), dtlist)
  } else {
    dt_list <- list(cmdcr::rbind_cmdc_data(dir = dir, filepattern = 'PRS'),
                    cmdcr::rbind_cmdc_data(dir = dir, filepattern = 'TEM'),
                    cmdcr::rbind_cmdc_data(dir = dir, filepattern = 'WIN'),
                    cmdcr::rbind_cmdc_data(dir = dir, filepattern = 'GST'),
                    cmdcr::rbind_cmdc_data(dir = dir, filepattern = 'PRE'),
                    cmdcr::rbind_cmdc_data(dir = dir, filepattern = 'RHU'),
                    cmdcr::rbind_cmdc_data(dir = dir, filepattern = 'EVP'),
                    cmdcr::rbind_cmdc_data(dir = dir, filepattern = 'SSD'))
    dt <- Reduce(function(...) data.table::merge(..., all = T), dt_list)
    data.table::setDT(dt)
    dt
  }
}
