#' @import plyr
NULL
#' Combine cmdc data.table by rows or columns
#' @param ... (optional) List of \code{data.table} objects to be combined. 
#' @param dtlist (optional) List of \code{data.table} to be combined.
#' @param dir Character. The directory which contains the cmdc files.
#' @param filepattern Character. Variable to be combined.
#' @param filepatterns Character. Variables to be combined.
#' @param year Numeric. The year of the data to be processed. 
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
  data.table::setDT(dt)
  dt
}

#' Merge cmdc data.table
#' @rdname rbind_cmdc_data
#' @export
merge_cmdc_data <- function (..., 
                             dtlist = NULL, 
                             dir, 
                             filepatterns = c("PRS", "TEM", "WIN", "GST", "PRE", "RHU", "EVP", "SSD"), 
                             year = NULL) 
{
  if (missing(dir)) {
    dt_list <- c(list(...), dtlist)
  }
  else {
    flist <- list.files(path = dir, pattern = ".TXT$", 
                        full.names = T)
    dates <- plyr::aaply(flist, 1, cmdcr::get_data_date)
    dates <- lubridate::ymd(paste(dates, "01", sep = ""))
    years <- unique(lubridate::year(dates))
    if (is.numeric(year)) {
      if (!(year %in% years)) 
        stop(year, " is not in ", paste(years, 
                                        collapse = ","))
      cat("Processing data of ", year, " ...\n", 
          sep = "")
      plyr::a_ply(years, 1, function(x) {
        dir.create(file.path(dir, x))
      })
      plyr::a_ply(years, 1, function(x) {
        file.copy(flist[stringr::str_detect(flist, as.character(x))], 
                  file.path(dir, x))
      })
      param_dt <- data.table(dir = rep(file.path(dir, 
                                                 year), each = length(filepatterns)), 
                             filepattern = filepatterns)
    }
    else {
      param_dt <- data.table::data.table(dir = rep(dir, 
                                                   each = length(filepatterns)), 
                                         filepattern = filepatterns)
    }
    dt_list <- plyr::mlply(param_dt, cmdcr::rbind_cmdc_data, 
                           .progress = "text", dtlist = NULL)
    dt <- Reduce(function(...) merge(..., all = T), dt_list)
    data.table::setDT(dt)
    return(dt)
  }
}
