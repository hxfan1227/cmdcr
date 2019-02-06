#' @importFrom plyr rename
NULL
#' Set column names for each dataset.
#' @param x A \code{cmdc} object. Created by \link[cmdcr]{import_cmdc_data}.
#' @param ... Not used yet.
#' @export
set_colnames <- function(x, ...){
  UseMethod("set_colnames")
}

#' @rdname set_colnames
#' @export
set_colnames.PRS <- function(x, ...){
  dt <- x$data
  x$data <- plyr::rename(dt, replace = c("V1" = "ID",
                                         "V2" = "Lat",
                                         "V3" = "Lon",
                                         "V4" = "H",
                                         "V5" = "Year",
                                         "V6" = "Month",
                                         "V7" = "Day",
                                         "V8" = "AvePRS",
                                         "V9" = "MaxPRS",
                                         "V10" = "MinPRS",
                                         "V11" = "PRSCODE1",
                                         "V12" = "PRSCODE2",
                                         "V13" = "PRSCODE3"))
  data.table::setkeyv(x$data, c('ID', 'Lon', 'Lat', 'H', 'Year', 'Month', 'Day'))
  x
}

#' @rdname set_colnames
#' @export
set_colnames.TEM <- function(x, ...){
  dt <- x$data
  x$data <- plyr::rename(dt, replace = c("V1" = "ID",
                                         "V2" = "Lat",
                                         "V3" = "Lon",
                                         "V4" = "H",
                                         "V5" = "Year",
                                         "V6" = "Month",
                                         "V7" = "Day",
                                         "V8" = "AveTEM",
                                         "V9" = "MaxTEM",
                                         "V10" = "MinTEM",
                                         "V11" = "TEMCODE1",
                                         "V12" = "TEMCODE2",
                                         "V13" = "TEMCODE3"))
  data.table::setkeyv(x$data, c('ID', 'Lon', 'Lat', 'H', 'Year', 'Month', 'Day'))
  x
}

#' @rdname set_colnames
#' @export
set_colnames.RHU <- function(x, ...){
  dt <- x$data
  x$data <- plyr::rename(dt, replace = c("V1" = "ID",
                                         "V2" = "Lat",
                                         "V3" = "Lon",
                                         "V4" = "H",
                                         "V5" = "Year",
                                         "V6" = "Month",
                                         "V7" = "Day",
                                         "V8" = "AveRHU",
                                         "V9" = "MinRHU",
                                         "V10" = "RHUCODE1",
                                         "V11" = "RHUCODE2"))
  data.table::setkeyv(x$data, c('ID', 'Lon', 'Lat', 'H', 'Year', 'Month', 'Day'))
  x
}

#' @rdname set_colnames
#' @export
set_colnames.PRE <- function(x, ...){
  dt <- x$data
  x$data <- plyr::rename(dt, replace = c("V1" = "ID",
                                         "V2" = "Lat",
                                         "V3" = "Lon",
                                         "V4" = "H",
                                         "V5" = "Year",
                                         "V6" = "Month",
                                         "V7" = "Day",
                                         "V8" = "20_8PRE",
                                         "V9" = "8_20PRE",
                                         "V10" = "20_20PRE",
                                         "V11" = "PRECODE1",
                                         "V12" = "PRECODE2",
                                         "V13" = "PRECODE3"))
  data.table::setkeyv(x$data, c('ID', 'Lon', 'Lat', 'H', 'Year', 'Month', 'Day'))
  x
}

#' @rdname set_colnames
#' @export
set_colnames.EVP <- function(x, ...){
  dt <- x$data
  x$data <- plyr::rename(dt, replace = c("V1" = "ID",
                                         "V2" = "Lat",
                                         "V3" = "Lon",
                                         "V4" = "H",
                                         "V5" = "Year",
                                         "V6" = "Month",
                                         "V7" = "Day",
                                         "V8" = "SEVP",
                                         "V9" = "LEVP",
                                         "V10" = "EVPCODE1",
                                         "V11" = "EVPCODE2"))
  data.table::setkeyv(x$data, c('ID', 'Lon', 'Lat', 'H', 'Year', 'Month', 'Day'))
  x
}

#' @rdname set_colnames
#' @export
set_colnames.WIN <- function(x, ...){
  dt <- x$data
  x$data <- plyr::rename(dt, replace = c("V1" = "ID",
                                         "V2" = "Lat",
                                         "V3" = "Lon",
                                         "V4" = "H",
                                         "V5" = "Year",
                                         "V6" = "Month",
                                         "V7" = "Day",
                                         "V8" = "AveWINS",
                                         "V9" = "MaxWINS",
                                         "V10" = "MaxWIND",
                                         "V11" = "ExtWINS",
                                         "V12" = "ExtWIND",
                                         "V13" = "WINCODE1",
                                         "V14" = "WINCODE2",
                                         "V15" = "WINCODE3",
                                         "V16" = "WINCODE4",
                                         "V17" = "WINCODE5"))
  data.table::setkeyv(x$data, c('ID', 'Lon', 'Lat', 'H', 'Year', 'Month', 'Day'))
  x
}

set_colnames.SSD <- function(x, ...){
  dt <- x$data
  x$data <- plyr::rename(dt, replace = c("V1" = "ID",
                                         "V2" = "Lat",
                                         "V3" = "Lon",
                                         "V4" = "H",
                                         "V5" = "Year",
                                         "V6" = "Month",
                                         "V7" = "Day",
                                         "V8" = "SSD",
                                         "V9" = "SSDCODE1"))
  data.table::setkeyv(x$data, c('ID', 'Lon', 'Lat', 'H', 'Year', 'Month', 'Day'))
  x
}

set_colnames.GST <- function(x, ...){
  dt <- x$data
  x$data <- plyr::rename(dt, replace = c("V1" = "ID",
                                         "V2" = "Lat",
                                         "V3" = "Lon",
                                         "V4" = "H",
                                         "V5" = "Year",
                                         "V6" = "Month",
                                         "V7" = "Day",
                                         "V8" = "AveGST",
                                         "V9" = "MaxGST",
                                         "V10" = "MinGST",
                                         "V11" = "GSTCODE1",
                                         "V12" = "GSTCODE2",
                                         "V13" = "GSTCODE3"))
  data.table::setkeyv(x$data, c('ID', 'Lon', 'Lat', 'H', 'Year', 'Month', 'Day'))
  x
}

