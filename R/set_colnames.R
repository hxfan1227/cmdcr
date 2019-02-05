#' @importFrom plyr rename
NULL
#' Set column names for each dataset.
#' @param x  Dataset to be processed.
#' @param ... Not used yet.
#' @export
set_colnames <- function(x, ...){
  UseMethod("set_colnames")
}

#' @rdname set_colnames
#' @export
set_colnames.PRS <- function(x, ...){
  plyr::rename(x, replace = c("V1" = "ID",
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
}

#' @rdname set_colnames
#' @export
set_colnames.TEM <- function(x, ...){
  plyr::rename(x, replace = c("V1" = "ID",
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
}

#' @rdname set_colnames
#' @export
set_colnames.RHU <- function(x, ...){
  plyr::rename(x, replace = c("V1" = "ID",
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
}

#' @rdname set_colnames
#' @export
set_colnames.PRE <- function(x, ...){
  plyr::rename(x, replace = c("V1" = "ID",
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
                              "V13" = "PRECODE3",
                              "V14" = "PRECODE4"))
}

#' @rdname set_colnames
#' @export
set_colnames.EVP <- function(x, ...){
  plyr::rename(x, replace = c("V1" = "ID",
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
}

#' @rdname set_colnames
#' @export
set_colnames.WIN <- function(x, ...){
  plyr::rename(x, replace = c("V1" = "ID",
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
}

set_colnames.SSD <- function(x, ...){
 plyr::rename(x, replace = c("V1" = "ID",
                             "V2" = "Lat",
                             "V3" = "Lon",
                             "V4" = "H",
                             "V5" = "Year",
                             "V6" = "Month",
                             "V7" = "Day",
                             "V8" = "SSD",
                             "V9" = "SSDCODE1"))
}

set_colnames.GST <- function(x, ...){
  plyr::rename(x, replace = c("V1" = "ID",
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
}

