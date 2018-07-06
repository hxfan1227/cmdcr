NULL
#' Set column names for each dataset.
#' @param x  Dataset to be processed.
#' @param ... Not used yet.
set_colnames <- function(x, ...){
  UseMethod("set_colnames")
}

#' @rdname set_colnames
#' @export
set_colnames.PRS <- function(x, ...){
  colnames(x) <<- c("ID",
                   "Lat",
                   "Lon",
                   "H",
                   "Year",
                   "Month",
                   "Day",
                   "AvePRS",
                   "MaxPRS",
                   "MinPRS",
                   "PRSCODE1",
                   "PRSCODE2",
                   "PRSCODE3")
}

#' @rdname set_colnames
#' @export
set_colnames.TEM <- function(x, ...){
  colnames(x) <<- c("ID",
                    "Lat",
                    "Lon",
                    "H",
                    "Year",
                    "Month",
                    "Day",
                    "AveTEM",
                    "MaxTEM",
                    "MinTEM",
                    "TEMCODE1",
                    "TEMCODE2",
                    "TEMCODE3")
}

#' @rdname set_colnames
#' @export
set_colnames.RHU <- function(x, ...){
  colnames(x) <<- c("ID",
                    "Lat",
                    "Lon",
                    "H",
                    "Year",
                    "Month",
                    "Day",
                    "AveRHU",
                    "MinRHU",
                    "RHUCODE1",
                    "RHUCODE2")
}

#' @rdname set_colnames
#' @export
set_colnames.PRE <- function(x, ...){
  colnames(x) <<- c("ID",
                    "Lat",
                    "Lon",
                    "H",
                    "Year",
                    "Month",
                    "Day",
                    "20_8PRE",
                    "8_20PRE",
                    "20_20PRE",
                    "PRECODE1",
                    "PRECODE2",
                    "PRECODE3",
                    "PRECODE4")
}

#' @rdname set_colnames
#' @export
set_colnames.EVP <- function(x, ...){
  colnames(x) <<- c("ID",
                    "Lat",
                    "Lon",
                    "H",
                    "Year",
                    "Month",
                    "Day",
                    "SEVP",
                    "LEVP",
                    "EVPCODE1",
                    "EVPCODE2")
}

#' @rdname set_colnames
#' @export
set_colnames.WIN <- function(x, ...){
  colnames(x) <<- c("ID",
                    "Lat",
                    "Lon",
                    "H",
                    "Year",
                    "Month",
                    "Day",
                    "AveWINS",
                    "MaxWINS",
                    "MaxWIND",
                    "ExtWINS",
                    "ExtWIND",
                    "WINCODE1",
                    "WINCODE2",
                    "WINCODE3",
                    "WINCODE4",
                    "WINCODE5")
}

set_colnames.SSD <- function(x, ...){
  colnames(x) <<- c("ID",
                    "Lat",
                    "Lon",
                    "H",
                    "Year",
                    "Month",
                    "Day",
                    "SSD",
                    "SSDCODE1")
}

set_colnames.GST <- function(x, ...){
  colnames(x) <<- c("ID",
                    "Lat",
                    "Lon",
                    "H",
                    "Year",
                    "Month",
                    "Day",
                    "AveGST",
                    "MaxGST",
                    "MinGST",
                    "GSTCODE1",
                    "GSTCODE2",
                    "GSTCODE3")
}

