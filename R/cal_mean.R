#' @import data.table
NULL
#' Calculate the mean of the nearest N stations
#' @param id The ID of the desired station
#' @param dt A \code{data.table} of observation data. 
#' Typically, it should contain the columns including ID, Year, Month and Day.
#' @param N A integer indicating how many stations to be used.
#' @param SDcols Specifies the columns of x to be included in the special symbol \link[data.table]{.SD} which stands for Subset of data.table.
#' May be character column names or numeric positions. 
#' This is useful for speed when applying a function through a subset of (possible very many) columns; 
#' e.g., \code{DT[, lapply(.SD, sum), by="x,y", .SDcols=301:350]}.
#' For convenient interactive use, the form startcol:endcol is also allowed (as in by), e.g., \code{DT[, lapply(.SD, sum), by=x:y, .SDcols=a:f]}.
#' @param BYcols A character vector of column names: e.g., \code{DT[, sum(a), by=c("x", "y")]}.
#' @return A data.table with missing value filled for the disired station.
cal_mean <- function(id, 
                     dt, 
                     N = 5,
                     SDcols = data.table::.SD, 
                     BYcols = c('Year', 'Month', 'Day')){
  temp <- dt[ID %in% nearst_station_id[[as.character(id)]][2:(N+1)],
             lapply(.SD, FUN = function(x){mean(x, na.rm = T)}),
             .SDcols = SDcols,
             by = BYcols]
  temp[, ID := id]
}