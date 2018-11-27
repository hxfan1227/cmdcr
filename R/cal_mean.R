#' @import data.table
NULL
#' Calculate the mean of the nearest N stations
#' @param id The ID of the desired station
#' @param dt A \code{data.table} of observation data. 
#' Typically, it should contain the columns including ID, Year, Month and Day.
#' @param N A integer indicating how many stations to be used.
#' @param BYcols A character vector of column names: e.g., \code{DT[, sum(a), by=c("x", "y")]}.
#' @param ... Other arguments passed to \link[data.table]{data.table}.
#' @return A data.table with missing value filled for the disired station.
cal_mean <- function(id, 
                     dt, 
                     N = 5,
                     BYcols = c('Year', 'Month', 'Day'), 
                     ...){
  temp <- dt[ID %in% nearst_station_id[[as.character(id)]][2:(N+1)],
             lapply(.SD, FUN = function(x){mean(x, na.rm = T)}),
             by = BYcols, 
             ...]
  temp[, ID := id]
}