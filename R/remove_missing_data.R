NULL
#' Function to remove missing data in the data.table.
#' @param x Numeric. A column of data to be processed.
#' @param ... Not used yet.
#' @export
remove_missing_data <- function(x, ...)
{
  x[abs(x) == 32700] <- 0
  x[abs(x) == 32766] <- NA
  x[abs(x) == 32744] <- NA
  x[!is.na(x) & floor((x-30000)/1000) == 0] <- x[!is.na(x) & floor((x-30000)/1000) == 0]-30000
  x[!is.na(x) & floor((x-30000)/1000) == 1] <- x[!is.na(x) & floor((x-30000)/1000) == 1]-31000
  x[!is.na(x) & floor((x-30000)/1000) == 2] <- x[!is.na(x) & floor((x-30000)/1000) == 2]-32000
  x[!is.na(x) & x>300000] <- x[!is.na(x) & x>300000] - 300000
  x[!is.na(x) & x>1000000] <- x[!is.na(x) & x>1000000] - 1000000
  return(x)
}
