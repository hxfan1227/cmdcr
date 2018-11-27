NULL
#' Get the nearest stations to the preferd station.
#' @param id The ID of the desired station
#' @param lonlat logical. If TRUE, coordinates should be in degrees; 
#' else they should represent planar ('Euclidean') space (e.g. units of meters).
#' See \link[raster]{pointDistance} for details.
#' @param dt A data.table which contains 3 columns (i.e., ID, Lon, Lat).
#' @param ... Other parameters which passed to \link[raster]{pointDistance}
#' @return A vector with station IDs sorted by distance.
get_nearest_station <- function(id, 
                                lonlat = T, 
                                dt, ...){
  temp_dis <- pointDistance(dt[ID == id, .(Lon, Lat)], 
                            dt[, .(Lon, Lat)],
                            lonlat = T, ...)
  names(temp_dis) <- dt$ID
  sorted_dis <- sort(temp_dis)
  return(as.numeric(names(sorted_dis)))
}