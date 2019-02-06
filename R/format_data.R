#' @import data.table
NULL
#' Format the meteorological data.
#' @param x A \code{cmdc} object. Created by \link[cmdcr]{import_cmdc_data}
#' @param base_direction Character. Direction of 0 degree. Should be one of (E)ast, (W)est, (S)outh and (N)orth.
#' @param ... Not used yet.
#' @export
format_data <- function(x, ...){
  UseMethod("format_data")
}

#' @rdname format_data
#' @export
format_data.WIN <- function(x, base_direction = "N", ...){
  x$data <- x$data[, lapply(.SD, cmdcr::remove_missing_data)]
  x$data[, ':='(
    # latitude
    V2 = (V2 - (V2 %/% 100) * 100) / 60 + (V2 %/% 100),
    # longitude
    V3 = (V3 - (V3 %/% 100) * 100) / 60 + (V3 %/% 100),
    # altitude
    V4 = ifelse(V4 > 100000, (V4 - 100000) * 0.1, V4 * 0.1),
    # average wind speed
    V8 = ifelse(V8 > 1000, (V8 - 1000) * 0.1, V8 * 0.1),
    # maximum wind speed
    V9 = ifelse(V9 > 1000, (V9 - 1000) * 0.1, V9 * 0.1),
    # maximum wind direction
    V10 = cal_wind_direction(V10, base_direction = base_direction),
    # extreme wind speed
    V11 = ifelse(V11 > 1000, (V11 - 1000) * 0.1, V11 * 0.1),
    # extreme wind direction
    V12 = cmdcr::cal_wind_direction(V12, base_direction = base_direction)
  )]
  x
}

#' @rdname format_data
#' @export
format_data.PRS <- function(x, ...){
  x$data = x$data[, lapply(.SD, cmdcr::remove_missing_data)]
  x$data[, ':='(
    # latitude
    V2 = (V2 - (V2 %/% 100) * 100) / 60 + (V2 %/% 100),
    # longitude
    V3 = (V3 - (V3 %/% 100) * 100) / 60 + (V3 %/% 100),
    # altitude
    V4 = ifelse(V4 > 100000, (V4 - 100000) * 0.1, V4 * 0.1),
    # average pressure
    V8 = V8 * 0.1,
    # maximum pressure
    V9 = ifelse(V9 > 20000, (V9 - 20000) * 0.1, V9 * 0.1)
  )]
  x
}

#' @rdname format_data
#' @export
format_data.EVP <- function(x, ...){
  x$data = x$data[, lapply(.SD, cmdcr::remove_missing_data)]
  x$data[, ':='(
    # latitude
    V2 = (V2 - (V2 %/% 100) * 100) / 60 + (V2 %/% 100),
    # longitude
    V3 = (V3 - (V3 %/% 100) * 100) / 60 + (V3 %/% 100),
    # altitude
    V4 = ifelse(V4 > 100000, (V4 - 100000) * 0.1, V4 * 0.1),
    # small evp
    V8 = ifelse(V8 > 1000, (V8 - 1000) * 0.1, V8 * 0.1),
    # large evp
    V9 = ifelse(V9 > 1000, (V9 - 1000) * 0.1, V9 * 0.1)
  )]
  x
}

#' @rdname format_data
#' @export
format_data.TEM <- function(x, ...){
  x$data = x$data[, lapply(.SD, cmdcr::remove_missing_data)]
  x$data[, ':='(
    # latitude
    V2 = (V2 - (V2 %/% 100) * 100) / 60 + (V2 %/% 100),
    # longitude
    V3 = (V3 - (V3 %/% 100) * 100) / 60 + (V3 %/% 100),
    # altitude
    V4 = ifelse(V4 > 100000, (V4 - 100000) * 0.1, V4 * 0.1),
    # average temperature
    V8 = V8 * 0.1,
    # maximum temperature
    V9 = V9 * 0.1,
    # minimum temperature
    V10 = V10 * 0.1
  )]
  x
}

#' @rdname format_data
#' @export
format_data.RHU <- function(x, ...){
  x$data = x$data[, lapply(.SD, cmdcr::remove_missing_data)]
  x$data[, ':='(
    # latitude
    V2 = (V2 - (V2 %/% 100) * 100) / 60 + (V2 %/% 100),
    # longitude
    V3 = (V3 - (V3 %/% 100) * 100) / 60 + (V3 %/% 100),
    # altitude
    V4 = ifelse(V4 > 100000, (V4 - 100000) * 0.1, V4 * 0.1),
    # average humidity
    V8 = ifelse(V8 > 300, (V8 - 300) * 0.01, V8 * 0.01),
    # average humidity (mannual)
    V9 = ifelse(V9 > 300, (V9 - 300) * 0.01, V9 * 0.01)
  )]
  x
}

#' @rdname format_data
#' @export
format_data.PRE <- function(x, ...){
  x$data = x$data[, lapply(.SD, cmdcr::remove_missing_data)]
  x$data[, ':='(
    # latitude
    V2 = (V2 - (V2 %/% 100) * 100) / 60 + (V2 %/% 100),
    # longitude
    V3 = (V3 - (V3 %/% 100) * 100) / 60 + (V3 %/% 100),
    # altitude
    V4 = ifelse(V4 > 100000, (V4 - 100000) * 0.1, V4 * 0.1),
    # 20-8 precipitation
    V8 = V8 * 0.1,
    # 8-20 precipitation
    V9 = V9 * 0.1,
    # 20-20 precipitation
    V10 = V10 * 0.1
  )]
  x
}

#' @rdname format_data
#' @export
format_data.SSD <- function(x, ...){
  x$data = x$data[, lapply(.SD, cmdcr::remove_missing_data)]
  x$data[, ':='(
    # latitude
    V2 = (V2 - (V2 %/% 100) * 100) / 60 + (V2 %/% 100),
    # longitude
    V3 = (V3 - (V3 %/% 100) * 100) / 60 + (V3 %/% 100),
    # altitude
    V4 = ifelse(V4 > 100000, (V4 - 100000) * 0.1, V4 * 0.1),
    # Sun hours
    V8 = V8 * 0.1
  )]
  x
}

#' @rdname format_data
#' @export
format_data.GST <- function(x, ...){
  x$data = x$data[, lapply(.SD, cmdcr::remove_missing_data)]
  x$data[, ':='(
    # latitude
    V2 = (V2 - (V2 %/% 100) * 100) / 60 + (V2 %/% 100),
    # longitude
    V3 = (V3 - (V3 %/% 100) * 100) / 60 + (V3 %/% 100),
    # altitude
    V4 = ifelse(V4 > 100000, (V4 - 100000) * 0.1, V4 * 0.1),
    # average ground temperature
    V8 = ifelse(V8 < -10000, (V8 + 10000) * 0.1, V8 * 0.1),
    # maixmum ground temperature
    V9 = ifelse(V9 < -10000, (V9 + 10000) * 0.1, V9 * 0.1),
    # minimum ground temperature
    V10 = ifelse(V10 < -10000, (V10 + 10000) * 0.1, V10 * 0.1)
  )]
  x
}


