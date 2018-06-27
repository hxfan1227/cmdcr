#' @import data.table
NULL
#' Tidy the meteorological data.
#' @param x Charater. Full name of file to be processed. Generate with \code{\link[cmdcr]{read_fname}}.
#' @param base_direction Character. Direction of 0 degree. Should be one of (E)ast, (W)est, (S)outh and (N)orth.
#' @param outdir Character. Output directory.
#' @param prefix Character. Prefix of the output file name.
#' @param ... Not used yet.
#' @export
format_data <- function(x, ...){
  UseMethod("format_data")
}
#' @rdname format_data
#' @export
format_data.WIN <- function(x,
                            base_direction = "N",
                            outdir,
                            prefix = "F",
                            ...){
  dt = data.table::fread(x)
  dt = dt[, lapply(.SD, cmdcr::remove_missing_data)]
  dt[, ':='(
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
  if (missing(outdir)){
    return(dt)
  } else{
    if (dir.exists(outdir)){
      fwrite(dt, file.path(outdir, paste0(prefix, basename(x))))
    }
  }
}


