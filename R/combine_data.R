#' @import data.table
#' @import plyr
#' @importFrom stringr str_detect
NULL
#' Combine meteorological data into a single file.
#' @param flist Character. List of file names to be processed.
#' @param outdir Character. Directory to store the files.
#' @return \code{NULL}
#' @export
combine_data <- function(flist,
                         outdir){
  if (!all(cmdcr::is_valid_fname(flist))){
    stop("Please check files below:\n", paste(flist[!cmdcr::is_valid_fname(flist)], collapse = "\n"))
  }
  if(missing(outdir)){
    outdir <- file.path(dirname(flist)[1], "combine")
    message("outdir is missing\nUsing ", outdir, " instead.")
  }
  data.types <- plyr::aaply(flist, 1, cmdcr::get_data_type)
  plyr::a_ply(data.types, 1, combine_data_, flist = flist, outdir = outdir)
  return(NULL)
}

#' Combine meteorological data into a single file.
#' @param dtype Character. Type of the meteorological variable.
#' @param flist  Character. List of file names to be processed.
#' @param outdir Character. Directory to store the files.
#' @param subfolder Logical. \code{True} will create the subfolder according to the data type (obtained by \code{\link[cmdcr]{get_data_type}}).
#' @param maxsize Numeric. Maximum size of data to be read into memory. Default is 512 Mb.
#' Note that if the total size of files exceed \code{maxisize}. The function will run in append mode.
#' Please refer to \code{\link[data.table]{fwrite}} for details.
combine_data_ <- function(dtype, flist, outdir, subfolder = T, maxsize = 512){
  f.list <- flist[stringr::str_detect(flist, dtype)]
  f.size <- sum(file.size(f.list)/1024^2)
  if (f.size <= maxsize){
    dt.list <- plyr::alply(f.list, 1, data.table::fread)
    dt <- do.call(plyr::rbind.fill, dt.list)
    data.table::setDT(dt)
    if (subfolder) {
      dir.create(file.path(outdir, dtype))
      data.table::fwrite(dt, file.path(outdir, dtype, paste0(dtype, ".txt")))
    } else{
      return(dt)
    }
  } else{
    message("Total size of files is larger than ", maxsize, " Mb\n")
    data.table::fwrite(data.table::fread(f.list[1]), file.path(outdir, dtype, paste0(dtype, ".txt")))
    plyr::a_ply(f.list[-1], 1, .fun = function(x){
      x_ <- data.table::fread(x)
      data.table::fwrite(x_, file.path(outdir, dtype, paste0(dtype, ".txt")), append = T)
    })
  }
}
