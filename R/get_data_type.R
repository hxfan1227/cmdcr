#' @importFrom stringr str_detect
NULL
#' Get the type and date of the meteorological variable according to the file name.
#' @param fname Character. The full name of the file to be processed.
#' @return \code{get_data_type} returns the type of the meteorological variable.
#'
#' \code{get_data_date} returns a character indicating the date of the meteorological variable.
#' @export
get_data_type <- function(fname){
  # check the validity of the file name
  fbasename <- basename(fname)
  if (!cmdcr::is_valid_fname(fname)) {
    stop(fname, " is not a valid file.\n")
  }
  return(stringr::str_split(fbasename, pattern = "-", simplify = T)[2])
}

#' @rdname get_data_type
#' @export
get_data_date <- function(fname){
  fbasename <- basename(fname)
  if(!cmdcr::is_valid_fname(fname)){
    stop(fname, " is not a valid file.\n")
  }
  return(stringr::str_extract_all(fbasename, "\\d{6}", simplify = T))
}
#' Check the validity of the file name.
#' @param fname Character. The full name of the file to be processed.
#' @return \code{TRUE} if the file name is valid and vice versa.
#' @export
is_valid_fname <- function(fname){
  fbasename <- basename(fname)
  return(
    stringr::str_detect(fbasename,
                        pattern = "SURF_CLI_CHN_MUL_DAY-(PRS-10004|TEM-12001|RHU-13003|PRE-13011|EVP-13240|SSD-14032|WIN-11002|GST-12030-0cm)-\\d{6}.TXT")
  )
}
