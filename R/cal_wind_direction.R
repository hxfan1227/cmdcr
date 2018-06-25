NULL
#' Covert wind direction code into degrees based on the base direction.
#' @param x Numeric. Wind direction code to be coverted.
#' @param base_direction Character. Direction of 0 degree. Should be one of (E)ast, (W)est, (S)outh and (N)orth.
#' @return Wind direction in degree format.
#' @export
cal_wind_direction <- function(x, base_direction = c("North", "South", "East", "West")){
  base_direction <- match.arg(base_direction,
                              choices = c("North", "South", "East", "West"))
  x <- ifelse(x == 1, 360, (x - 1) * 22.5)
  switch(base_direction,
         "North" = x,
         "South" = ifelse((x + 180) > 360, x + 180 - 360, x + 180),
         "East" = ifelse((x + 90) > 360, x + 90 - 360, x + 90),
         "West" = ifelse((x + 270) > 360, x + 270 - 360, x + 270))
}
