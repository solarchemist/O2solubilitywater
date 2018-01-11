#' Vapour pressure of water
#'
#' Vapour pressure of water as a function of temperature
#' This function returns the vapour pressure of water at the given
#' temperature(s) from the water::vapourwater dataset.
#'
#' @param temperature numeric vector, in degrees Celsius
#'
#' @return vapour pressure of water, in kilopascal
#' @export
#'
#' @examples
#' \dontrun{
#' VapourPressureWater(45)
#' VapourPressureWater(c(20, 25, 45, 60))
#' }
VapourPressureWater <- function(temperature) {
   data <- water::vapourwater
   # if T outside range in data, warn the user
   if (any(temperature < min(data$temperature)) | any(temperature > max(data$temperature))) {
      warning("At least one supplied temperature is outside the data range (",
           paste(range(data$temperature), collapse = " - "),
           " Celsius). Returning NAs for those values.")
   }
   # The vapourwater dataset only contains data for every degree celsius (or less),
   # so we use the interpolation function to fill in the rest.
   # Note that approx(rule = 1) returns NAs for input outside the data range.
   pressure <-
      stats::approx(x = data$temperature,
                    y = data$pressure,
                    method = "linear",
                    rule = 1,
                    xout = temperature)$y
   return(pressure)
}
