#' find extreme temp per year
#'
#'
#' @param tmax_c (C)
#' @param tmin_c (C)
#' @param year
#'
#' @return extreme temps

extreme_temp = function(temperature_max, temperatue_min, year) {

  #combine inputs to one data frame

  ex = data.frame(temperature_max, temperature_min, year)

  #extract maximum and minimum

  extreme_max = ex %>%
    select(year, temperature_max) %>%
    group_by(year) %>%
    summarize(max = max(temperature_max, na.rm = TRUE))

  extreme_min = ex %>%
    select(year, temperature_min) %>%
    group_by(year) %>%
    summarize(min = min(temperature_min, na.rm = TRUE))

  return(extreme_max, extreme_min)
}

