#' find mean percip per year
#'
#'
#' @param precipication
#' @param year
#'
#' @return extreme temps

mean_precip = function(precip, year) {

  #combine inputs to one data frame

  mean_p = data.frame(precip, year)

  #extract mean

  mean_precip = mean_p %>%
    group_by(year) %>%
    summarize(mean = mean(precip, na.rm = TRUE))


  return(mean_precip)
}

