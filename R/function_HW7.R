#' Creating a year column from dates for easy grouping and visualization
#'
#' @param data data.frame to use
#' @return same data.frame but now with a column that just has Year for each entry
#' @importFrom lubridate mdy
#' @importFrom assertthat assert_that
#' @export
#'

library(tidyverse)
library(lubridate)
library(assertthat)

parse_dates <- function(data){
  assert_that(is.data.frame(data))
  data$DATE <- mdy(data$Date)
  data_yr <- data %>% mutate(Year = year(DATE))
  data_yr_mo <- data_yr %>% mutate(Month = month(DATE)) %>% select(-Date)
  return(data_yr_mo)
}
