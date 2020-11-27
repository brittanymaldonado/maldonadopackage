#' Creating a year column from dates for easy grouping and visualization
#'
#' @param data data.frame to use
#' @return same data.frame but now with a column that just has Year for each entry
#' @importFrom lubridate mdy
#' @importFrom assertthat assert_that
#' @export
#'


yearify <- function(data){
  assert_that(is.data.frame(data))
  data$Date1 <- mdy(data$Date)
data_yr <- data %>% mutate(Year = year(Date1))
return(data_yr)
}
