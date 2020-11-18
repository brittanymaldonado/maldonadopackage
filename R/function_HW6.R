#' Calculating sum of data grouped by two categories
#' 
#' @param data data.frame to use
#' @param group1 first column to group by
#' @param group2 second column to group by
#' @param tosum what to take the sum of
#' @return data frame with sums of a numerical category grouped by two other categories
#' 
#' @example 
#' sum_g1g2(agp, Species, Year, Mass)


sum_g1g2 <- function(data, group1, group2, tosum){
  summed <- data %>% group_by(group1, group2) %>% summarise(sumg1g2 = sum(tosum, na.rm = TRUE)) 
  return(summed)
}


