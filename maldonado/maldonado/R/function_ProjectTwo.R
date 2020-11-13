#' Return summary of linear model fit
#' 
#' @param data data.frame to use
#' @param x first variable from data to use
#' @param y second variable from data to use
#' @return summary of linear model fit
#' 
#' @example 
#' summarized_lm(surveys_complete, weight, hindfoot_length)


summarized_lm <- function(data, x, y){
  
  lm <- lm(x ~ y, data) 
  sum <- summary(lm)
  return(sum)
}