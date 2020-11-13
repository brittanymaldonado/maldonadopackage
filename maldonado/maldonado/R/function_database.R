#' Calculate volume of shrub
#' 
#' @param length Length of the shrub(numeric, default = 1)
#' @param width Width of the shrub (numeric, default = 1)
#' @param height Height of the shrub (numeric, default = 1)
#' @return Volume of shrub (numeric)
#' 
#' @example 
#' calc_shrub_vol(length = 2, width = 3, height = 2)



calc_shrub_vol <- function(length = 1, width = 1, height = 1){volume <- length*width*height 
+ return(volume)}

#' Estimate mass of shrub
#' 
#' @param volume Volume of the shrub (numeric)
#' @return mass of the shrub (numeric)
#' 
#' @example 
#' est_shrub_mass(volume = 20)

est_shrub_mass <- function(volume){mass <- 2.65*volume^0.9
return(mass)}

#'Calculate density of shrub
#'
#'@param length Length of the shrub (numeric, default = 2)
#'@param width Width of the shrub (numeric, default = 4)
#'@param height Height of the shrub (numeric, default =1)
#'@return Density of the shrub (numeric)
#'
#'@example
#'shrub_density(legnth = 5, width = 6, height = 7)

shrub_density <- function(length = 2, width = 4, height = 1){
  vol <- calc_shrub_vol(length, width, height)
  mass <- est_shrub_mass(vol)
  den <- mass/vol
  return(den)}
