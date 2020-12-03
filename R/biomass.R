#' Calculating biomass by species by life stage for each year
#'
#' @param data data.frame to use
#' @param Species first column to group by
#' @param Stage second column to group by
#' @param Year third column to group by
#' @param Mass column that is summed
#' @return data frame with biomass for each life stage of a species for each year
#' @export
#'

biomass <- function(data, Species, Stage, Year, Mass){
  summed <- data %>% group_by(Species, Stage, Year) %>% summarise(Biomass = sum(Mass, na.rm = TRUE))
  return(summed)
}
