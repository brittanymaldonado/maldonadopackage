#' Visualizing trends for long-term datasets
#'
#' @param data data.frame to use
#' @return plot with biomass by year with species colored
#' @importFrom ggplot2 ggplot
#' @export
#'

plot_data <- function(data, Year, Biomass, Species){
  ggplot(data = data,
       mapping = aes(x = Year, y = Biomass, color = Species)) +
  geom_line() +
  labs(title = "Yearly biomass", x = "Year", y = "Biomass") +
  theme_bw()
}
