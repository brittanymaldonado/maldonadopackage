#' Visualizing trends for long-term datasets
#'
#' @param data data.frame to use
#' @return
#' @importFrom tidyverse
#' @export
#'

plot_data <- function(data, x, y, col, z){
  ggplot(data = data,
       mapping = aes(x = x, y = y, color = col)) +
  geom_line() +
  facet_wrap(vars(z)) +
  theme_bw()
}
