#' Calculates area of a square
#'
#' @param Side_length length of a side of the square
#' @return area_of_square
#'
#' @export
#'


square_calc <- function(side_length){
  assertthat::assert_that(is.numeric(side_length))
  area <- side_length ^ 2
  assert_that(sqrt(area) == side_length)
  return(area)
}
