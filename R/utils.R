#' furthest
#'
#' @param polygon An sf polygon
#' @param axis 1 is north/south, 2 is east/west
#' @param reverse will return the furthest east or south unless this is TRUE
#'
#' @return An integer defining which coordinate is furthest on the axis
#' @export
#'
#' @examples
#' 
furthest <- function(polygon, axis = 1, reverse = FALSE) {
  stopifnot(inherits(polygon, "sf"))

  coords <- sf::st_coordinates(poly)[1:4, axis]
  if(!reverse) return(which.min(coords)) else which.max(coords)
}


#' Make points
#'
#' @param dim1 
#' @param dim2 
#' @param dim1x 
#' @param dim2x 
#' @param dim1y 
#' @param dim2y 
#' @param refx 
#' @param refy 
#' @param moddim1 
#' @param moddim2 
#'
#' @return A length two vector
#'
#' @examples
make_points <- function(dim1, dim2, dim1x, dim2x, dim1y, dim2y, refx, refy, moddim1 = 0, moddim2 = 0) {
  x <- (dim1 - moddim1) * dim1x + (dim2 - moddim2) * dim2x + refx
  y <- (dim1 - moddim1) * dim1y + (dim2 - moddim2) * dim2y + refy
  return(c(x = x, y = y))
}

#' Make a polygon
#'
#' @param dim1 
#' @param dim2 
#' @param dim1x 
#' @param dim2x 
#' @param dim1y 
#' @param dim2y 
#' @param refx 
#' @param refy 
#' @param coords 
#'
#' @return
#'
#' @examples
make_polygon <- function(dim1, dim2, dim1x, dim2x, dim1y, dim2y, refx, refy, coords = NULL) {
  stopifnot(!is.null(coords))
  coords_out <- purrr::map(coords, ~ make_points(dim1, dim2, dim1x, dim2x, dim1y, dim2y, refx, refy, moddim1 = .x[1], moddim2 = .x[2]))
  sf::st_polygon(list(matrix(unlist(coords_out), ncol = 2, byrow = TRUE)))
}