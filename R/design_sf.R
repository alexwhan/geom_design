#' Make sf from design
#'
#' @param design A data.frame
#' @param polygon An sf polygon
#' @param dim1_var The (unquoted) name of the variable describing the first dimension of the design
#' @param dim2_var The (unquoted) name of the variable describing the second dimension of the design
#'
#' @return
#' @export
#'
#' @examples
design_sf <- function(design, polygon, dim1_var = RUN, dim2_var = RANGE) {
  dim1_quo <- rlang::enquo(dim1_var)
  dim1_var <- deparse(substitute(dim1_var))
  dim2_quo <- rlang::enquo(dim2_var)
  dim2_var <- deparse(substitute(dim2_var))
  #check that dim1_var and dim2_var terms are in the design
  stopifnot(all(c(dim1_var, dim2_var) %in% names(design)))
  
  #check that dim1_var and dim2_var terms are integers
  stopifnot(inherits(design[[dim1_var]], "integer"),
            inherits(design[[dim2_var]], "integer"))
  
  #check if the dim1_var and dim2_var terms give unique combinations
  stopifnot(nrow(design) == nrow(design[, c(dim1_var, dim2_var)]))
  
  ndim1_var <- max(design[[dim1_var]])
  ndim2_var <- max(design[[dim2_var]])
  
  #find the most extreme south, west, north points to calculate the deltas for dim1 and dim2
  south <- furthest(polygon, 1)
  west <- furthest(polygon, 2)
  north <- furthest(polygon, 1, TRUE)
  
  coords <- sf::st_coordinates(polygon)[,1:2]
  
  #calculate delta for each dim for x/y
  dim1x <- (coords[west, 1] - coords[south, 1]) / ndim1_var
  dim2x <- (coords[north, 1] - coords[west, 1]) / ndim2_var
  dim1y <- (coords[west, 2] - coords[south, 2]) / ndim1_var
  dim2y <- (coords[north, 2] - coords[west, 2]) / ndim2_var
  
  #all output coords are based on this point
  minx <- coords[south, 1]
  miny <- coords[south, 2]
  
  #A list used to make polygongons
  corners <- list(
    c1 = c(1, 1),
    c2 = c(0, 1),
    c3 = c(0, 0),
    c4 = c(1, 0),
    c5 = c(1, 1)
  )
  
  design_rw <- dplyr::rowwise(design)
  design_list <- dplyr::mutate(design_rw, 
                        polygon = list(make_polygon(!!dim1_quo, !!dim2_quo, dim1x, dim2x, dim1y, dim2y, minx, miny, coords = corners)))
  return(sf::st_sf(design_list))
      
}
