#' @include getGridRowsWeights.R
#' @include getSmallGrid.R
#' @include gridRowsWeightsSpPoly.R
#' @export
#' 
#' @title 
#' Spatial overlay for grid to polygon
#' 
#' @description
#' Adds a variable to a \code{SpatialPolygonsDataFrame} corresponding to a variable in a SpatialGridDataFrame.
#' This function is appropriate for a single grid overlay. Use \code{gridRowsWeightsSpPoly} and run a
#' loop over the grids if there is more than one grid.
#' 
#' @param parallel cluster object
#' @param spgrid \code{SpatialGridDataFrame} to be overlaid
#' @param poly \code{SpatialPolygonsDataFrame} to be added to
#' @param idPoly vector in \code{poly} that contains a unique polygon
#' identifier. Pass the column itself to the function - not the name.
#' @param gridVar \code{character} name of the variable to be extracted from \code{spgrid}
#' @param polyVar \code{character} name of the new variable to be added to \code{poly}. Default is \code{gridVar}. 
#' 
#' @return a \code{SpatialPolygonsDataFrame}. This will be \code{poly} with an extra column.
overGridPolyPar <- function(cl, spgrid, poly, idPoly = idPoly, gridVar, 
                            polyVar = gridVar){
  poly$ID35a0208022f <- 1:length(poly)
  rowsWeights <- gridRowsWeightsSpPolyPar(cl = cl, spgrid = spgrid, poly = poly, 
                                       idPoly = poly$ID35a0208022f)
  poly$ID35a0208022f <- NULL
  
  # Use a temporary name 
  poly$AGHXeMzfjF6dcVOPVJ2 <- sapply(1:length(poly), FUN = function(x){
    spgrid@data[rowsWeights[[x]]$rows,gridVar] %*% rowsWeights[[x]]$weights
  })
  names(poly)[which(names(poly) == "AGHXeMzfjF6dcVOPVJ2")] <- polyVar
  poly
}

#' @include getGridRowsWeights.R
#' @include getSmallGrid.R
#' @include gridRowsWeightsSpPoly.R
#' @export
#' 
#' @title 
#' Spatial overlay for grid to polygon
#' 
#' @description
#' Adds a variable to a \code{SpatialPolygonsDataFrame} corresponding to a variable in a SpatialGridDataFrame.
#' This function is appropriate for a single grid overlay. Use \code{gridRowsWeightsSpPoly} and run a
#' loop over the grids if there is more than one grid.
#' 
#' @param parallel cluster object
#' @param spgrid \code{SpatialGridDataFrame} to be overlaid
#' @param poly \code{SpatialPolygonsDataFrame} to be added to
#' @param idPoly vector in \code{poly} that contains a unique polygon
#' identifier. Pass the column itself to the function - not the name.
#' @param gridVar \code{character} name of the variable to be extracted from \code{spgrid}
#' @param polyVar \code{character} name of the new variable to be added to \code{poly}. Default is \code{gridVar}. 
#' 
#' @return a \code{SpatialPolygonsDataFrame}. This will be \code{poly} with an extra column.
overGridPoly <- function(spgrid, poly, idPoly = idPoly, 
                         gridVar, polyVar = gridVar){
  poly$ID35a0208022f <- 1:length(poly)
  rowsWeights <- gridRowsWeightsSpPoly(spgrid = spgrid, poly = poly, 
                                       idPoly = poly$ID35a0208022f)
  poly$ID35a0208022f <- NULL
  
  # Use a temporary name 
  poly$AGHXeMzfjF6dcVOPVJ2 <- sapply(1:length(poly), FUN = function(x){
    spgrid@data[rowsWeights[[x]]$rows,gridVar] %*% rowsWeights[[x]]$weights
  })
  names(poly)[which(names(poly) == "AGHXeMzfjF6dcVOPVJ2")] <- polyVar
  poly
}
