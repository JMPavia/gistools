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
#' @param grid \code{SpatialGridDataFrame} to be overlaid
#' @param poly \code{SpatialPolygonsDataFrame} to be added to
#' @param idPoly vector in \code{poly} that contains a unique polygon
#' identifier. Pass the column itself to the function - not the name.
#' @param gridVar \code{character} name of the variable to be extracted from \code{grid}
#' @param gridArcMin size of the cell in \code{grid}. A future version could remove this.
#' @param polyVar \code{character} name of the new variable to be added to \code{poly}. Default is \code{gridVar}. 
#' 
#' @return a \code{SpatialPolygonsDataFrame}. This will be \code{poly} with an extra column.
overGridPoly <- function(grid, poly, idPoly = idPoly, gridArcMin, gridVar, polyVar = gridVar){
  rowsWeights <- gridRowsWeightsSpPoly(grid = grid, poly = poly, idPoly = idPoly,gridArcMin = gridArcMin)
  poly$AGHXeMzfjF6dcVOPVJ2 <- sapply(1:length(poly), FUN = function(x){
    grid@data[rowsWeights[[x]]$rows,gridVar] %*% rowsWeights[[x]]$weights
  })
  names(poly)[which(names(poly) == "AGHXeMzfjF6dcVOPVJ2")] <- polyVar
  poly
}