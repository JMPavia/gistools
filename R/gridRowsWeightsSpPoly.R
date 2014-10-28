#' @include getGridRowsWeights.R
#' @include getSmallGrid.R
#' @export

#' @title
#' Extract the rows and weights used for aggregating a SpatialGridDataFrame
#' to a SpatialPolygonsDataFrame
#' 
#' @description
#' Computes the rows and weights for overlaying a \code{SpatialGridDataFrame} on 
#' all individual polygons in a \code{SpatialPolygonsDataFrame}. Rows refers
#' to the rows in the SpatialGridDataFrame; weights are the weights that apply
#' to the corresponding rows. The procedure is parallelized by default. If
#' paralellized, progress is reported in a text file named 'log.txt' in the
#' working directory
#' 
#' @param grid \code{SpatialGridDataFrame} to be aggregated to a polygon
#' @param poly \code{SpatialPolygonsDataFrame} that \code{grid} is to be 
#' aggregated t.
#' @param idPoly vector in \code{poly} that contains a unique polygon
#' identifier. Pass the column itself to the function - not the name.
#' @param gridArcMin
#' @return A list of dataframes with columns \code{rows} and \code{weights}.
#' \code{rows} are the rows corresponding to grid cells that the
#' polygon overlaps with. \code{weights} are the weights for each
#' of these cells to account for partial overlap. Each element of the list
#' corresponds to an individual element of \code{spPoly}. 
gridRowsWeightsSpPoly <- function(grid, poly, idPoly, gridArcMin) {
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  file.remove("log.txt")
  grid$id <- 1:length(grid)
  brickGrid <- brick(grid)
  gridRowsWeights <- foreach(X = idPoly, .packages = c("sp", "raster", "PBSmapping", 
                                                       "maptools", "Grid2Polygons")) %dopar% 
    {
      sink("log.txt", append = TRUE)
      cat(paste("Starting iteration", X, "\n"))
      sink()
      tmp <- getGridRowsWeights(geoValue = X, brickGrid = brickGrid, 
                                geoPoly = poly, geoPolyVar = idPoly, 
                                gridArcMin = gridArcMin)
      return(tmp)
    }
  stopCluster(cl)
  return(gridRowsWeights)
}
