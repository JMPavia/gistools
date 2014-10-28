#' @title
#' Shrinks a rasterbrick for faster computation
#' 
#' @description
#' Shrinks a rasterbrick to only those cells that immediately surround
#' a polygon.
#' @param thisPoly polygon that the cells will surround
#' @param brick bricked \code{SpatialGridDataFrame} that will be shrunk
#' @param gridArcMin cell size for \code{brick}
#' @return a raster brick object (subset of brick)
getSmallGrid <- function(thisPoly, brick, gridArcMin) {
  # This subsets a RasterBrick object, converted from a SpatialGridDataFrame using brick() from the 'raster' package, and
  # returns a rectangular smaller grid, around a given SpatialPolygonDataFrame of length one.
  bb <- bbox(thisPoly)
  gridArcDegree <- gridArcMin/60
  
  e <- extent(bb[1, 1] - gridArcDegree, bb[1, 2] + gridArcDegree, bb[2, 1] - gridArcDegree, bb[2, 2] + gridArcDegree)
  newGrid <- crop(brick, e)
  
  sge <- as(newGrid, "SpatialGridDataFrame")
  return(sge)
} 
