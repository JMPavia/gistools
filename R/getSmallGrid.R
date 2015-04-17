#' @title
#' Shrinks a rasterbrick for faster computation
#' 
#' @description
#' Shrinks a rasterbrick to only those cells that immediately surround
#' a polygon.
#' @param thisPoly polygon that the cells will surround
#' @param brick bricked \code{SpatialGridDataFrame} that will be shrunk
#' @param spgridGridSlot The \code{grid} slot from the given 
#' \code{SpatialGridDataFrame}
#' @return a raster brick object (subset of brick)
getSmallGrid <- function(thisPoly, brick, spgridGridSlot) {
  # This subsets a RasterBrick object, converted from a SpatialGridDataFrame using brick() from the 'raster' package, and
  # returns a rectangular smaller grid, around a given SpatialPolygonDataFrame of length one.
  bb <- bbox(thisPoly)
  
  cellX <- spgridGridSlot@cellsize[1]
  cellY <- spgridGridSlot@cellsize[2]
  
  e <- extent(bb[1, 1] - cellX, bb[1, 2] + cellX, 
              bb[2, 1] - cellY, bb[2, 2] + cellY)
  newGrid <- crop(brick, e)
  
  sge <- as(newGrid, "SpatialGridDataFrame")
  return(sge)
} 
