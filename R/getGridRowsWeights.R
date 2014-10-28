#' @include getSmallGrid.R
#' @export
#' @title
#' getGridRowsWeights

#' @description
#' This utility function gets weights of the rows 
#' of a \code{SpatialGridDataFrame}
#' (converted to a \code{raster brick} before passing to this
#' function) corresponding to a particular individual polygon.
#' 
#' @details
#' This function is used in the process of aggregating when 
#' computation speed is a concern. (the function \code{over()} in 
#' the \code{sp} package, for example, takes a very long time 
#' with no log file
#' provided). These can then be looped to overlay a \code{SpatialGridDataFrame} on
#' a \code{SpatialPolygonsDataFrame}. The rows and weights are stored 
#' for repeated overlays of many \code{SpatialGridDataFrame} objects.
#' rows = c(1,2); weights = c(0.4,0.6) would indicate
#' that the polygon
#' 
#' @param geoPoly The \code{SpatialPolygonsDataFrame} containing 
#' the polygon to be overlaid.
#' @param brickGrid The \code{SpatialGridDataFrame} to be overlaid, 
#' converted to a \code{raster brick} object.
#' @param geoValue \code{numeric} or \code{character} that 
#' identifies the polygon within geoPoly.
#' @param geoPolyVar The variable in \code{geoPoly} used to search for
#'               \code{geoValue}.
#' @param gridArcMin The cell size in \code{brickGrid} in arcminutes.
#'               
#' @return A \code{data.frame} with columns \code{rows} and \code{weights}.
#' \code{rows} are the rows corresponding to grid cells that the
#' polygon overlaps with. \code{weights} are the weights for each
#' of these cells to account for partial overlap. 
getGridRowsWeights <- function(geoValue, brickGrid, geoPoly, geoPolyVar, gridArcMin) {
  # Requires that 'id' is a column in brickGrid\
  if (!("id" %in% brickGrid@data@names)) {
    stop("\"id\" not included in a column in brickGrid. Try: \n spGridDf$id <- 1:length(spGridDf), before bricking")
  }
  if (is.na(geoValue)) {
    return(NA)
  } else {
    # subset the given SpatialPointsPolygon to the zip
    thisPoly <- tryCatch(geoPoly[geoPolyVar == geoValue, ], error = function(e) NA)
    
    if (suppressWarnings(is.na(thisPoly))) {
      return(NA)
    } else {
      grid1 <- getSmallGrid(thisPoly = thisPoly, brick = brickGrid, gridArcMin = gridArcMin)
      
      # Convert the poly to a PolySet from PBSMapping
      tmp1 <- SpatialPolygons2PolySet(thisPoly)
      
      # Convert the grid to a poly then to a PolySet
      tmp2 <- Grid2Polygons(grid1, zcol = "id")
      tmp3 <- SpatialPolygons2PolySet(tmp2)
      
      # Fix up the projection
      attr(tmp1, "projection") <- "LL"
      
      # Get the intersections
      tmp4 <- joinPolys(tmp1, tmp3, operation = "INT")
      
      # rollup = 2 subtracts holes.
      suppressMessages(suppressWarnings(areasJoin <- calcArea(tmp4, rollup = 2)))
      suppressMessages(suppressWarnings(areasGrid <- calcArea(tmp3, rollup = 1)))
      
      # Non zero grid elements
      nonZeroGridElements <- areasJoin$PID
      
      # Now pull out the areas proportions and the large grid rows
      weights <- areasJoin$area/areasGrid$area[nonZeroGridElements]
      rowsInBigGrid <- grid1$id[nonZeroGridElements]
      
      # Some grid cells have more than one polygon joins so this aggregates them and actually turns them into weights.
      denom <- sum(weights)
      weights <- aggregate(x = weights, by = list(rowsInBigGrid), FUN = sum)$x/denom
      
      return(data.frame(rows = unique(rowsInBigGrid), weights = weights))
    }
  }
} 
