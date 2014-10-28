#' @export
#' @include gAreaHoles.R
#' @title
#' Aggregate an attribute of a polygon onto another polygon.
#' @description
#' This function calculates an area-weighted sum of an attribute of one polygon, contained
#' in each of the elements of another polygon.
#' 
#' @details
#' This function relies on the joinPolys function provided by PBSMapping and overlays one
#' attribute of a polygon onto another. The function loops through the elements of
#' \code{polyX}. At each stage, the function identifies the elements of \code{polyY}
#' that overlap the polygon from \code{polyX} and averages the values of \code{varY}
#' on an area-weighted basis. \code{polyX} is returned with extra columns. 
#' \code{varY} can be a \code{data.frame} of several variables.
#' ***Consider recoding with the join operation on polysets. This seems to
#' result in a ~30% reduction in computation time for this stage. Requires a ~2 min
#' conversion of \code{polyY} to a PolySet as there is no nice way (that I know of)
#' to subset a \code{SpatialPolygonsDataFrame} by a box to speed things up.
#' 
#' @param polyX a \code{SpatialPolygonsDataFrame} to be matched to.
#' \code{polyX} is returned with extra columns.
#' @param idX id variable in \code{polyX}
#' @param polyY a \code{SpatialPolygonsDataFrame} containing variables 
#' to be averaged.
#' @param varY character vector of variable(s) in \code{polyY} 
#' to be added to \code{polyX}. To be clear,
#' @param FUN function to be applied to the weighted values. For example,
#' \code{sum}, along with \code{varY=area}, would be used if the user 
#' wanted to sum the areas of one \code{SpatialPolygonsDataFrame} over
#' another. User can specify a vector of functions up to
#' the length of \code{varY} e.g. \code{FUN=c(fun1, fun2)}.
#' @param noIntersectReturn what to return for elements that have no
#' intersection, specified for each element of varY. 
#' Must be same length as varY.
#'               
#' @return a \code{SpatialPolygonsDataFrame} - \code{polyX} with variable(s)
#' \code{varY} added.


overPolyPoly <- function(polyX, idX, polyY, varY, FUN, noIntersectReturn) {
  polyY$id <- 1:NROW(polyY)
  
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  ptm <- proc.time()
  file.remove("log.txt")
  YRowsWeights <- foreach(X = 1:length(idX), .export = "gAreaHoles", .packages = c("sp", "raster", 
                                                           "maptools", "rgeos", "rgdal"
  )) %dopar% {
    sink("log.txt", append = TRUE)
    cat(paste("Starting iteration", X, "\n"))
    sink()
    if (is.na(idX[X])) {
      return(NA)
    }
    # subset the given SpatialPointsPolygon to the zip
    thisPoly <- tryCatch(polyX[idX == idX[X], ], error = function(e) NA)
    
    if (suppressWarnings(is.na(thisPoly))) {
      return(NA)
    }
    
    # 3.019 seconds
    joinTmp <- gIntersection(polyY, thisPoly, byid=TRUE, id=as.character(1:length(polyY)))
    if (is.null(joinTmp)){
      # This is returned if there are no polygons that
      # intersect.
      return("No intersections")
    }
    joinY <- as(joinTmp, "SpatialPolygonsDataFrame")
    
    joinY$id <- as.numeric(row.names(joinY))

    smallY <- polyY[joinY$id,]
    
    # Don't be alarmed that the IDs don't match
    # the joinY IDs come from row numbers of polyY
    # while the smallY IDs come from ID slots of polyY
    areasY <- gAreaHoles(smallY, byid=TRUE)
    areasJoin <- gAreaHoles(joinY, byid=TRUE)
    
    # Now pull out the area proportions and the polyY rows
    proportions <- areasJoin/areasY
    
    rowsInPolyY <- smallY$id
    return(data.frame(rows = unique(rowsInPolyY), proportions = proportions))
    #       # Convert the polys to PolySets from PBSMapping
    #       tmpXPolSet <- SpatialPolygons2PolySet(thisPoly)
    #             
    #       # Get the intersections
    #       # 2.192 seconds
    #       sum(mean(timeit(tmpJoin <- joinPolys(tmpXPolSet, polyYPolySet, operation = "INT")))$total.time)
    #       
    #       # rollup = 2 subtracts holes.
    #       suppressMessages(suppressWarnings(areasJoin <- calcArea(tmpJoin, rollup = 2)))
    #       suppressMessages(suppressWarnings(areasY <- calcArea(xxx, rollup = 2)))
    #       
    #       # Add the polygon chunks for each element of smallY.
    #       joinAreas <- aggregate(x = areasJoin$area, by = list(areasJoin$PID), FUN = sum)$x
    #       
    #       # Now pull out the area proportions and the polyY rows
    #       proportions <- joinAreas/areasY$area
    #       
    #       # Non zero grid elements
    #       nonZeroYElements <- areasJoin$PID
    #       
    #       # Now pull out the areas proportions and the large grid rows
    #       weights <- areasJoin$area/areasY$area
    #       rowsInY <- tmpYPolSet$id[nonZeroGridElements]
    #       
    #       # Some grid cells have more than one polygon joins so this aggregates them and actually turns them into weights.
    #       denom <- sum(weights)
    #       weights <- aggregate(x = weights, by = list(rowsInBigGrid), FUN = sum)$x/denom
    
    #       return(data.frame(rows = unique(rowsInBigGrid), weights = weights))
  }
  stopCluster(cl)
  # Now have the rows and proportions in polyY.
  # Now to get the actual functions of polyY.
  for (i in 1:length(varY)){
    polyX@data[,varY[i]] <- sapply(1:length(idX), FUN = function(x){
      suppressWarnings(if (is.na(YRowsWeights[[x]])){
        return(NA)
      } else if (YRowsWeights[[x]] == "No intersections") {
        return(noIntersectReturn[i])
      } else {
        dataVar <- polyY@data[YRowsWeights[[x]]$rows,varY[i]] * YRowsWeights[[x]]$proportions
        return(c(FUN)[[i]](dataVar))
      })
    })
  }
  polyX
}
