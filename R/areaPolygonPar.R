#' @export
#' @title
#' Parallel version of areaPolygon from geosphere.
#' @description
#' Compute the area of a polygon on a sphere. Polygons should not self-intersect. Parallel version of areaPolygon.
#' @param poly a \code{SpatialPolygonsDataFrame}
#' @return a vector of areas. Progress is tracked using a log file in the working directory.
areaPolygonPar <- function(cl, poly){
  registerDoParallel(cl)
  file.remove("log.txt")
  areas <- foreach(polyNum = 1:NROW(poly), .combine = c, 
                   .packages = c("geosphere")) %dopar% {
  sink("log.txt", append = TRUE)
  cat(paste("areaPolygonPar.R Starting iteration", polyNum, "\n"))
  sink()
  tmp <- areaPolygon(poly[polyNum,])
  return(tmp)
}
areas
}
