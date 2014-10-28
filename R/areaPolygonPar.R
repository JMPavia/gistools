#' @export
#' @title
#' Parallel version of areaPolygon from geosphere.
#' @description
#' Compute the area of a polygon on a sphere. Polygons should not self-intersect. Parallel version of areaPolygon.
#' 
#' @param poly a \code{SpatialPolygonsDataFrame}
#' @return a vector of areas
areaPolygonPar <- function(poly){
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  file.remove("log.txt")
  areas <- foreach(polyNum = 1:NROW(poly), .combine = c, .packages = c("geosphere")) %dopar% 
{
  sink("log.txt", append = TRUE)
  cat(paste("areaPolygonPar.R Starting iteration", polyNum, "\n"))
  sink()
  tmp <- areaPolygon(poly[polyNum,])
  return(tmp)
}
stopCluster(cl)
areas
}
