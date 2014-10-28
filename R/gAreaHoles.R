#' @export
#' @title Area of geometry with holes not correct
#' 
#' @description
#' Fixes holes and calculates the area of the given geometry
#' 
#' @param spgeom sp object as defined in packages sp
#' @param byid logical determining if the function should be 
#' applied across subgeometries (TRUE) or the entire object (FALSE)
#' 
#' @return Returns the area of the geometry in the units 
#' of the current projection. By definition non-[MULTI]POLYGON 
#' geometries have an area of 0. The area of a POLYGON is 
#' the area of its shell less the area of any holes. Note 
#' that this value may be different from the area slot of 
#' the Polygons class as this value does not subtract 
#' the area of any holes in the geometry.
gAreaHoles <- function(spgeom, byid=FALSE){
  spgeom <- createSPComment(spgeom)
  slot(spgeom, "polygons") <- lapply(slot(spgeom, "polygons"), checkPolygonsHoles)
  spgeomUTM <- spTransform(spgeom, CRS("+proj=utm +zone=24 +units=km"))
  gArea(spgeomUTM, byid=byid)
}