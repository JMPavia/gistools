% Generated by roxygen2 (4.0.2): do not edit by hand
\name{getSmallGrid}
\alias{getSmallGrid}
\title{Shrinks a rasterbrick for faster computation}
\usage{
getSmallGrid(thisPoly, brick, gridArcMin)
}
\arguments{
\item{thisPoly}{polygon that the cells will surround}

\item{brick}{bricked \code{SpatialGridDataFrame} that will be shrunk}

\item{gridArcMin}{cell size for \code{brick}}
}
\value{
a raster brick object (subset of brick)
}
\description{
Shrinks a rasterbrick to only those cells that immediately surround
a polygon.
}
