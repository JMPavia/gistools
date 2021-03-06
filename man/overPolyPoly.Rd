% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/overPolyPoly.R
\name{overPolyPoly}
\alias{overPolyPoly}
\title{Aggregate an attribute of a polygon onto another polygon.}
\usage{
overPolyPoly(polyX, idX, polyY, varY, FUN, noIntersectReturn)
}
\arguments{
\item{polyX}{a \code{SpatialPolygonsDataFrame} to be matched to.
\code{polyX} is returned with extra columns.}

\item{idX}{id variable in \code{polyX}}

\item{polyY}{a \code{SpatialPolygonsDataFrame} containing variables
to be averaged.}

\item{varY}{character vector of variable(s) in \code{polyY}
to be added to \code{polyX}. To be clear,}

\item{FUN}{function to be applied to the weighted values. For example,
\code{sum}, along with \code{varY=area}, would be used if the user
wanted to sum the areas of one \code{SpatialPolygonsDataFrame} over
another. User can specify a vector of functions up to
the length of \code{varY} e.g. \code{FUN=c(fun1, fun2)}.}

\item{noIntersectReturn}{what to return for elements that have no
intersection, specified for each element of varY.
Must be same length as varY.}
}
\value{
a \code{SpatialPolygonsDataFrame} - \code{polyX} with variable(s)
\code{varY} added.
}
\description{
This function calculates an area-weighted sum of an attribute of one polygon, contained
in each of the elements of another polygon.
}
\details{
#

This function relies on the joinPolys function provided by PBSMapping and overlays one
attribute of a polygon onto another. The function loops through the elements of
\code{polyX}. At each stage, the function identifies the elements of \code{polyY}
that overlap the polygon from \code{polyX} and averages the values of \code{varY}
on an area-weighted basis. \code{polyX} is returned with extra columns.
\code{varY} can be a \code{data.frame} of several variables.
***Consider recoding with the join operation on polysets. This seems to
result in a ~30% reduction in computation time for this stage. Requires a ~2 min
conversion of \code{polyY} to a PolySet as there is no nice way (that I know of)
to subset a \code{SpatialPolygonsDataFrame} by a box to speed things up.
}

