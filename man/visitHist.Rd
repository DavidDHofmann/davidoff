% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Functions.R
\name{visitHist}
\alias{visitHist}
\title{Retrieve the Visitation History of a Track}
\usage{
visitHist(line, raster)
}
\arguments{
\item{line}{\code{SpatialLines} or \code{SpatialLinesDataFrame} which depicts
a trajectory and is used to identify the visitation history.}

\item{raster}{\code{RasterLayer} containing categories which are used for the
visitation history}
}
\value{
\code{data.frame}
}
\description{
Function to get the visitation history
}
