% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Functions.R
\name{sim2tracks}
\alias{sim2tracks}
\title{Coerce multiple Simulated Trajectories to SpatialLines}
\usage{
sim2tracks(simulation = NULL, crs = NULL, keep.data = F)
}
\arguments{
\item{simulation}{\code{data.frame} resulting from the function
\code{disperse()}}

\item{keep.data}{logical Should the input dataframe be conserved?}
}
\value{
\code{SpatialLinesDataFrame}
}
\description{
Function to coerce simulated coordinates to lines after a desired number of
steps. Basically a wrapper around \code{sim2tracks()}
}
