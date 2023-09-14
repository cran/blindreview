\name{plotdiag.blind.all}
\alias{plotdiag.blind.all}
\title{Plot Results of Blind Review}
\description{
Plots results from running brMask on a dataset using plotting functions found in 
   blindreview and forsearch packages.}
\usage{
plotdiag.blind.all(object, treatnum, mt=" ", st=" ", cc=NULL, ccrand=NULL, 
    objnames=FALSE, verbose=TRUE)
}
\arguments{
  \item{object}{A brMask object}
  \item{treatnum}{Identifying number of plot, for use in graphs and as part of
      file name}
  \item{mt}{Main title of graph}
  \item{st}{Subtitle of graph}
  \item{cc}{Vector of integers of fixed parameters to include in plot or NULL to
     include all fixed parameters}
  \item{ccrand}{Vector of integers of random parameters to include in plot or
     NULL to include all random parameters}
  \item{objnames}{Logical. TRUE causes names of object to be printed}   
  \item{verbose}{If TRUE, indicates beginning and end of function}
}
\value{None returned, creates graphs only}
\author{William R. Fairweather}
\keyword{ hplot }
