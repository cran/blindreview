\name{unmask}
\alias{unmask}
\title{Print Tables of Encoded and Original Variable IDs and Observation Numbers
}
\description{Prints tables resulting from masking performed by brMask function
     to permit user to identify potential outliers by their original identities
}
\usage{unmask(object, obsrange=NULL, verbose=TRUE)
}
\arguments{
  \item{object }{Name of brMask object}
  \item{obsrange }{NULL or vector of integers, NULL causes entire data frame of
       observation numbers to be printed}
  \item{verbose }{TRUE causes function identifier to display before and after 
         run}
}
\value{None returned, printout only}
\author{William R. Fairweather}
\keyword{ manip }
