\name{unmask}
\alias{unmask}
\title{Print Tables of Encoded and Original Variable IDs and Observation Numbers
}
\description{Prints tables resulting from masking performed by brMask function
     to permit user to identify potential outliers in their original identities
}
\usage{unmask(object, obsrange=NULL, verbose=TRUE)
}
\arguments{
  \item{object}{name of brMask object}
  \item{obsrange}{NULL or vector of integers, NULL causes entire data frame of
       observation numbers to be printed}
  \item{verbose}{TRUE causes function identifier to display before and after 
         run}
}
\value{None returned, printout only}
\author{William R. Fairweather}
\examples{
info4 <- system.file("extdata", "micebrM.R", package="blindreview");
brMaskOut <- source(info4);
print(brMaskOut);
brMaskOut <- brMaskOut[[1]];
unmask(brMaskOut)
}
\keyword{ manip }
