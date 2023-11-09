\name{plotdiag.blind.fixed}
\alias{plotdiag.blind.fixed}
\title{Plot Diagnostic Statistics of Fixed Coefficients in Blind Review 
}
\description{
Plot output from forsearch_xxx to show change in fixed coefficients as the number of 
observations in the forward search procedure increases. Save plot in folder containing 
working directory. 
}
\usage{
plotdiag.blind.fixed(forn, coeff.codenums=NULL, maintitle = "Put main title here", 
subtitle = "Put subtitle here", caption="Put caption here",wmf = "Put_stored_name_here", 
Cairo=TRUE, printgraph=TRUE,legend = "Dummy legend name",  
verbose = TRUE)
}
\arguments{
  \item{forn}{Name of output file from forsearch_xxx
}
  \item{coeff.codenums}{Numeric vector of coefficients to include together on the plot. 
  Codes are output by identifyFixedCoeffs (for lm files) or by identifyCoeffs function 
  (for lme files)
}
  \item{maintitle}{Main title of plot
}
  \item{subtitle}{Subtitle of plot
}
  \item{caption}{Content of caption
}
  \item{wmf}{File name of stored plot; omit ".wmf"  
}
\item{Cairo}{TRUE causes use of Cairo graphics
}
  \item{printgraph}{TRUE causes graph to print to file and
          closes device
}
  \item{legend}{Name of legend
}
  
  \item{verbose}{If TRUE, indicates beginning and end of function
}
}
\value{Process and plot fixed coefficient statistics from forsearch_lm, forsearch_lme,
        forsearch_glm, or forsearch_cph.
}
\references{
Atkinson, A and M Riani. Robust Diagnostic Regression Analysis, Springer, New York, 2000.
}
\author{William R. Fairweather
}
 \keyword{ hplot }
