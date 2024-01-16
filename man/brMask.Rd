\name{brMask}
\alias{brMask}
\title{Enables Blind Review of Database}
\description{
Assigns identification randomly to one of the variables of the dataset as chosen
     by the user, say Treatment, and assigns random number to the observations 
     of the dataset. Reorders the observations. A file is created so that the 
     user can identify any outliers identified by the review in terms of their 
     original, unchanged values. 
}
\usage{brMask(data, blinded, verbose=TRUE) 
}
\arguments{
  \item{data}{Database to be evaluated}
  \item{blinded}{Character, name of variable to be blinded}
  \item{verbose}{TRUE causes function identifier to display before and after run}
}
\details{The first variable of the database must be Observation. The first 
      element of the brMask object is the database to be reviewed blindly.}
\value{LIST
   \item{Masked Dataframe}{Database with substitute variable and substitute
        observation numbers}
   \item{Randomization Date}{Date of randomization of blinded items}
   \item{Variable}{Codes for unblinding variable that was blinded}
   \item{Observations}{Codes for unblinding observation numbers}
   \item{Call}{Call to this function}
}
\references{
{Atkinson, A and M Riani. Robust Diagnostic Regression Analysis, Springer, New York, 2000.}
{Pinheiro, JC and DM Bates. Mixed-Effects Models in S and S-Plus, Springer, New York, 2000.}
{https://CRAN.R-project.org/package=forsearch}
{E9 Statistical Principles for clinical Trials, US Food & Drug Administration and 
     International Conference on Harmonization, 1998 }
}
\author{William R. Fairweather}
\examples{
Observation <- 1:14
Dose <- c(3.4,5.2,7,8.5,10.5,13,18,21,28,6.5,10,14,21.5,29)
Prep <- factor(c(rep(0,9),rep(1,5)))
Convulse <- c(0,5,11,14,18,21,23,30,27,2,10,18,21,27)
Total <- c(33,32,38,37,40,37,31,37,30,40,30,40,35,37)
NoConvulse <- Total-Convulse
mice <- data.frame(Observation,Dose,Prep,Convulse,Total,NoConvulse)
brMask(data=mice,blinded="Prep")
}
\keyword{ manip }
