\name{brMask}
\alias{brMask}
\title{Blind Review of Database Using Forward Search Procedures}
\description{
Assigns identification randomly to one of the variables of the dataset as chosen
     by the user, say Treatment, and assigns random number to the observations 
     of the dataset. Then runs the appropriate forward search function on the 
     blinded dataset. A file is created so that the user can identify any 
     outliers identified by the forward search procedure in terms of their 
     original, unchanged values. Output is primarily for plotting by the 
     plotdiag.blind.all function.
}
\usage{brMask(data, blinded, analysis, 
     initial.sample=1000, n.obs.per.level=1, skip.step1=NULL, fixed=NULL,
     lme.random=NULL, lme.formula=NULL,
     glm.estimate.phi=TRUE, glm.cobs=1, glm.response.cols=NULL, 
     glm.indep.cols=NULL, glm.formula=NULL, glm.binomialrhs=NULL, 
     glm.family=NULL, cph.formula.elements=NULL, cph.event.time=NULL, 
     cph.status=NULL, cph.x=NULL, cph.ties=NULL, cph.redunCorr=0.9,
     arguments=FALSE, verbose=TRUE) 
}
\arguments{
  \item{data}{Dataset to be evaluated}
  \item{blinded}{Character, name of variable to be blinded}
  \item{analysis}{Character, one of "lme", "lm", "glm", or "cph"}
  \item{initial.sample}{Number of observations in Step 1 of forward search}
  \item{n.obs.per.level}{Number of observations per level of (possibly crossed) 
        factor levels}
  \item{skip.step1}{NULL or a vector of integers for observations to be included
   in Step 1}
  \item{fixed}{Fixed effects formula as described in stats::lm}
  \item{lme.random}{Random effects formula as described in nlme::lme}
  \item{lme.formula}{a simplified formula of the form resp ~ cov | group where resp
       is the response, cov is the primary covariate, and group is the grouping
       factor, as in nlme::groupedData}
  \item{glm.estimate.phi}{TRUE causes phi to be estimated; FALSE causes phi to 
       be set = 1}
  \item{glm.cobs}{Number of observations to include in each inner subgroup of 
      Step 1}
  \item{glm.response.cols}{Vector of column numbers (2) of responses and 
      nonresponses}
  \item{glm.indep.cols}{Column number(s) of independent variables}
  \item{glm.formula}{Formula relating response to independent variables. Required
       except for family=binomial}
  \item{glm.binomialrhs}{Right-hand side of formula, as text object. Required for 
       family=binomial}
  \item{glm.family}{Error distribution and link}
  \item{cph.formula.elements}{Vector of names of independent variables to appear
        in a formula object}
  \item{cph.event.time}{Vector of event times, censored or not}
  \item{cph.status}{Vector indicator of event or censoring: 1 = event, 0 = censored, 
        same length as event.time}
  \item{cph.x}{Data frame of independent variables, with number of rows = length 
       of event.time, First column is Observation. Factor variables must be defined 
       in advance. Does not include event time or status.}
  \item{cph.ties}{Character string specifying the method of handling ties in event
       time, "efron", "breslow" or "exact", . See survival::coxph for definitions. 
       Default is "efron" if specified =NULL}
  \item{cph.redunCorr}{Level of correlation required before declare variables
       are redundant}
  \item{arguments}{Logical. TRUE causes display of arguments of forsearch_xxx
       function}
  \item{verbose}{TRUE causes function identifier to display before and after run}
}
\value{LIST, unnecessary elements for current analysis are NULL
  \item{Analysis}{"lme", "lm", "glm", or "cph"}
  \item{Unmask}{Data frame of original and masked values}
  \item{Rows in stage}{List of (masked) observation numbers in each stage}
  \item{Standardized residuals}{Matrix of errors at each stage}
  \item{Number of model parameters}{Rank of model}
  \item{Sigma}{Estimate of random error at final stage; used to standardize all 
     residuals}
  \item{Fixed parameter estimates }{Matrix of parameter estimates at each stage}
  \item{s^2 }{Estimate of random error at each stage}
  \item{Leverage }{Matrix of leverage of each observation at each stage}
  \item{Modified Cook distance }{Estimate of sum of squared changes in parameter
     estimates at each stage}
  \item{t statistics }{t statistics for each fixed parameter}
  \item{Family}{Family and link}
  \item{Residual deviance}{Vector of deviances}
  \item{Null deviance}{Vector of null deviances}
  \item{PhiHat}{Vector of values of phi parameter}
  \item{Deviance residuals and augments}{Deviance residuals with indication of 
      whether each is included in fit}
  \item{AIC}{Vector of AIC values}
   \item{Number of rows included in Step 1 }{Number of observations included in Step 1}
   \item{Rows by subgroup }{List of row numbers, by subgroup}
   \item{Random parameter estimates}{Matrix of parameter estimates at each stage}
   \item{Dims}{Dims from fit of lme function}
   \item{Fit statistics }{AIC, BIC, and log likelihood}
   \item{Wald Test}{Wald test from fit of coxph function}
   \item{LogLikelihood}{Log likelihood from fit of coxph function}
   \item{Likelihood ratio test}{Likelihood ratio test from fit of coxph function}
   \item{forsearch Call}{Call to forsearch function}
   \item{Call }{Call to this function}
}
\references{
{Atkinson, A and M Riani. Robust Diagnostic Regression Analysis, Springer, New York, 2000.}
{Pinheiro, JC and DM Bates. Mixed-Effects Models in S and S-Plus, Springer, New York, 2000.}
{https://CRAN.R-project.org/package=forsearch}
}
\author{William R. Fairweather}
%\examples{
%Observation <- 1:14
%Dose <- c(3.4,5.2,7,8.5,10.5,13,18,21,28,6.5,10,14,21.5,29)
%Prep <- factor(c(rep(0,9),rep(1,5)))
%Convulse <- c(0,5,11,14,18,21,23,30,27,2,10,18,21,27)
%Total <- c(33,32,38,37,40,37,31,37,30,40,30,40,35,37)
%NoConvulse <- Total-Convulse
%mice <- data.frame(Observation,Dose,Prep,Convulse,Total,NoConvulse)
%brMask(data=mice,blinded="Prep",analysis="glm",initial.sample=100,
%fixed=Convulse~Prep:Dose,glm.response.cols=c(4,6),glm.indep.cols=c(2,3),
%glm.family=binomial("log"),glm.binomialrhs="Dose+Prep")
%}
\keyword{ manip }
