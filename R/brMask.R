#' @export
brMask <-
function (data, blinded, analysis, 
initial.sample=1000, n.obs.per.level=1, skip.step1=NULL, 

fixed=NULL,

lme.random=NULL, lme.formula=NULL,

glm.estimate.phi=TRUE, glm.cobs=1, glm.response.cols=NULL, glm.indep.cols=NULL, 
glm.formula=NULL, glm.binomialrhs=NULL, glm.family=NULL,

cph.formula.elements=NULL, cph.event.time=NULL, cph.status=NULL, cph.x=NULL, 
cph.ties = NULL, cph.redunCorr = 0.9, 

arguments=FALSE, 

verbose=TRUE) {

     # NAME                                               brMask
     #
     # VALUE               List containing forward search after randomizing levels of blinding variable and all observation numbers. Includes
     #                         element holding date and time of blinded review
     #
     # INPUT data          Dataset to be blinded prior to forward search
     #       blinded       Character column name of variable to be blinded
     #       analysis      Character variable for forsearch analysis to be run; xxx part of forsearch_xxx
     #       arguments     Logical. TRUE causes display of arguments of forsearch_xxx function
     #
     #
     MC <- match.call()
     if(verbose) {
          print("", quote=FALSE)
          print("Running brMask", quote=FALSE)
          print("", quote=FALSE)
          print(date(), quote=FALSE)
          print("", quote=FALSE)
          print("Call:", quote=FALSE)
          print(MC, quote=FALSE)
          print("", quote=FALSE)
     }
     #########################################################
     # Ensure that first independent variable is Observation #
     #########################################################
     uu <- dimnames(data)
     if(uu[[2]][1] != "Observation"){Hmisc::prn(uu);stop("First column of dataset must be 'Observation'")}
     #
     ##########################################################################
     # Ensure that one of the available analysis functions has been requested #
     ##########################################################################  
     if(analysis !="lme" & analysis !="lm" & analysis !="glm" & analysis !="cph")stop("Only one of the 4 analysis arguments may be used.")
     #
     ##############################################################
     # Ensure that the named blinding variable is in the database #
     # and get the column number of the named variable            #
     ##############################################################
     ncolsdata <- dim(data)[2]
     namesdata <- names(data)
     got.one <- (blinded != namesdata)
     if(all(got.one))stop("No variable in the dataset has been identified as the one to blind.")
     blindcol <- (1:ncolsdata)[!got.one]
     #
     ###########################################################
     # Randomize rows of data frame of observations            #
     # df2 will be name of data frame that is blindly analyzed #
     # Collect up new observation numbers for later decoding   # 
     # and store these and original treatment IDs in ORIGobs   #
     ###########################################################
     nobs <- dim(data)[1]
     randobs <- sample(1:nobs,nobs)        
     df2 <- data[randobs,]                                # rows of df2 are randomized rownumbers of input data frame
     df2 <- df2[order(df2[,1]),]
     ORIGobs <- data.frame(data[,1], randobs, data[,blindcol])                               #  ORIGobs is data frame
     names(ORIGobs) <- c("OriginalObs", "MaskedObs", "OriginalVar")
     names(df2)[1] <- "Observation"                    # renames variable 'Observation'
     #
     ########################################################
     # Randomize levels of treatments from column 'blinded' #
     # If only 2 treatments, either could be blinded as #1  #
     # Otherwise, #1 cannot be blinded as #1                #
     ########################################################
     treats <- unique(data[,blindcol])
     ntreats <- length(treats)
     randtreats <- sample(1:2,2)
     if(ntreats > 2){
          randtreats <- c(1,2)
          # don't allow first number to be 1
          while(randtreats[1]==1){
               randtreats <- sample(1:ntreats,ntreats)
          }
     }
     masktreats <- data.frame(randtreats,treats)
     masktreats <- masktreats[order(masktreats[,1]),]
     names(masktreats) <- c("BlindVar", "OrigVar")
     #
     ###########################################################
     # Substitute blinded values for original treatment values #
     # Make sure that treatment is a factor                    #
     ###########################################################
     uu <- rep(0,nobs)
     for(i in 1:ntreats){
          uu[df2[,blindcol]==masktreats[i,2]] <- masktreats[i,1]
     }
     uu <- as.factor(uu)
     df2[,blindcol] <- uu
     #
     ########################################
     # Add randomized treatments to ORIGobs #
     ########################################
     BlindVar <- rep(0,nobs)
     for(i in 1:ntreats){
          BlindVar[ORIGobs[,3]==treats[i]] <- randtreats[i]
     }   #   i
     ORIGobs <- data.frame(ORIGobs,BlindVar)
     ORIGobs <- ORIGobs[,c(4,2,3,1)]
     ORIGobs <- ORIGobs[order(ORIGobs[,1],ORIGobs[,2]),]

     treats <- treats[randtreats]
     #
     #######################################################
     # Run forsearch_xxx for chosen analysis #
     #######################################################
     if(analysis=="lme"){
          if(arguments)Hmisc::prn(args(forsearch::forsearch_lme))

          forsearch.out <- forsearch::forsearch_lme(fixed=fixed, data=df2, random=lme.random, formula=lme.formula,
              initial.sample=initial.sample, n.obs.per.level=n.obs.per.level, skip.step1=skip.step1,   
              unblinded=FALSE, verbose=FALSE)
    }
     if(analysis=="lm"){
          if(arguments)Hmisc::prn(args(forsearch::forsearch_lm))

          forsearch.out <- forsearch::forsearch_lm(formula=fixed, data=df2, initial.sample=initial.sample,
                   n.obs.per.level=n.obs.per.level, skip.step1=skip.step1, unblinded=FALSE, verbose=FALSE)
     }
     if(analysis=="glm"){
          if(arguments)Hmisc::prn(args(forsearch::forsearch_glm))

          forsearch.out <- forsearch::forsearch_glm(initial.sample=initial.sample, cobs=glm.cobs, 
                 response.cols=glm.response.cols, indep.cols=glm.indep.cols,
                 family=glm.family, formula=glm.formula, binomialrhs=glm.binomialrhs, data=df2, 
                 n.obs.per.level=n.obs.per.level, estimate.phi=glm.estimate.phi,
                 skip.step1=skip.step1, unblinded=FALSE, verbose=FALSE)
     }
     if(analysis=="cph"){
          if(arguments)Hmisc::prn(args(forsearch::forsearch_cph))

          forsearch_out <- forsearch::forsearch_cph(formula.elements=cph.formula.elements, event.time=cph.event.time, 
                 status=cph.status, x=cph.x, initial.sample = initial.sample, 
                 n.obs.per.level = n.obs.per.level, skip.step1 = skip.step1, ties = cph.ties, 
                 redunCorr = cph.redunCorr, unblinded = FALSE, verbose = FALSE) 
     }
     #
     ################################
     # Add date and time to ORIGobs #
     ################################
     ORIGobs <- list("Randomization Date"=date(), Variable=masktreats, Observations=ORIGobs)
     #
     ##########################################################
     # Output list has general, lm, glm, lme and cph elements #
     ##########################################################
     listout.br <- list(
        Analysis =                            analysis,
        Unmask =                              ORIGobs,     

        "Rows in stage" =                     forsearch.out$"Rows in stage",
        "Standardized residuals" =            forsearch.out$"Standardized residuals", 
        "Number of model parameters" =        forsearch.out$"Number of model parameters", 
         Sigma =                              forsearch.out$Sigma, 
        "Fixed parameter estimates" =         forsearch.out$"Fixed parameter estimates", 
        "s^2" =                               forsearch.out$"s^2", 
         Leverage =                           forsearch.out$Leverage, 
        "Modified Cook distance" =            forsearch.out$"Modified Cook distance", 
        "t statistics" =                      forsearch.out$"t statistics",

         Family =                             forsearch.out$Family, 
        "Residual deviance" =                 forsearch.out$"Residual deviance", 
        "Null deviance" =                     forsearch.out$"Null deviance", 
         PhiHat =                             forsearch.out$PhiHat, 
        "Deviance residuals and augments" =   forsearch.out$"Deviance residuals and augments", 
         AIC =                                forsearch.out$AIC,

        "Number of rows included in Step 1" = forsearch.out$"Number of rows included in Step 1",
        "Rows by subgroup" =                  forsearch.out$"Rows by subgroup", 
        "Random parameter estimates" =        forsearch.out$"Random parameter estimates", 
         Dims =                               forsearch.out$Dims, 
        "Fit statistics" =                    forsearch.out$"Fit statistics", 
        "Wald Test" =                         forsearch.out$"Wald Test",
        "LogLikelihood"=                      forsearch.out$LogLikelihood,
        "Likelihood ratio test"=              forsearch.out$"Likelihood ratio test",
        "forsearch Call" =                    forsearch.out$Call,
         Call =                               MC
     )
     #
     if(verbose) {
          print("", quote=FALSE)
          print("Finished running brMask", quote=FALSE)
          print("", quote=FALSE)
          print(date(), quote=FALSE)
          print("", quote=FALSE)
     }
     return(listout.br)
}
