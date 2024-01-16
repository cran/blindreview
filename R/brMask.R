#' @export
brMask <-
function (data, blinded, verbose=TRUE) {

     # NAME                                               brMask
     #
     # VALUE               List with 3 levels: Masked Dataframe, Unmask, and Call.  The second (Unmask) is a list with 4 levels: 
     #                         a level holding date and time of blinded review, a table of masked variables, and a table of masked observation numbers
     #
     # INPUT data          Dataset to be masked
     #       blinded       Character name of variable to be blinded. Must be a factor variable
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
     ##############################################################
     # Ensure that the named blinding variable is in the database #
     # and get the column number of the named variable            #
     ##############################################################
     ncolsdata <- dim(data)[2]
     namesdata <- names(data)
     got.one <- (blinded != namesdata)
     if(all(got.one))stop("No variable in the dataset has been identified as the one to blind.")
     blindcol <- (1:ncolsdata)[!got.one]
     zzz <- data[,blindcol]
     if(!is.factor(zzz))stop("Column to be blinded must be a factor")
     #
     #################################################################
     # Randomize rows of data frame of observations                  #
     # df2 will be name of data frame that is to be blindly analyzed #
     # Collect up new observation numbers for later decoding         # 
     # and store these and original treatment IDs in ORIGobs         #
     #################################################################
     nobs <- dim(data)[1]
     randobs <- sample(1:nobs,nobs)        
     df2 <- data[randobs,]                                # rows of df2 are randomized rownumbers of input data frame
     df2[,1] <- 1:nobs
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
     ##########################
     # Revise rownames of df2 #
     ##########################
     dimnames(df2)[[1]] <- 1:nobs
     ################################
     # Add date and time to ORIGobs #
     ################################
     ORIGobs <- list("Randomization Date"=date(), Variable=masktreats, Observations=ORIGobs)
     #
     listout.br <- list(
         "Masked Dataframe"=  df2,
         Unmask =             ORIGobs,     
         Call =               MC    )
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
