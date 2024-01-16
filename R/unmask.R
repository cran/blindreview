#' @export
unmask <-
function (object, obsrange=NULL, verbose=TRUE) 
{
     #                          unmask
     #
     # VALUE         Printout of encoded and original treatment IDs and observation numbers
     #
     # INPUT    object       List output from blinding function (brMask)
     #          obsrange     Null or numeric vector. NULL causes printing of complete list of masked observation numbers. c(7,13,22,88) prints four
     #                           observation numbers whose blinded values are 7, 13, 22 and 88.   1:22 prints blinded values 1 through 22.            
     #          verbose      Logical. TRUE causes printing of program ID before and after running.
     #
     # NOTE:  Always unmasks blinded variable; can print all or some of masked and original observation numbers 
     #
     MC <- match.call()
     if(verbose) {
          print("", quote=FALSE)
          print("Running unmask", quote=FALSE)
          print("", quote=FALSE)
          print(date(), quote=FALSE)
          print("", quote=FALSE)
          print("Call:", quote=FALSE)
          print(MC, quote=FALSE)
          print("", quote=FALSE)
     }
     ###################################################################################
     # Print date of blind review, requested observation numbers, then treatment codes #
     ###################################################################################
     x <- object[[2]]
     print("The blind review resulting in these coded values was completed", quote=FALSE)
     print(x[[1]]) 
     print("", quote=FALSE)
     if(is.null(obsrange)){
          print(x[[3]], quote=FALSE)
     }
     else{
          y <- x[[3]][,2]
          y <- outer(y,obsrange,FUN="==")
          index <- apply(y,1,any)
          print(x[[3]][index,], quote=FALSE)
     }
     print(x[[2]], quote=FALSE)
     #
     if(verbose) {
          print("", quote=FALSE)
          print("Finished running unmask", quote=FALSE)
          print("", quote=FALSE)
          print(date(), quote=FALSE)
          print("", quote=FALSE)
     }
     return()
}
