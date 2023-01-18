#' @export
plotdiag.blind.all <-
function (object, treatnum=1, mt = " ", st = " ", cc = NULL, ccrand = NULL, objnames=FALSE, verbose=TRUE) 
{
#                                  plotdiag.blind.all
#
# VALUE            Complete run of all plotdiag.xxx functions for a blind review
#
# INPUT
#      object           Name of object file contaiining data for plotting
#      treatnum         Identifying number of plot
#      objnames         Logical. TRUE causes names of object to be printed
#

     MC <- match.call()
     if (verbose){
        print("", quote=FALSE)
        print("Running plotdiag.blind.all", quote=FALSE)
        print("", quote=FALSE)
        print(date(), quote=FALSE)
        print("", quote=FALSE)
        print("Call:", quote=FALSE)
        print(MC, quote=FALSE)
        print("", quote=FALSE)
     }
     #
     MCobject <- object$Call
     if(is.null(MCobject))stop("object is not the name of a blind review object file")

     print("The Call that created this database was", quote=FALSE)
     print(MCobject)
     analysis <- object[[1]]
     if(!any(analysis==c("lm","lme","glm"))){
          Hmisc::prn(analysis)
          stop("Call does not indicate a recognized underlying analysis function")
     }
     rundate <- object[[2]][[1]]
     if(substring(MCobject,1,6)[1]=="brMask"){
          wmflead <- paste("Masked blind", treatnum, sep=" ")
     cpt <- paste("Masked blind review", rundate, sep=" ")
     }
     else{
          wmflead <- paste("Pooled blind", treatnum, sep=" ")
     cpt <- paste("Pooled blind review", rundate, sep=" ")
     }
     if(analysis=="lm"){
#          plotdiag.AICX(object, maintitle = mt, subtitle = st, caption = cpt, wmf=paste(wmflead, "AICX"), verbose=FALSE)
          forsearch::plotdiag.Cook(object, maintitle = mt, subtitle = st, caption = cpt, wmf=paste(wmflead, "Cook"), addline="none", verbose=FALSE)  
#          plotdiag.deviance.residuals(object, maintitle = mt, subtitle = st, caption = cpt, wmf=paste(wmflead, "Deviance residuals"), verbose=FALSE)
#          plotdiag.deviances(object, maintitle = mt, subtitle = st, caption = cpt, wmf=paste(wmflead, "Deviances"), verbose=FALSE)
#          plotdiag.fit3(object, maintitle = mt, subtitle = st, caption = cpt, wmf=paste(wmflead, "fit3"), verbose=FALSE)
          forsearch::plotdiag.leverage(object, maintitle = mt, subtitle = st, caption = cpt, wmf=paste(wmflead, "leverage"), verbose=FALSE)          
          forsearch::plotdiag.params.fixed(object, coeff.codenums=cc, maintitle = mt, subtitle = st, caption = cpt, wmf=paste(wmflead, "params fixed"), verbose=FALSE)
#          plotdiag.params.random(object, coeff.codenums=ccrand, maintitle = mt, subtitle = st, caption = cpt, wmf=paste(wmflead, "params random"), verbose=FALSE)
#          plotdiag.phihatx(object, maintitle = mt, subtitle = st, caption = cpt, wmf=paste(wmflead, "phihatx"), verbose=FALSE)
          forsearch::plotdiag.residuals(object, maintitle = mt, subtitle = st, caption = cpt, wmf=paste(wmflead, "residuals"), verbose=FALSE)         
          forsearch::plotdiag.s2(object, maintitle = mt, subtitle = st, caption = cpt, wmf=paste(wmflead, "s2"), addline="none", verbose=FALSE)
          forsearch::plotdiag.tstats(object, maintitle = mt, subtitle = st, caption = cpt, wmf=paste(wmflead, "tstats"), verbose=FALSE)  

          if(objnames)Hmisc::prn(names(object))
          print(forsearch::search.history(object))
     }

     if(analysis=="lme"){
#          plotdiag.AICX(object, maintitle = mt, subtitle = st, caption = cpt, wmf=paste(wmflead, "AICX"), verbose=FALSE)
          forsearch::plotdiag.Cook(object, maintitle = mt, subtitle = st, caption = cpt, wmf=paste(wmflead, "Cook") , addline="none", verbose=FALSE )
#          plotdiag.deviance.residuals(object, maintitle = mt, subtitle = st, caption = cpt, wmf=paste(wmflead, "Deviance residuals"), verbose=FALSE)
#          plotdiag.deviances(object, maintitle = mt, subtitle = st, caption = cpt, wmf=paste(wmflead, "Deviances"), verbose=FALSE)
          forsearch::plotdiag.fit3(object, maintitle = mt, subtitle = st, caption = cpt, wmf=paste(wmflead, "fit3"), verbose=FALSE)
          forsearch::plotdiag.leverage(object, maintitle = mt, subtitle = st, caption = cpt, wmf=paste(wmflead, "leverage"), verbose=FALSE)          
          forsearch::plotdiag.params.fixed(object, coeff.codenums=cc, maintitle = mt, subtitle = st, caption = cpt, wmf=paste(wmflead, "params fixed"), verbose=FALSE)
          forsearch::plotdiag.params.random(object, coeff.codenums=ccrand, maintitle = mt, subtitle = st, caption = cpt, wmf=paste(wmflead, "params random"), verbose=FALSE)
#          plotdiag.phihatx(object, maintitle = mt, subtitle = st, caption = cpt, wmf=paste(wmflead, "phihatx"), verbose=FALSE)
          forsearch::plotdiag.residuals(object, maintitle = mt, subtitle = st, caption = cpt, wmf=paste(wmflead, "residuals"), verbose=FALSE)         
#          plotdiag.s2(object, maintitle = mt, subtitle = st, caption = cpt, wmf=paste(wmflead, "s2"), addline="none", verbose=FALSE)
          forsearch::plotdiag.tstats(object, maintitle = mt, subtitle = st, caption = cpt, wmf=paste(wmflead, "tstats"), verbose=FALSE)  

          if(objnames)Hmisc::prn(names(object))
          print(forsearch::search.history(object))
     }

     if(analysis=="glm"){
          forsearch::plotdiag.AICX(object, maintitle = mt, subtitle = st, caption = cpt, wmf=paste(wmflead, "AICX"), verbose=FALSE)
#          plotdiag.Cook(object, maintitle = mt, subtitle = st, caption = cpt, wmf=paste(wmflead, "Cook"), addline="none", verbose=FALSE)          Should be doing this?  
          forsearch::plotdiag.deviance.residuals(object, maintitle = mt, subtitle = st, caption = cpt, wmf=paste(wmflead, "Deviance residuals"), verbose=FALSE)
          forsearch::plotdiag.deviances(object, devtype="R", maintitle = mt, subtitle = st, caption = cpt, wmf=paste(wmflead, "Deviances type R"), verbose=FALSE)
          forsearch::plotdiag.deviances(object, devtype="N", maintitle = mt, subtitle = st, caption = cpt, wmf=paste(wmflead, "Deviances type N"), verbose=FALSE)
#          plotdiag.fit3(object, maintitle = mt, subtitle = st, caption = cpt, wmf=paste(wmflead, "fit3"), verbose=FALSE)
          forsearch::plotdiag.leverage(object, maintitle = mt, subtitle = st, caption = cpt, wmf=paste(wmflead, "leverage"), verbose=FALSE)          
          forsearch::plotdiag.params.fixed(object, coeff.codenums=cc, maintitle = mt, subtitle = st, caption = cpt, wmf=paste(wmflead, "params fixed"), verbose=FALSE)
#          plotdiag.params.random(object, coeff.codenums=ccrand, maintitle = mt, subtitle = st, caption = cpt, wmf=paste(wmflead, "params random"), verbose=FALSE)
#          plotdiag.residuals(object, maintitle = mt, subtitle = st, caption = cpt, wmf=paste(wmflead, "residuals"), verbose=FALSE )
#          plotdiag.s2(object, maintitle = mt, subtitle = st, caption = cpt, wmf=paste(wmflead, "s2"), addline="none", verbose=FALSE)
          forsearch::plotdiag.tstats(object, maintitle = mt, subtitle = st, caption = cpt, wmf=paste(wmflead, "tstats"), verbose=FALSE)  

          if(objnames)Hmisc::prn(names(object))
          print(forsearch::search.history(object))

          forsearch::plotdiag.phihatx(object, maintitle = mt, subtitle = st, caption = cpt, wmf=paste(wmflead, "phihatx"), verbose=FALSE)
     }
     if(verbose){
        print("", quote=FALSE)
        print("Finished running plotdiag.blind.all", quote=FALSE)
        print("", quote=FALSE)
        print("NOTE: All references in these graphs to observation numbers are to the MASKED values.", quote=FALSE)
        print("", quote=FALSE)
        print(date(), quote=FALSE)
        print("", quote=FALSE)
     }
     return()
}
