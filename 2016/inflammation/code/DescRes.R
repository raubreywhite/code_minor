DescRes <- function(x, f=NULL,lab=NULL){
  descMean <- apply(x[,-1],2,mean,na.rm=T)
  descNum <- apply(x[,-1],2,sum,na.rm=T)
  descDenom <- apply(x[,-1],2,function(x){sum(!is.na(x))})
  descRes <- format(round(descMean,2),nsmall=2)
  ind <- apply(x[,-1],2,max,na.rm=T)==1
  descRes[ind] <- paste0("n=",descNum[ind])
  descRes <- paste0(descRes," (",descDenom,")")
  descRes <- c("",descRes)
  
  retVal <- descRes
  if(!is.null(f)){
    retVal <- cbind(descRes,as.data.frame(f))
    names(retVal) <- c("Mean/n (N)","Crude OR","2.5% to 97.5%","Adj OR","2.5% to 97.5%")
    retVal <- pander::pandoc.table.return(retVal, caption=lab, style="simple", split.tables=Inf, split.cells=Inf)
  }
    
  return(retVal)
}