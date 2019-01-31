CoxToDF <- function(fit){
  if("coxme" %in% class(fit)){
    res <- data.frame(
      "var"=names(coef(fit)),
      "coef"=coef(fit),
      "exp(coef)"=exp(diag(vcov(fit))),
      "se(coef)"=sqrt(diag(vcov(fit)))
      )
    names(res) <- c("var","coef","exp(coef)","se(coef)")
    res$z <- round(res$coef/res$`se(coef)`,2)
    res$`Pr(>|z|)` <- round(RAWmisc::CalcPValue(res$coef,res$`se(coef)`),3)
  } else if("coxph.penal" %in% class(fit)){
    res <- as.data.frame(coef(summary(fit)))
    res$var <- row.names(res)
    res$`exp(coef)` <- exp(res$coef)
    res <- res[,c("var","coef","exp(coef)","se(coef)","p")]
  } else {
    res <- as.data.frame(coef(summary(fit)))
    res$var <- row.names(res)
    res <- res[,c("var","coef","exp(coef)","se(coef)","z","Pr(>|z|)")]
  }
  res$`95% CI HR LOWER` <- exp(res$coef-1.96*res$`se(coef`)
  res$`95% CI HR UPPER` <- exp(res$coef+1.96*res$`se(coef`)
  
  return(res)
}