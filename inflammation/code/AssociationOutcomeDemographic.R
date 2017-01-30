
AssociationOutcomeDemographics <- function(d){
  
  data <- d[["data"]]
  namesOutcome <- d[["namesOutcome"]]
  namesCovariates <- d[["namesCovariates"]]
  namesConIMPG <- d[["namesConIMPG"]]
  namesConIMPP <- d[["namesConIMPP"]]
  namesBinIMPG <- d[["namesBinIMPG"]]
  namesBinIMPP <- d[["namesBinIMPP"]]
  
  for(i in namesOutcome){
    fit <- glm(as.formula(paste(i," ~ 
                                c_ageCat + 
                                c_parity +
                                c_preBMIOrdered + 
                                c_smoker + 
                                c_educ + 
                                c_chronicInflam + 
                                c_chronicRheum + 
                                c_chronicAsthma + 
                                c_chronicOther +
                                c_breastfeeding")),family=binomial(), data=data)
    f <- Greg::printCrudeAndAdjustedModel(fit, 
                                          add_references=TRUE, 
                                          reference_zero_effect=1,
                                          ci_lim=c(-50,50))
    tab <- Greg:::prPrintCAstring(f, css.rgroup="", caption=paste0(label(data[,i])))
    saveRDS(tab,paste0("results/tableDetailed_",i,".RDS"))
  }
  
  for(i in namesOutcome){
    fit <- glm(as.formula(paste(i," ~ 
                                c_age + 
                                c_firstBorn +
                                c_abnormalBMI + 
                                c_smoker + 
                                c_educ + 
                                c_chronicInflam + 
                                c_chronicRheum + 
                                c_chronicAsthma + 
                                c_chronicOther +
                                c_formulaOnly")),family=binomial(), data=data)
    f <- Greg::printCrudeAndAdjustedModel(fit, 
                                          add_references=TRUE, 
                                          reference_zero_effect=1,
                                          ci_lim=c(-50,50))
    tab <- Greg:::prPrintCAstring(f, css.rgroup="", caption=paste0(label(data[,i])))
    saveRDS(tab,paste0("results/tableSimple_",i,".RDS"))
  }
}





