Attributable <- function(d,varMaster,outcome){
  
  
  var <- c()
  for(v in varMaster){
    if(sum(d[[v]]==1 & d[[outcome]]==1,na.rm=T)>=5) var <- c(var,v)
  }
  
  fitData <- copy(d)
  fit <- glm2::glm2(
    as.formula(
      sprintf("%s~%s",outcome,paste0(var,collapse="+"))
    ), data=fitData,
    weights=weight
  )
  summary(fit)
  
  effects <- coef(summary(fit))
  effects <- data.frame(effects)
  effects$var <- row.names(effects)
  names(effects) <- c("beta","se","t","p","var")
  effects <- effects[-1,]
  sigEffects <- effects$var[effects$p<0.05]
  sigEffectsProtective <- effects$var[effects$p<0.05 & effects$beta<0]
  sigEffectsHarmful <- effects$var[effects$p<0.05 & effects$beta>0]
  
  res <- vector("list",length=length(var))
  for(i in 1:length(var)){
    print(i)
    index <- i
    
    x <- as.data.frame(coef(summary(fit)))
    x$var <- row.names(x)
    pval <- x[which(x$var==var[i]),4]
    isHarmful <- x[which(x$var==var[i]),1]>0
    
    p1 <- predict(fit,fitData,type="response")
    dx <- copy(fitData)
    if(isHarmful){
      dx[[var[i]]] <- 0
    } else {
      dx[[var[i]]] <- 1
    }
    p0 <- predict(fit,dx,type="response")
    dx[,p1:=p1*weight]
    dx[,p0:=p0*weight]
    res[[index]] <- dx[,.(cases=sum(n_maltreatmentSyndrome),p1=sum(p1,na.rm=T),p0=sum(p0,na.rm=T)),by=.(FODAR)]
    res[[index]][,var:=var[i]]
    
    
    res[[index]][,pval := pval]
    #res[[index]][,modelPeriod:=modelPeriod]
  }
  
  res <- rbindlist(res)
  res <- res[var %in% sigEffects]
  res[,attr:=p1-p0]
  
  resx <- res[,.(attr=sum(attr,na.rm=T),cases=mean(cases,na.rm=T)),by=.(FODAR)]
  resx[,var:="Unexplained"]
  resx[,attr:=cases-attr]
  resx[attr<0,attr:=0]
  res2 <- res[,c("FODAR","var","attr")]
  resx <- resx[,names(res2),with=F]
  plotData <- rbind(res2,resx)
  
  ordering <- plotData[var!="Unexplained",.(attr=sum(attr)),by=.(var)]
  setorder(ordering,attr)
  plotData[,var:=factor(var,levels=c("Unexplained",ordering$var))]
  
  q <- ggplot(plotData,aes(x=FODAR,y=attr,fill=var))
  q <- q + geom_area()
  q <- q + scale_fill_manual(values=Colours(length(unique(plotData$var))))
  return(q)
  
}


AttributableMixed <- function(d,varMaster,outcome){
  
  
  var <- c()
  for(v in varMaster){
    if(sum(d[[v]]==1 & d[[outcome]]==1,na.rm=T)>=5) var <- c(var,v)
  }
  
  d[,is2008andOlder:=0]
  d[FODAR>=2008,is2008andOlder:=1]
  
  f <- sprintf("%s~%s + (1|location)",outcome,paste0(var,collapse="+"))
  #f <- sprintf("%s~ %s + %s + (1|location)",outcome,paste0(var,collapse="+"),paste0(paste0(var,":is2008andOlder"),collapse="+"))
  
  fitData <- copy(d)
  fit <- lme4::lmer(
    as.formula(f),
    data=fitData,
    weights=weight,
    verbose=2L
  )
  summary(fit)
  
  effects <- coef(summary(fit))
  effects <- data.frame(effects)
  effects$var <- row.names(effects)
  names(effects) <- c("beta","se","t","var")
  effects$p <- 2*(1-pnorm(abs(effects$t)))
  effects <- effects[-1,]
  sigEffects <- effects$var[effects$p<0.05]
  sigEffectsProtective <- effects$var[effects$p<0.05 & effects$beta<0]
  sigEffectsHarmful <- effects$var[effects$p<0.05 & effects$beta>0]
  
  sigEffectsProtective <- stringr::str_replace_all(sigEffectsProtective,c("is2008andOlder"),"")
  sigEffectsProtective <- stringr::str_replace_all(sigEffectsProtective,c(":"),"")
  
  sigEffectsHarmful <- stringr::str_replace_all(sigEffectsHarmful,c("is2008andOlder"),"")
  sigEffectsHarmful <- stringr::str_replace_all(sigEffectsHarmful,c(":"),"")
  
  res <- vector("list",length=length(var))
  for(i in 1:length(var)){
    print(i)
    index <- i
    
    x <- as.data.frame(coef(summary(fit)))
    x$var <- row.names(x)
    pval <- x[which(x$var==var[i]),4]
    isHarmful <- x[which(x$var==var[i]),1]>0
    
    p1 <- predict(fit,fitData,type="response")
    dx <- copy(fitData)
    if(isHarmful){
      dx[[var[i]]] <- 0
    } else {
      dx[[var[i]]] <- 1
    }
    p0 <- predict(fit,dx,type="response")
    dx[,p1:=p1*weight]
    dx[,p0:=p0*weight]
    res[[index]] <- dx[,.(cases=sum(n_maltreatmentSyndrome),p1=sum(p1,na.rm=T),p0=sum(p0,na.rm=T)),by=.(FODAR,location)]
    res[[index]][,var:=var[i]]
    
    
    res[[index]][,pval := pval]
    #res[[index]][,modelPeriod:=modelPeriod]
  }
  
  res <- rbindlist(res)
  res <- res[var %in% sigEffects]
  res[,attr:=p1-p0]
  
  resx <- res[,.(attr=sum(attr,na.rm=T),cases=mean(cases,na.rm=T)),by=.(FODAR,location)]
  resx[,var:="Unexplained"]
  resx[,attr:=cases-attr]
  resx[attr<0,attr:=0]
  res2 <- res[,c("FODAR","location","var","attr")]
  resx <- resx[,names(res2),with=F]
  plotData <- rbind(res2,resx)
  
  ordering <- plotData[var!="Unexplained",.(attr=sum(attr)),by=.(var)]
  setorder(ordering,attr)
  plotData[,var:=factor(var,levels=c("Unexplained",ordering$var))]
  plotData[,denom:=sum(attr),by=.(FODAR,location)]
  plotData[,perc:=attr/denom*100]
  
  q <- ggplot(plotData,aes(x=FODAR,y=attr,fill=var))
  q <- q + geom_area()
  q <- q + scale_fill_manual(values=Colours(length(unique(plotData$var))))
  q <- q + scale_y_continuous("Cases")
  q1 <- q + facet_wrap(~location)
  
  q <- ggplot(plotData,aes(x=FODAR,y=perc,fill=var))
  q <- q + geom_area()
  q <- q + scale_fill_manual(values=Colours(length(unique(plotData$var))))
  q <- q + scale_y_continuous("Percentage attributable")
  q2 <- q + facet_wrap(~location)
  
  return(list("cases"=q1,"percentage"=q2))
  
}