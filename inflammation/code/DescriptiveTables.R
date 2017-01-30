GetT1Stat <- function(data, varname, strata, digits=0){
  u <- unique(data[,varname])
  u <- u[!is.na(u)]
  u <- u[order(u)]
  if(length(u)==1) if(!is.factor(data[,varname])){
    data[,varname] <- factor(as.numeric(data[,varname]), levels=c(1,0))
    levels(data[,varname]) <- c("Yes","No")
  }
  if(length(u)==2) if(sum(u==c(0,1))==2) if(!is.factor(data[,varname])){
    data[,varname] <- factor(as.numeric(data[,varname]), levels=c(1,0))
    levels(data[,varname]) <- c("Yes","No")
  }
  if(is.factor(data[,varname])){
    digits <- 0
  } else digits <- 2
  x <- Gmisc::getDescriptionStatsBy(data[,varname],
                             data[,strata],
                             add_total_col=TRUE,
                             show_all_values=FALSE,    statistics=FALSE,
                             useNA = c("always"),
                             html=TRUE,
                             digits=digits
  )
  return(x)
}

GetT1StatPandoc <- function(data, varname, strata, digits=0, lab){
  u <- unique(data[,varname])
  u <- u[!is.na(u)]
  u <- u[order(u)]
  if(length(u)==1) if(!is.factor(data[,varname])){
    data[,varname] <- factor(as.numeric(data[,varname]), levels=c(1,0))
    levels(data[,varname]) <- c("Yes","No")
  }
  if(length(u)==2) if(sum(u==c(0,1))==2) if(!is.factor(data[,varname])){
    data[,varname] <- factor(as.numeric(data[,varname]), levels=c(1,0))
    levels(data[,varname]) <- c("Yes","No")
  }
  if(is.factor(data[,varname])){
    digits <- 0
  } else digits <- 2
  x <- Gmisc::getDescriptionStatsBy(data[,varname],
                                    data[,strata],
                                    add_total_col=TRUE,
                                    show_all_values=FALSE,    statistics=FALSE,
                                    useNA = c("always"),
                                    html=T,
                                    digits=digits
  )
  retval <- as.data.frame(matrix(x,nrow=nrow(x),ncol=ncol(x)))
  names(retval) <- dimnames(x)[[2]]
  retval <- cbind(row.names(x),retval)
  retvalrow <- retval[1,]
  for(i in 1:ncol(retvalrow)) retvalrow[,i] <- "&nbsp;"
  retvalrow[1,1] <- lab
  retval <- rbind(retvalrow,retval)
  retvalrow[1,1] <- "&nbsp;"
  retval <- rbind(retval,retvalrow)
  
  return(retval)
}

TableLOD <- function(d){
  
  summaryIM <- as.data.frame(d[["summaryIM"]])
  summaryIM$PregLOD <- format(round(summaryIM$PregLOD,2),nsmall=2)
  summaryIM$dagger <- ""
  summaryIM$dagger[summaryIM$PregPerc<0.5] <- "&dagger;"
  summaryIM$PregIM %<>% str_replace("im_[0-9][0-9][0-9]_","")
  summaryIM$PregPerc <- paste0(round(summaryIM$PregPerc*100),"%",summaryIM$dagger)
  summaryIM$dagger <- ""
  summaryIM$dagger[summaryIM$PPPerc<0.5] <- "&dagger;"
  summaryIM$PPPerc <- paste0(round(summaryIM$PPPerc*100),"%",summaryIM$dagger)
  for(i in c("Pregp25","Pregp50","Pregp75","PPp25","PPp50","PPp75")) summaryIM[,i] <- format(round(summaryIM[,i],1),nsmall=1)
  
  summaryIM <- summaryIM[,c("imNum","PregIM","PregLOD","PregPerc","Pregp25","Pregp50","Pregp75",
                            "PPPerc","PPp25","PPp50","PPp75")]
  
  tab <- htmlTable(summaryIM,
                   rnames=FALSE,
                   header=c("#","Inflammation marker","LOD","% detected", "P25", "P50", "P75","% detected", "P25", "P50", "P75"),
                   align="llccccccccc",
                   cgroup=c("","Pregnant", "Post-partum"),
                   n.cgroup=c(3,4,4),
                   caption="Summary information for inflammation markers",
                   tfoot="&dagger; Excluded in subsequent analyses due to less than 50% over LOD"
  )
  saveRDS(tab,"results/tableLOD.RDS")
}

TableRawOutcomeSensitive <- function(d){
  data <- d[["data"]]
  namesOutcome <- d[["namesOutcome"]]
  namesCovariates <- d[["namesCovariates"]]
  namesConIMPG <- d[["namesConIMPG"]]
  namesConIMPP <- d[["namesConIMPP"]]
  namesBinIMPG <- d[["namesBinIMPG"]]
  namesBinIMPP <- d[["namesBinIMPP"]]
  
  tableData <- list()
  for(i in c("o_pp_sensitive_1","o_pp_sensitive_2")){
    tableData[[label(data[,i])]] <- GetT1Stat(data, i, "Deprimerade_pp_1")
  }
  
  tab <- mergeDesc(tableData, htmlTable_args=list(caption="Recoding of raw outcome variable to analysis outcome variable"))
  tab
  saveRDS(tab,"results/tableRawOutcomeSensitive.RDS")
}

TableRawOutcomeSpecific <- function(d){
  data <- d[["data"]]
  namesOutcome <- d[["namesOutcome"]]
  namesCovariates <- d[["namesCovariates"]]
  namesConIMPG <- d[["namesConIMPG"]]
  namesConIMPP <- d[["namesConIMPP"]]
  namesBinIMPG <- d[["namesBinIMPG"]]
  namesBinIMPP <- d[["namesBinIMPP"]]
  
  tableData <- list()
  for(i in c("o_pp_specific_1","o_pp_specific_2")){
    tableData[[label(data[,i])]] <- GetT1Stat(data, i, "Deprimerade_pp_2")
  }
  
  tab <- mergeDesc(tableData, htmlTable_args=list(caption="Recoding of raw outcome variable to analysis outcome variable"))
  tab
  saveRDS(tab,"results/tableRawOutcomeSpecific.RDS")
}

TableDemographics <- function(d){
  
  data <- d[["data"]]
  namesOutcome <- d[["namesOutcome"]]
  namesCovariates <- d[["namesCovariates"]]
  namesConIMPG <- d[["namesConIMPG"]]
  namesConIMPP <- d[["namesConIMPP"]]
  namesBinIMPG <- d[["namesBinIMPG"]]
  namesBinIMPP <- d[["namesBinIMPP"]]
  namesZScores <- c("meanZPG","meanZdownPG","meanZupPG","meanZPP","meanZdownPP","meanZupPP","meanZdiff")
  
  tableData <- list()
  for(i in c(namesCovariates,namesZScores)){
    if(i %in% c("c_firstBorn",
                "c_participationType",
                "c_preBMIOrdered",
                "c_abnormalBMI",
                "c_sampleTimePP",
                "c_sampleTimePG",
                "c_formulaOnly",
                "c_age")) next
    if(label(data[,i])=="") next
    tableData[[label(data[,i])]] <- GetT1StatPandoc(data, i, "o_pp_sensitive_1",lab=label(data[,i]))
  }
  
  tab <- rbindlist(tableData)
  setnames(tab,1,"Variable")
  tab <- pander::pandoc.table.return(tab, caption=paste0("Demographic summaries according to outcome: ",label(d$data$o_pp_sensitive_1)), style="simple", split.tables=Inf, split.cells=Inf)
  #tab <- Gmisc::mergeDesc(tableData, htmlTable_args=list(caption=paste0("Demographic summaries according to outcome: ",label(d$data$o_pp_sensitive_1))))
  
  saveRDS(tab,"results/tableDemographics_1.RDS")
  
  tableData <- list()
  for(i in c(namesCovariates,namesZScores)){
    if(i %in% c("c_firstBorn",
                "c_participationType",
                "c_preBMIOrdered",
                "c_abnormalBMI",
                "c_sampleTimePP",
                "c_sampleTimePG",
                "c_formulaOnly",
                "c_age")) next
    print(label(data[,i]))
    if(label(data[,i])=="") next
    tableData[[label(data[,i])]] <- GetT1StatPandoc(data, i, "o_pp_sensitive_2",lab=label(data[,i]))
  }
  
  tab <- rbindlist(tableData)
  setnames(tab,1,"Variable")
  tab <- pander::pandoc.table.return(tab, 
                              caption=paste0("Demographic summaries according to outcome: ",label(d$data$o_pp_sensitive_2)), 
                              style="simple", split.tables=Inf, split.cells=Inf)
  #tab <- Gmisc::mergeDesc(tableData, htmlTable_args=list(caption=paste0("Demographic summaries according to outcome: ",label(d$data$o_pp_sensitive_2))))
  
  saveRDS(tab,"results/tableDemographics_2.RDS")
}





