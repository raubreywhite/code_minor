RAWmisc::InitialiseProject(
  PROJHOME = "/analyses/code_minor/hanna_paper_2/",
  PROJRAW = "/analyses/data_raw/hanna_paper_2/",
  PROJCLEAN = "/analyses/data_clean/hanna_paper_2",
  PROJBAKED = "/analyses/results_baked/hanna_paper_2/",
  PROJFINAL = "/analyses/results_final/hanna_paper_2/",
  PROJSHARED = "/dropbox/results_shared/hanna_paper_2/")

suppressWarnings(suppressMessages(library(ggplot2)))

d <- haven::read_spss("/analyses/data_raw/hanna_paper_2/alla_large_final_gw32.sav")

# CREATING THE DATA FOR THIS GRAPH (YOU WILL NEED TO REDO THIS FOR EACH GRAPH)
plotData <- d[,c("v32_month","v32_date_maxtemp_1","v32_EPDS_9R_rdn")] # WIDE FORMAT
print(plotData[1:5,]) # LOOK AT WHAT WIDE FORMAT DATA LOOKS LIKE
plotData <- reshape2::melt(plotData,id.vars=c("v32_month")) # LONG FORMAT
print(plotData[1:5,]) # LOOK AT WHAT LONG FORMAT DATA LOOKS LIKE

# DETERMINE GRAPH ORDER
plotData$variable <- factor(plotData$variable,levels=c("v32_date_maxtemp_1","v32_EPDS_9R_rdn"))
# GIVE THEM NICE NAMES
levels(plotData$variable) <- c("Max temperature","EPDS Scores at 32 weeks")

# BASIC GRAPH
q <- ggplot(plotData, aes(x=as.factor(v32_month),y=value))
q <- q + geom_boxplot()
q <- q + facet_wrap(~variable,ncol=1,scales="free")
q

# FANCIER GRAPH
q <- ggplot(plotData, aes(x=as.factor(v32_month),y=value))
q <- q + geom_boxplot()
q <- q + facet_wrap(~variable,ncol=1,scales="free")
q <- q + scale_x_discrete("Month",breaks=c(1:12),labels=c("J","F","M","A","M","J","J","A","S","O","N","D"))
q <- q + scale_y_continuous("Value")
q

# SAVE TO DISK
png("/dropbox/clients/hanna/paper_2/sample_scripts/test_results/")

d[,plotYear:=stringr::str_extract(v32_month_year,"[0-9][0-9][0-9][0-9]$")]
d[,c("v32_month_year","v32_month","plotYear"),with=F]
unique(d$Month_year)

q <- ggplot(d)
d$v32_date_maxtemp_1

TOP GRAPH:
  
  v32_date_maxtemp_1 and v32_month_year_chronological (line graph) 

BOTTOM GRAPH:
  
  v32_EPDS_9R_rdn and v32_month_year_chronological (box plot)