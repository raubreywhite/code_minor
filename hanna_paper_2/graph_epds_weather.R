RAWmisc::InitialiseProject(
  PROJHOME = "/analyses/code_minor/hanna_paper_2/",
  PROJRAW = "/analyses/data_raw/hanna_paper_2/",
  PROJCLEAN = "/analyses/data_clean/hanna_paper_2",
  PROJBAKED = "/analyses/results_baked/hanna_paper_2/",
  PROJFINAL = "/analyses/results_final/hanna_paper_2/",
  PROJSHARED = "/dropbox/results_shared/hanna_paper_2/")

library(ggplot2)
library(data.table)
library(gridExtra)

d <- haven::read_spss("/analyses/data_raw/hanna_paper_2/alla_large_final_gw32.sav")

#### TWO BARPLOTS
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

# FANCIER GRAPH, THAT HAS EVERYTHING MADE BIGGER SO THAT IT LOOKS NORMAL SIZE
# WHEN DISPLAYED AT HIGH RESOLUTION (i.e. 300 DPI FOR HALF A SHEET OF A4 PAPER)
q <- ggplot(plotData, aes(x=as.factor(v32_month),y=value))
q <- q + geom_boxplot()
q <- q + facet_wrap(~variable,ncol=1,scales="free")
q <- q + scale_x_discrete("Month",breaks=c(1:12),labels=c("J","F","M","A","M","J","J","A","S","O","N","D"))
q <- q + scale_y_continuous("Value")
q <- q + theme_gray(base_size=22)
q

# SAVE TO DISK
# WITH PIXEL RESOLUTION FOR HALF OF AN A4 SHEET OF PAPER
png("/dropbox/clients/hanna/paper_2/sample_scripts/test_results/graphs_epds_weather.png",width=3508/2,height=2480/2)
print(q)
dev.off()

#### ONE LINE, ONE BARPLOT
# CREATING THE DATA FOR THIS GRAPH (YOU WILL NEED TO REDO THIS FOR EACH GRAPH)
plotDataLine <- d[,c("v32_month","v32_date_maxtemp_1")]
plotDataLine <- aggregate(. ~ v32_month, data=plotDataLine, FUN=mean) # SUMMARISES THE DATA TO BE MONTHLY AVERAGE

plotDataBoxplot <- d[,c("v32_month","v32_EPDS_9R_rdn")]

# BASIC GRAPH
q <- ggplot(plotDataLine, aes(x=v32_month,y=v32_date_maxtemp_1))
topGraph <- q + geom_line()
topGraph

q <- ggplot(plotDataBoxplot, aes(x=as.factor(v32_month),y=v32_EPDS_9R_rdn))
bottomGraph <- q + geom_boxplot()
bottomGraph

gridExtra::grid.arrange(topGraph, bottomGraph, ncol=1)



# FANCIER GRAPH
q <- ggplot(plotDataLine, aes(x=v32_month,y=v32_date_maxtemp_1))
q <- q + geom_line()
q <- q + scale_x_continuous("Month",breaks=c(1:12),labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), minor_breaks=NULL)
q <- q + scale_y_continuous("Average max temperature")
topGraph <- q
topGraph


q <- ggplot(plotDataBoxplot, aes(x=as.factor(v32_month),y=v32_EPDS_9R_rdn))
q <- q + geom_boxplot()
q <- q + scale_x_discrete("Month",breaks=c(1:12),labels=c("J","F","M","A","M","J","J","A","S","O","N","D"))
q <- q + scale_y_continuous("EPDS scores")
bottomGraph <- q
bottomGraph

gridExtra::grid.arrange(topGraph, bottomGraph, ncol=1)

# FANCIER GRAPH, THAT HAS EVERYTHING MADE BIGGER SO THAT IT LOOKS NORMAL SIZE
# WHEN DISPLAYED AT HIGH RESOLUTION (i.e. 300 DPI FOR HALF A SHEET OF A4 PAPER)
q <- ggplot(plotDataLine, aes(x=v32_month,y=v32_date_maxtemp_1))
q <- q + geom_line()
q <- q + scale_x_continuous("Month",breaks=c(1:12),labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), minor_breaks=NULL)
q <- q + scale_y_continuous("Average max temperature")
q <- q + theme_gray(base_size=22)
topGraph <- q
topGraph


q <- ggplot(plotDataBoxplot, aes(x=as.factor(v32_month),y=v32_EPDS_9R_rdn))
q <- q + geom_boxplot()
q <- q + scale_x_discrete("Month",breaks=c(1:12),labels=c("J","F","M","A","M","J","J","A","S","O","N","D"))
q <- q + scale_y_continuous("EPDS scores")
q <- q + theme_gray(base_size=22)
bottomGraph <- q
bottomGraph

# SAVE TO DISK
# WITH PIXEL RESOLUTION FOR HALF OF AN A4 SHEET OF PAPER
png("/dropbox/clients/hanna/paper_2/sample_scripts/test_results/graphs_epds_weather_line_box.png",width=3508/2,height=2480/2)
gridExtra::grid.arrange(topGraph, bottomGraph, ncol=1)
dev.off()





