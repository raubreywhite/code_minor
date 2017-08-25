Sys.setenv(R_TOOLS="C:\\Apps\\Rtools")
Sys.setenv(R_TOOLS_PATH="C:\\Apps\\Rtools\\bin;C:\\Apps\\Rtools\\gcc-4.6.3\\bin")
Sys.setenv(PATH=paste0(c(Sys.getenv("R_TOOLS_PATH"),Sys.getenv("PATH"),Sys.getenv("R_PATH")),collapse=";"))

setwd("H:/Uppsala")

upgradeRLocalSetup <- FALSE
source("RLocalSetup.R")

# Packrat
setwd("inflammation")
suppressWarnings(packrat::on(auto.snapshot=FALSE))
setwd("..")
#packrat::status()
#packrat::snapshot()

# Unload package
try(devtools::unload("inflammation"),TRUE)

# Load package
suppressWarnings(devtools::load_all("inflammation"))
library(data.table)


SMAOpng <- function (file = "Figure.png", w = 1, h = 1, landscape = TRUE) 
{
  width <- 2480/2
  height <- 3508/2
  if (landscape) {
    width <- 3508/2
    height <- 2480/2
  }
  width <- width*w
  height <- height*h
  png(file, width = width, height = height)
}

d <- CleanData()

write.table(d$data[,c("CustomDataR",d$namesLog2IMPG)],file="data_to_send/log2impg.csv",dec=",",sep=";")
write.table(d$data[,c("CustomDataR",d$namesLog2IMPP)],file="data_to_send/log2impp.csv",dec=",",sep=";")
write.table(d$data[,c("CustomDataR","meanZPG","meanZPP")],file="data_to_send/zscores.csv",dec=",",sep=";")

xtabs(~d$data$c_breastfeeding)


xtabs(~d$data$o_pp_sensitive_2[d$analysisDAPCIMPG_pp_sensitive_2])

nrow(d$data)
sum(!is.na(d$data$meanZPG))
sum(!is.na(d$data$meanZPP))

d$downIF
d$upIF

# Figure 1
IMClusters(d)

# Figure 2
BoxPlotsByCaseSpecific(d,
                       pgKeep=c("l_im_194_ADA","l_im_192_STAMPB","l_im_191_ST1A1","l_im_162_IL10","l_im_118_AXIN1"))

# Figure 3, Table S1
IndividualIFAnalyses(d)

# Table 1
TableDemographics(d)

# Table 2
AssociationExposureDemographics(d,extraPG=c("c_firstBorn","c_caesarean","c_breastfeedingOnly"))

# Table 3
ZScoreAnalysesLassoTest(d)

# Figure S1
BoxPlotsByCase(d)


file <- system.file("extdata","results_pg.Rmd",package="inflammation")
RmdToHTML(file,paste0("results/ResultsPG_",format(Sys.time(), "%Y_%m_%d"),".html"))

file <- system.file("extdata","results_pp.Rmd",package="inflammation")
RmdToHTML(file,paste0("results/ResultsPP_",format(Sys.time(), "%Y_%m_%d"),".html"))

#d$data[,]

#TableLOD(d)
#TableRawOutcomeSpecific(d)
#TableRawOutcomeSensitive(d)
#TableDemographics(d)
#IMOutliers(d)

#IMClusters(d)







file <- system.file("extdata","results.Rmd",package="inflammation")
RmdToHTML(file,paste0("results/Results_",format(Sys.time(), "%Y_%m_%d"),".html"))


stop("STOP")
####

x <- readRDS("results/en_antiNSAIDS-loadings.RDS")
if(!is.null(x)){
  en[[length(en)+1]] <- x
  en[[length(en)]]$exclusions <- "Exclude: Anti/NSAIDs"
}

x <- readRDS("results/en_smoker-loadings.RDS")
if(!is.null(x)){
  en[[length(en)+1]] <- x
  en[[length(en)]]$exclusions <- "Exclude: Smokers"
}
en <- rbindlist(en)



if(FALSE){
  sum(d$analysisDAPCIMPG_pp_sensitive_1)
sum(d$analysisDAPCIMPP_pp_sensitive_2)
xtabs(~d$data$o_pp_sensitive_1)
xtabs(~d$data$o_pp_sensitive_1[d$analysisDAPCIMPG_pp_sensitive_1])

numbers <- d$data[,c("o_pp_sensitive_1",
                     "o_pp_sensitive_2",
                     "o_pp_sensitive_3",
                     "hasLog2IMPG",
                     "hasLog2IMPP")]
numbers$N <- 1
numbers <- numbers[,c("o_pp_sensitive_1",
                      "o_pp_sensitive_2",
                      "o_pp_sensitive_3",
                      "N",
                      "hasLog2IMPG",
                      "hasLog2IMPP")]
numbers$analysisDAPCIMPG_pp_sensitive_1 <- d$analysisDAPCIMPG_pp_sensitive_1
numbers$analysisDAPCIMPG_pp_sensitive_2 <- d$analysisDAPCIMPG_pp_sensitive_2
numbers$analysisDAPCIMPG_pp_sensitive_3 <- d$analysisDAPCIMPG_pp_sensitive_3
numbers$analysisDAPCIMPP_pp_sensitive_1 <- d$analysisDAPCIMPP_pp_sensitive_1
numbers$analysisDAPCIMPP_pp_sensitive_2 <- d$analysisDAPCIMPP_pp_sensitive_2
numbers$analysisDAPCIMPP_pp_sensitive_3 <- d$analysisDAPCIMPP_pp_sensitive_3
#numbers$o_pp_sensitive_1 <- as.numeric(numbers$o_pp_sensitive_1)-1
#numbers$o_pp_sensitive_2 <- as.numeric(numbers$o_pp_sensitive_2)-1

numbers <- data.table(numbers)
numbers$N <- 1
numbers_o_pp_sensitive_1 <- numbers[,lapply(.SD,function(x) return(sum(x,na.rm=T))),by=list(o_pp_sensitive_1),.SDcols=4:ncol(numbers)]
numbers_o_pp_sensitive_1[,o_pp_sensitive_1:=as.character(o_pp_sensitive_1)]
numbers_o_pp_sensitive_1[is.na(o_pp_sensitive_1),o_pp_sensitive_1:="Missing"]
setorder(numbers_o_pp_sensitive_1,o_pp_sensitive_1)
numbers_o_pp_sensitive_1[,analysisDAPCIMPG_pp_sensitive_2:=NULL]
numbers_o_pp_sensitive_1[,analysisDAPCIMPP_pp_sensitive_2:=NULL]
numbers_o_pp_sensitive_1[,analysisDAPCIMPG_pp_sensitive_3:=NULL]
numbers_o_pp_sensitive_1[,analysisDAPCIMPP_pp_sensitive_3:=NULL]

numbers_o_pp_sensitive_2 <- numbers[,lapply(.SD,function(x) return(sum(x,na.rm=T))),by=list(o_pp_sensitive_2),.SDcols=4:ncol(numbers)]
numbers_o_pp_sensitive_2[,o_pp_sensitive_2:=as.character(o_pp_sensitive_2)]
numbers_o_pp_sensitive_2[is.na(o_pp_sensitive_2),o_pp_sensitive_2:="Missing"]
setorder(numbers_o_pp_sensitive_2,o_pp_sensitive_2)
numbers_o_pp_sensitive_2[,analysisDAPCIMPG_pp_sensitive_1:=NULL]
numbers_o_pp_sensitive_2[,analysisDAPCIMPP_pp_sensitive_1:=NULL]
numbers_o_pp_sensitive_2[,analysisDAPCIMPG_pp_sensitive_3:=NULL]
numbers_o_pp_sensitive_2[,analysisDAPCIMPP_pp_sensitive_3:=NULL]

numbers_o_pp_sensitive_3 <- numbers[,lapply(.SD,function(x) return(sum(x,na.rm=T))),by=list(o_pp_sensitive_3),.SDcols=4:ncol(numbers)]
numbers_o_pp_sensitive_3[,o_pp_sensitive_3:=as.character(o_pp_sensitive_3)]
numbers_o_pp_sensitive_3[is.na(o_pp_sensitive_3),o_pp_sensitive_3:="Missing"]
setorder(numbers_o_pp_sensitive_3,o_pp_sensitive_3)
numbers_o_pp_sensitive_3[,analysisDAPCIMPG_pp_sensitive_1:=NULL]
numbers_o_pp_sensitive_3[,analysisDAPCIMPP_pp_sensitive_1:=NULL]
numbers_o_pp_sensitive_3[,analysisDAPCIMPG_pp_sensitive_2:=NULL]
numbers_o_pp_sensitive_3[,analysisDAPCIMPP_pp_sensitive_2:=NULL]

tab <- htmlTable(numbers_o_pp_sensitive_1,
                 rnames=FALSE,
                 header=c("Outcome","N","Has PG IF","Has PP IF", "Complete case PG", "Complete case PP"),
                 caption="Healthy post-partum versus depressed post-partum (EPDS or MINI)")
saveRDS(tab,file="results/dapc_numbers_o_pp_sensitive_1.RDS")

tab <- htmlTable(numbers_o_pp_sensitive_2,
                 rnames=FALSE,
                 header=c("Outcome","N","Has PG IF","Has PP IF", "Complete case PG", "Complete case PP"),
                 caption="Healthy throughout versus healthy in pregnancy and depressed post-partum (EPDS or MINI)")
saveRDS(tab,file="results/dapc_numbers_o_pp_sensitive_2.RDS")

tab <- htmlTable(numbers_o_pp_sensitive_3,
                 rnames=FALSE,
                 header=c("Outcome","N","Has PG IF","Has PP IF", "Complete case PG", "Complete case PP"),
                 caption="Healthy throughout versus dpressed in pregnancy (EPDS or MINI)")
saveRDS(tab,file="results/dapc_numbers_o_pp_sensitive_3.RDS")
}


RunMultipleDAPCs(d=d,
                 pganalysis1=d$analysisDAPCIMPG_pp_sensitive_1_antiNSAIDS,
                 pganalysis2=d$analysisDAPCIMPG_pp_sensitive_2_antiNSAIDS,
                 pganalysis3=d$analysisDAPCIMPG_pp_sensitive_3_antiNSAIDS,
                 ppanalysis1=d$analysisDAPCIMPP_pp_sensitive_1_antiNSAIDS,
                 ppanalysis2=d$analysisDAPCIMPP_pp_sensitive_2_antiNSAIDS,
                 ppanalysis3=d$analysisDAPCIMPP_pp_sensitive_3_antiNSAIDS,
                 naming="antiNSAIDS")

RunMultipleDAPCs(d=d,
                 pganalysis1=d$analysisDAPCIMPG_pp_sensitive_1_smoker,
                 pganalysis2=d$analysisDAPCIMPG_pp_sensitive_2_smoker,
                 pganalysis3=d$analysisDAPCIMPG_pp_sensitive_3_smoker,
                 ppanalysis1=d$analysisDAPCIMPP_pp_sensitive_1_smoker,
                 ppanalysis2=d$analysisDAPCIMPP_pp_sensitive_2_smoker,
                 ppanalysis3=d$analysisDAPCIMPP_pp_sensitive_3_smoker,
                 naming="smoker")

RunMultipleENs(d=d,
                 pganalysis1=d$analysisDAPCIMPG_pp_sensitive_1,
                 pganalysis2=d$analysisDAPCIMPG_pp_sensitive_2,
                 pganalysis3=d$analysisDAPCIMPG_pp_sensitive_3,
                 ppanalysis1=d$analysisDAPCIMPP_pp_sensitive_1,
                 ppanalysis2=d$analysisDAPCIMPP_pp_sensitive_2,
                 ppanalysis3=d$analysisDAPCIMPP_pp_sensitive_3,
                 naming="all")

RunMultipleENs(d=d,
                 pganalysis1=d$analysisDAPCIMPG_pp_sensitive_1_antiNSAIDS,
                 pganalysis2=d$analysisDAPCIMPG_pp_sensitive_2_antiNSAIDS,
                 pganalysis3=d$analysisDAPCIMPG_pp_sensitive_3_antiNSAIDS,
                 ppanalysis1=d$analysisDAPCIMPP_pp_sensitive_1_antiNSAIDS,
                 ppanalysis2=d$analysisDAPCIMPP_pp_sensitive_2_antiNSAIDS,
                 ppanalysis3=d$analysisDAPCIMPP_pp_sensitive_3_antiNSAIDS,
                 naming="antiNSAIDS")

RunMultipleENs(d=d,
                 pganalysis1=d$analysisDAPCIMPG_pp_sensitive_1_smoker,
                 pganalysis2=d$analysisDAPCIMPG_pp_sensitive_2_smoker,
                 pganalysis3=d$analysisDAPCIMPG_pp_sensitive_3_smoker,
                 ppanalysis1=d$analysisDAPCIMPP_pp_sensitive_1_smoker,
                 ppanalysis2=d$analysisDAPCIMPP_pp_sensitive_2_smoker,
                 ppanalysis3=d$analysisDAPCIMPP_pp_sensitive_3_smoker,
                 naming="smoker")


dapc <- list()
dapc[[length(dapc)+1]] <- readRDS("results/dapc_all-loadings.RDS")
dapc[[length(dapc)]]$exclusions <- "Exclude: None"
dapc[[length(dapc)+1]] <- readRDS("results/dapc_antiNSAIDS-loadings.RDS")
dapc[[length(dapc)]]$exclusions <- "Exclude: Anti/NSAIDs"
dapc[[length(dapc)+1]] <- readRDS("results/dapc_smoker-loadings.RDS")
dapc[[length(dapc)]]$exclusions <- "Exclude: Smokers"
dapc <- rbindlist(dapc)
dapc <- dapc[,c("names","val","supports","outcome","exposure","exclusions"),with=FALSE]



res <- merge(dapc,en,by=c("names","outcome","exposure","exclusions"))#,all.x=TRUE,all.y=TRUE)
dim(res)

q <- ggplot(res,aes(x=exclusions,y=names,fill=supports))
q <- q + geom_tile()
q <- q + facet_wrap(exposure~outcome)
q <- SMAOFormatGGPlot(q,xAngle=90, sizeMultiplier = 2, ncol=5)
SMAOpng("results/dapc_and_en.png",landscape=FALSE)
print(q)
dev.off()

d1 <- readRDS("results/en_all-loadings.RDS")
d1$exclusions <- "Exclude: None"
d2 <- readRDS("results/en_antiNSAIDS-loadings.RDS")
d2$exclusions <- "Exclude: Anti/NSAIDs"
d3 <- readRDS("results/en_smoker-loadings.RDS")
d3$exclusions <- "Exclude: Smokers"
en <- rbind(d1,d2,d3)



d1 <- readRDS("results/dapc_antiNSAIDS-crossvalidation.RDS")
d1 <- readRDS("results/dapc_all-loadings.RDS")
d1$exclusions <- "Exclude: None"
d2 <- readRDS("results/dapc_antiNSAIDS-loadings.RDS")
d2$exclusions <- "Exclude: Anti/NSAIDs"
d3 <- readRDS("results/dapc_smoker-loadings.RDS")
d3$exclusions <- "Exclude: Smokers"
dx <- rbind(d1,d2,d3)
dx$x <- paste0(dx$exposure,": ",dx$outcome)
dx$val <- as.numeric(dx$val)
dx$cat <- cut(dx$val,breaks=c(0,0.05,0.1,0.2,0.5,1))

q <- ggplot(dx,aes(x=exclusions,y=names,fill=cat))
q <- q + geom_tile()
q <- q + scale_fill_brewer(palette="Reds")
q <- q + facet_wrap(~x,scales="free_x")
q <- SMAOFormatGGPlot(q,xAngle=90, sizeMultiplier = 2, ncol=5)
SMAOpng("results/dapc.png",landscape=FALSE,h=1.5)
print(q)
dev.off()


d1 <- readRDS("results/dapc_all-crossvalidation.RDS")
d1$exclusions <- "Exclude: None"
d2 <- readRDS("results/dapc_antiNSAIDS-crossvalidation.RDS")
d2$exclusions <- "Exclude: Anti/NSAIDs"
d3 <- readRDS("results/dapc_smoker-crossvalidation.RDS")
d3$exclusions <- "Exclude: Smokers"
dx <- rbind(d1,d2,d3)
dx$x <- paste0(dx$exposure,": ",dx$outcome)

q <- ggplot(dx,aes(x=exclusions))
q <- q + geom_errorbar(aes(ymin=l,ymax=u),width=0,lwd=3)
q <- q + geom_point(aes(y=m),size=10)
q <- q + geom_point(aes(y=max),size=10,col="red")
q <- q + facet_wrap(~x,scales="free_x")
q <- q + scale_y_continuous("",lim=c(0,1))
q <- SMAOFormatGGPlot(q,xAngle=90, sizeMultiplier = 2, ncol=5)
SMAOpng("results/dapc_crossvalidation.png",landscape=FALSE)
print(q)
dev.off()

file <- system.file("extdata","report.Rmd",package="inflammation")
RmdToHTML(file,paste0("results/Report_",format(Sys.time(), "%Y_%m_%d"),".html"))
