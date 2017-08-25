Sys.setenv(R_TOOLS="C:\\Apps\\Rtools")
Sys.setenv(RSTUDIO_PANDOC="C:\\Apps\\RStudio\\bin\\pandoc")
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
library(Hmisc)
library(stringr)
library(magrittr)
library(tidyr)


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

xtabs(~d$data$c_breastfeeding)


xtabs(~d$data$o_pp_sensitive_2[d$analysisDAPCIMPG_pp_sensitive_2])

nrow(d$data)
sum(!is.na(d$data$meanZPG))
sum(!is.na(d$data$meanZPP))

d$downIF
d$upIF

#### TRIAL 
form <- "o_pp_sensitive_2~l_im_194_ADA+l_im_192_STAMPB+l_im_118_AXIN1"
for(i in d$modelPG1) form <- paste0(form,"+",i)
summary(glm(as.formula(form),data=d$data,family="binomial"))

form <- "o_pp_sensitive_2~l_im_194_ADA"
for(i in d$modelPG1) form <- paste0(form,"+",i)
summary(glm(as.formula(form),data=d$data,family="binomial"))

# Figure 1
IMClusters(d)

# Figure 2
BoxPlotsByCaseSpecific(d,
                       pgKeep=c("l_im_194_ADA","l_im_192_STAMPB","l_im_184_CASP8","l_im_162_IL10","l_im_118_AXIN1"))

# Figure 3, Table S1
IndividualIFAnalyses(d)

# Figure 4
ZScoreBoxPlots(d)

# Table 1
TableDemographics(d)

# Table 2
AssociationExposureDemographics(d,extraPG=c("c_firstBorn","c_caesarean","c_breastfeedingOnly"))

# Table 3
ZScoreAnalysesLassoTest(d)

# Figure S1
BoxPlotsByCase(d)

# Figure S2
BoxPlotsByCaseSpecific(d,
                       pgKeep=c("l_im_194_ADA","l_im_192_STAMPB","l_im_184_CASP8","l_im_162_IL10","l_im_118_AXIN1"))


# SUPPLEMETNAL TABLES 
IFSummaryTables()


file <- system.file("extdata","results_pg.Rmd",package="inflammation")
RmdToHTML(file,paste0("results/ResultsPG_",format(Sys.time(), "%Y_%m_%d"),".html"))

file <- system.file("extdata","results_pp.Rmd",package="inflammation")
RmdToHTML(file,paste0("results/ResultsPP_",format(Sys.time(), "%Y_%m_%d"),".html"))

file <- system.file("extdata","supplemental_tables.Rmd",package="inflammation")
RmdToHTML(file,paste0("results/SupplementalTables_",format(Sys.time(), "%Y_%m_%d"),".html"))


