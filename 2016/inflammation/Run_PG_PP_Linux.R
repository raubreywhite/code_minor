RAWmisc::InitialiseProject(
  PROJHOME = "/analyses/code_minor/inflammation/",
  PROJRAW = "/analyses/data_raw/inflammation/",
  PROJCLEAN = "/analyses/data_clean/inflammation",
  PROJBAKED = "/analyses/results_baked/inflammation/",
  PROJFINAL = "/analyses/results_final/inflammation/",
  PROJSHARED = "/dropbox/results_shared/inflammation/")
library(data.table)
library(Hmisc)
library(stringr)
library(magrittr)
library(tidyr)
library(htmlTable)

dir.create(file.path(RPROJ$PROJSHARED,lubridate::today()))

SMAOpng <- RAWmisc::SMAOpng

d <- CleanData()


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

file <- system.file("extdata","supplemental_tables.Rmd",package="inflammation")
RAWmisc::RmdToDOCX("reports_skeleton/supplemental_tables.Rmd",paste0("reports_formatted/SupplementalTables_",format(Sys.time(), "%Y_%m_%d"),".docx"),
                   copyFrom="reports_skeleton")



## V2
summary(d$data$c_lengthPG)
na.omit(d$data$CustomDataR[d$data$c_lengthPG<0])

d$modelPG2
d$modelPP2


