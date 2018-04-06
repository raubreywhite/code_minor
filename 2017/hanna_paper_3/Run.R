if(.Platform$OS.type=="unix"){
  RAWmisc::UseRClone()
  RAWmisc::AllowFileManipulationFromInitialiseProject()
  
  if(dir.exists("/dropbox")){
    SHARED <- "/dropbox/clients/hanna/paper_3/richard/"
    RCLONE_SHARED <- NULL
  } else {
    SHARED <- "/tmp/results_shared/code_major/2017/klima_analyses/"
    RCLONE_SHARED <- "data:/clients/hanna/paper_3/richard/"
  }
  
  RAWmisc::InitialiseProject(
    HOME = "/git/code_minor/2017/hanna_paper_3/",
    RAW = "/tmp/data_raw/code_minor/2017/hanna_paper_3/",
    CLEAN = "/tmp/data_clean/code_minor/2017/hanna_paper_3",
    BAKED = "/tmp/results_baked/code_minor/2017/hanna_paper_3/",
    FINAL = "/tmp/results_final/code_minor/2017/hanna_paper_3/",
    SHARED = SHARED,
    RCLONE_RAW = "crypt:/data_raw/code_minor/2017/hanna_paper_3/",
    RCLONE_SHARED = RCLONE_SHARED
  )
}

library(data.table)
library(ggplot2)

CleanData()
ls()

SeasonalAnalysis()
SeasonalAdjustedIMPredictingDepression()
