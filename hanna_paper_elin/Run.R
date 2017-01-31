RAWmisc::InitialiseProject(
  PROJHOME = "/analyses/code_minor/hanna_paper_elin/",
  PROJRAW = "/analyses/data_raw/hanna_paper_elin/",
  PROJCLEAN = "/analyses/data_clean/hanna_paper_elin",
  PROJBAKED = "/analyses/results_baked/hanna_paper_elin/",
  PROJFINAL = "/analyses/results_final/hanna_paper_elin/",
  PROJSHARED = "/dropbox/results_shared/hanna_paper_elin/")

suppressWarnings(suppressMessages(library(data.table)))
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(tidyr)))
suppressWarnings(suppressMessages(library(magrittr)))
suppressWarnings(suppressMessages(library(pomp)))

assign("RUN_ALL", TRUE, envir=globalenv())

unlink(file.path(RPROJ$PROJSHARED,lubridate::today()), recursive=TRUE, force=TRUE)
dir.create(file.path(RPROJ$PROJSHARED,lubridate::today()))

CleanData()
rmarkdown::render("Notes.Rmd", output_dir = file.path(RPROJ$PROJSHARED,lubridate::today()))
cmd <- sprintf("cd %s; zip -P sqo391 %s %s",
               RPROJ$PROJBAKED,
               file.path(RPROJ$PROJSHARED,lubridate::today(),"data.zip"),
               "data_inflammation_factors.xlsx")
system(cmd)

