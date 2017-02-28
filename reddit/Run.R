RAWmisc::InitialiseProject(
  PROJHOME = "/analyses/code_minor/safer_births/",
  PROJRAW = "/analyses/data_raw/safer_births/",
  PROJCLEAN = "/analyses/data_clean/safer_births",
  PROJBAKED = "/analyses/results_baked/safer_births/",
  PROJFINAL = "/analyses/results_final/safer_births/",
  PROJSHARED = "/dropbox/results_shared/safer_births/")

suppressWarnings(suppressMessages(library(data.table)))
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(tidyr)))
suppressWarnings(suppressMessages(library(magrittr)))
suppressWarnings(suppressMessages(library(pomp)))
library(rstanarm)

assign("RUN_ALL", TRUE, envir=globalenv())

#unlink(file.path(RPROJ$PROJSHARED,lubridate::today()), recursive=TRUE, force=TRUE)
#dir.create(file.path(RPROJ$PROJSHARED,lubridate::today()))

#rmarkdown::render("Notes.Rmd", output_dir = file.path(RPROJ$PROJSHARED,lubridate::today()))
