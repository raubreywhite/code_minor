RAWmisc::InitialiseProject(
  PROJHOME = "/analyses/code_minor/hanna_paper_2/",
  PROJRAW = "/analyses/data_raw/hanna_paper_2/",
  PROJCLEAN = "/analyses/data_clean/hanna_paper_2",
  PROJBAKED = "/analyses/results_baked/hanna_paper_2/",
  PROJFINAL = "/analyses/results_final/hanna_paper_2/",
  PROJSHARED = "/dropbox/results_shared/hanna_paper_2/")

suppressWarnings(suppressMessages(library(data.table)))
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(pomp)))

assign("RUN_ALL", TRUE, envir=globalenv())
d <- haven::read_spss("/analyses/data_raw/hanna_paper_2/Meteorological_data_170124HH.sav")
d <- data.table(haven::read_spss("/analyses/data_raw/hanna_paper_2/Meteorological_data_170124HH.sav"))
unique(d$Month_year)