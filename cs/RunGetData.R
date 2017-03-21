RAWmisc::InitialiseProject(
  PROJHOME = "/analyses/code_minor/cs/",
  PROJRAW = "/analyses/data_raw/cs/",
  PROJCLEAN = "/analyses/data_clean/cs",
  PROJBAKED = "/analyses/results_baked/cs/",
  PROJFINAL = "/analyses/results_final/cs/",
  PROJSHARED = "/dropbox/results_shared/cs/")

suppressWarnings(suppressMessages(library(data.table)))

UpdateLastWeekData()
UpdateOdds()