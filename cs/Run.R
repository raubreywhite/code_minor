RAWmisc::InitialiseProject(
  PROJHOME = "/analyses/code_minor/cs/",
  PROJRAW = "/analyses/data_raw/cs/",
  PROJCLEAN = "/analyses/data_clean/cs",
  PROJBAKED = "/analyses/results_baked/cs/",
  PROJFINAL = "/analyses/results_final/cs/",
  PROJSHARED = "/dropbox/results_shared/cs/")

suppressWarnings(suppressMessages(library(data.table)))
suppressWarnings(suppressMessages(library(ggplot2)))

assign("RUN_ALL", TRUE, envir=globalenv())

f <- list.files(file.path(RPROJ$PROJRAW))[1:5]
d <- vector("list",length(f))
for(i in 1:length(f)) d[[i]] <- readRDS(file.path(RPROJ$PROJRAW,f[i]))
d <- rbindlist(d)
d[,href:=NULL]
d[,games:=stringr::str_count(scores,",")+1]
numGames <- max(d$games)
d[,paste0("S",1:numGames) := tstrsplit(scores, ",", fixed=TRUE)]
d[is.na(S1),S1:=paste0(scoreA,":",scoreB)]
d[,scoreA:=NULL]
d[,scoreB:=NULL]
d[,scores:=NULL]