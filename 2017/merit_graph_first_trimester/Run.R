RAWmisc::AllowFileManipulationFromInitialiseProject()
RAWmisc::InitialiseProject(
  HOME = "/git/code_minor/2017/merit_graph_first_trimester/",
  RAW = "/analyses/data_raw/code_minor/2017/merit_graph_first_trimester/",
  CLEAN = "/analyses/data_clean/code_minor/2017/merit_graph_first_trimester",
  BAKED = "/analyses/results_baked/code_minor/2017/merit_graph_first_trimester/",
  FINAL = "/analyses/results_final/code_minor/2017/merit_graph_first_trimester/",
  SHARED = "/dropbox/results_shared/code_minor/2017/merit_graph_first_trimester/"
)

library(data.table)
library(ggplot2)

d <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"data.xlsx")))
d <- melt.data.table(d,id.vars="Label")
d

q <- ggplot(d,aes(x=Label,y=value,fill=variable))
q <- q + geom_bar(stat="identity")
q