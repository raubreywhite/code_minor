RAWmisc::AllowFileManipulationFromInitialiseProject()

RAWmisc::InitialiseProject(
  HOME = "/git/code_minor/2017/inflammation_data/",
  RAW = "/analyses/data_raw/code_minor/2017/inflammation_data/",
  CLEAN = "/analyses/data_clean/code_minor/2017/inflammation_data",
  BAKED = "/analyses/results_baked/code_minor/2017/inflammation_data/",
  FINAL = "/analyses/results_final/code_minor/2017/inflammation_data/",
  SHARED = "/dropbox/results_shared/code_minor/2017/inflammation_data/")

dir.create(RAWmisc::PROJ$SHARED_TODAY)

library(data.table)
library(ggplot2)

ifs <- data.table(openxlsx::read.xlsx(file.path(RAWmisc::PROJ$RAW,"Poroma PEA results final.xlsx"),sheet=2))
ifs[Sample.ID=="1276",Sample.ID:=1267]
dim(ifs)

ifs <- melt.data.table(ifs,id="Sample.ID", variable.factor=F)
setnames(ifs,c("CustomDataR","IF","val"))

dim(ifs)
ifs <- ifs[!CustomDataR %in% c("8070","8669")]
dim(ifs)

ifs[,tail:="pg"]
ifs[stringr::str_detect(CustomDataR,"pp$"),tail:="pp"]
#ifs[,x:=CustomDataR]
ifs[,CustomDataR:=stringr::str_extract(CustomDataR,"[0-9]*")]
ifs[,CustomDataR:=as.numeric(CustomDataR)]
ifs[,val:=2^val]

ifs[,imNum:=as.numeric(stringr::str_extract(IF, "^[0-9][0-9][0-9]"))]

file <- file.path(RAWmisc::PROJ$RAW,"lod.txt")
lod <- as.data.frame(fread(file))
lod$lod <- as.numeric(stringr::str_replace_all(lod$lod, ",", "."))
lod$imNum <- as.numeric(stringr::str_extract(lod$factor, "^[0-9][0-9][0-9]"))
lod <- lod[,-1]
lod$lod <- 2^(lod$lod)

dim(ifs)
ifs <- merge(ifs,lod,by="imNum")
dim(ifs)

ifs[,underLOD:=0]
ifs[is.na(val),underLOD:=1]
ifs[is.na(val),val:=lod/sqrt(2)]
ifs[IF=="152_PD-L1" & CustomDataR==631, val:=NA]
ifs[,IF:=stringr::str_replace_all(IF,"-","_")]
ifs[,lod:=NULL]
ifs[,val:=log2(val)]

ifs[,imNum:=NULL]
setnames(ifs,"val","im_log2")
ifs <- dcast.data.table(ifs,CustomDataR~IF+tail,value.var=c("im_log2","underLOD"))

dim(ifs)

saveRDS(ifs,file=file.path(RAWmisc::PROJ$CLEAN,"inflammation_markers_log2_with_lod_sqrt2.RDS"))
openxlsx::write.xlsx(ifs,file=file.path(RAWmisc::PROJ$SHARED_TODAY,"inflammation_markers_log2_with_lod_sqrt2.xlsx"))


sum(!is.na(ifs$im_log2_101_IL_8_pg))+sum(!is.na(ifs$im_log2_101_IL_8_pp))

