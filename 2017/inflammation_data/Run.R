RAWmisc::InitialiseOpinionatedUnix("code_minor/2017/inflammation_data")

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

goodIFs <- ifs[,.(percUnderLOD=mean(underLOD,na.rm=T)),by=.(IF,tail)]
goodIFs <- goodIFs[percUnderLOD<0.5]
goodIFs[,percUnderLOD:=NULL]

ifs[,imNum:=NULL]
setnames(ifs,"val","im_log2")

ifs[,zscore:=(im_log2-mean(im_log2))/sd(im_log2),by=.(IF,tail)]
zs <- merge(ifs,goodIFs,by=c("IF","tail"))
zs <- zs[,.(im_log2=median(zscore,na.rm=T)),by=.(CustomDataR,tail)]
zs[,im_log2:=(im_log2-mean(im_log2))/sd(im_log2),by=.(tail)]
zs[,IF:="zscore"]
zs[,underLOD:=0]
ifs[,zscore:=NULL]

setorder(zs,CustomDataR)
zs

#CustomDataR   zscorePG
#1:          10 -0.1480937
#2:          13  1.2092408
#3:          31  0.6577556

ifs <- rbind(ifs,zs)
ifs <- dcast.data.table(ifs,CustomDataR~IF+tail,value.var=c("im_log2","underLOD"))

dim(ifs)

saveRDS(ifs,file=file.path(RAWmisc::PROJ$CLEAN,"inflammation_markers_log2_with_lod_sqrt2.RDS"))
openxlsx::write.xlsx(ifs,file=file.path(RAWmisc::PROJ$SHARED_TODAY,"inflammation_markers_log2_with_lod_sqrt2.xlsx"))

d1 <- haven::read_sav(file.path(RAWmisc::PROJ$RAW,"IM preg_depr_ae.sav"))
setDT(d1)
ifs[CustomDataR==10]
as.numeric(d1[KOD=="10",]$CSF1r/d1[KOD=="10",]$CSF1)
as.numeric(d1[KOD=="10",]$CXCL6r/d1[KOD=="10",]$CXCL6)
as.numeric(d1[KOD=="10",]$IL15RAr/d1[KOD=="10",]$IL15RA)
as.numeric(d1[KOD=="10",]$TWEAKr/d1[KOD=="10",]$TWEAK)

ifs[CustomDataR==10]$im_log2_189_TWEAK_pp
d1[KOD=="10",]$TWEAKr
d1[KOD=="10",]$TWEAK

x <- merge(ifs[,c("CustomDataR","im_log2_189_TWEAK_pg")],d1[,c("KOD","TWEAKr")],by.x="CustomDataR",by.y="KOD")
x[,g:=im_log2_189_TWEAK_pg/TWEAKr]
cor(x$im_log2_189_TWEAK_pg,x$TWEAKr, use="pairwise.complete.obs")

x <- merge(ifs[,c("CustomDataR","im_log2_152_PD_L1_pg")],d1[,c("KOD","TNFRSF9")],by.x="CustomDataR",by.y="KOD")
cor(x$im_log2_152_PD_L1_pg,x$TNFRSF9, use="pairwise.complete.obs")

f <- names(d1)[4:80]
for(i in f){
  nameA <- i
  nameB <- paste0(i,"r")
  print(i)
  print(sum(is.na(d1[[nameA]])))
  print(sum(is.na(d1[[nameB]])))
}


