RAWmisc::InitialiseProject(
  PROJHOME = "/analyses/code_minor/fhi_downsizing/",
  PROJRAW = "/analyses/data_raw/fhi_downsizing/",
  PROJCLEAN = "/analyses/data_clean/fhi_downsizing",
  PROJBAKED = "/analyses/results_baked/fhi_downsizing/",
  PROJFINAL = "/analyses/results_final/fhi_downsizing/",
  PROJSHARED = "/dropbox/results_shared/fhi_downsizing/")

suppressWarnings(suppressMessages(library(data.table)))
suppressWarnings(suppressMessages(library(ggplot2)))

d <- data.table(readxl::read_excel(file.path(RPROJ$PROJRAW,"2017_03_20.xlsx")))
d[,.(yearwork=sum(yearwork)*0.15),by=area]

d[,.(yearwork=sum(yearwork)),by=.(area,category)]
unique(d$area)
d[,area:=factor(area,levels=c(
  "Direktørens stab",
  "Kommunikasjonsavdelingen",
  "Instituttstab",
  "Kunnskapssenter",
  "Helsedata og digitalisering",
  "Psykisk og fysisk helse",
  "Smittevern, miljø og helse"))]
unique(d$area)

target <- 120
targetPerc <- target/sum(d$yearwork)

pd <- d[,.(yearwork=sum(yearwork)),by=.(area,category)]
pd[,category:=factor(category,levels=0:4)]
pdAll <- pd[,.(target=sum(yearwork)*(1-targetPerc)),by=area]
setorder(pd,area,category)

q <- ggplot(pd,aes(x=area,y=yearwork))
q <- q + geom_bar(aes(fill=category),stat="identity")
q <- q + geom_point(data=pdAll,mapping=aes(x=area,y=target),col="red",size=5)
q <- q + scale_fill_brewer("Kategori",palette="Set2")
q <- q + guides(fill = guide_legend(reverse = TRUE))
q <- q + scale_x_discrete("")
q <- q + scale_y_continuous("Årsverk")
q <- q + labs(title="Årsverk per i dag, og ønsket mål etter nedbemanning")
q <- q + RAWmisc::theme_SMAO()
q <- q + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
RAWmisc::SMAOpng(file.path(RPROJ$PROJSHARED,"område_etter_kategori.png"))
print(q)
RAWmisc::MakeFootnote("Rød prikk betyr ønsket målet",size=1.5)
dev.off()

pd <- d[,.(yearwork=sum(yearwork)),by=.(area,category)]
pd[,fired4:=0]
pd[category==4,fired4:=yearwork]
pd[,total:=sum(yearwork),by=area]
pd[,targetFired:=total*targetPerc]
pd <- dcast.data.table(pd,targetFired+area~category,value.var="yearwork")
setnames(pd,c("targetFired","area","cat0","cat1","cat2","cat3","cat4"))
pd[is.na(cat0),cat0:=0]
pd[is.na(cat1),cat1:=0]
pd[is.na(cat2),cat2:=0]
pd[is.na(cat3),cat3:=0]
pd[is.na(cat4),cat4:=0]

pd[,fired0:=0]
pd[,fired1:=0]
pd[,fired2:=0]
pd[,fired3:=0]
pd[,fired4:=0]

a <- as.matrix(pd[,c("cat0","cat1","cat2","cat3"),with=F])
a <- a %*% (c(0,0,1,2))
b <- c(pd$targetFired-pd$cat4)
multiplier <- c(b/a)
pd[,multiplier:=multiplier]
pd[,fired4:=cat4]
pd[,fired2:=round(multiplier*cat2,1)]
pd[,fired3:=round(2*multiplier*cat3,1)]
pd[,multiplier:=NULL]
pd[,targetFired:=round(targetFired,1)]
setnames(pd,c("spark_mål","område",
              "arsverk_kat0","arsverk_kat1","arsverk_kat2","arsverk_kat3","arsverk_kat4",
              "sparket_kat0","sparket_kat1","sparket_kat2","sparket_kat3","sparket_kat4"))
openxlsx::write.xlsx(pd,file.path(RPROJ$PROJSHARED,"antall_sparket_etter_område.xlsx"))

d[,m:=0.0]
for(i in 1:length(multiplier)){
  d[area==levels(d$area)[i],m:=multiplier[i]]
}
d[category==2,fired:=yearwork*m]
d[category==3,fired:=yearwork*m*2]
d[category==4,fired:=yearwork]
d[,fired:=round(fired,1)]
d[,targetFired:=NULL]
d[,total:=NULL]
d[,m:=NULL]
openxlsx::write.xlsx(d,file.path(RPROJ$PROJSHARED,"antall_sparket_detaljert.xlsx"))


