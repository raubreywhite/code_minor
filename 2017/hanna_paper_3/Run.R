RAWmisc::InitialiseProject(
  HOME = "/git/code_minor/2017/hanna_paper_3/",
  RAW = "/analyses/data_raw/code_minor/2017/hanna_paper_3/",
  CLEAN = "/analyses/data_clean/code_minor/2017/hanna_paper_3",
  BAKED = "/analyses/results_baked/code_minor/2017/hanna_paper_3/",
  FINAL = "/analyses/results_final/code_minor/2017/hanna_paper_3/",
  SHARED = "/dropbox/clients/hanna/paper_3/richard/")

dir.create(RAWmisc::PROJ$SHARED_TODAY)

library(data.table)
library(ggplot2)

depPG <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"Codes and depression status_170928.xlsx"),sheet="Pregnancy"))
depPP <- data.table(readxl::read_excel(file.path(RAWmisc::PROJ$RAW,"Codes and depression status_170928.xlsx"),sheet="Postpartum"))
depPG <- depPG[,c("CustomDataR","added_epds_preg")]
depPP <- depPP[,c("CustomDataR","added_epds_pp")]

d <- data.table(haven::read_spss(file.path(RAWmisc::PROJ$RAW,"alla_large_participation_2017_09_01.sav")))

nrow(d)
d <- merge(d,depPG,by="CustomDataR",all.x=T)
nrow(d)

nrow(d)
d <- merge(d,depPP,by="CustomDataR",all.x=T)
nrow(d)

d[!is.na(added_epds_preg),v32_EPDS_D12_9R:=added_epds_preg]
d[!is.na(added_epds_pp),ppv6_EPDS_D_9R:=added_epds_pp]

#nrow(d)
#d <- merge(d,pregDep,by=c("CustomDataR"),all.x=T)
#nrow(d)

sum(d$im_participating_preg==1,na.rm=T)
sum(d$im_participating_pp==1,na.rm=T)

d[is.na(v32_EPDS_D2_9R),im_participating_preg:=0]
d[is.na(v32_SSRI),im_participating_preg:=0]
d[is.na(im_fasting_sample),im_participating_preg:=0]
d[is.na(im_sample_year_preg),im_participating_preg:=0]
d[is.na(SIN_366_preg),im_participating_preg:=0]
d[is.na(COS_366_preg),im_participating_preg:=0]

sum(d$im_participating_preg==1,na.rm=T)
sum(d$im_participating_pp==1,na.rm=T)

plot(d$im_103_BDNF_pp~d$COS_366_pp)
plot(d$im_103_BDNF_pp~d$SIN_366_pp)
summary(lm(d$im_103_BDNF_pp~d$SIN_366_pp+d$COS_366_pp))
summary(lm(d$im_102_VEGFA_pp~d$SIN_366_pp+d$COS_366_pp))
plot(d$im_102_VEGFA_pp~d$SIN_366_pp)
sd(d$im_103_BDNF_pp,na.rm=T)
sd(d$im_102_VEGFA_pp,na.rm=T)

pg_depressed <- c("v32_EPDS_D12_9R")
pp_depressed <- c("ppv6_EPDS_D_9R")

pg_confs <- c("v32_EPDS_D2_9R",
              "v32_SSRI",
              "im_fasting_sample",
              "im_sample_year_preg")

pp_confs <- c(
  "ppv6_EPDS_D2_9R",
  "im_sample_year_pp")

pg <- d[im_participating_preg==1,c(
  "CustomDataR",
  "zscorePG",
  "v32_EPDS_D2_9R",
  "v32_SSRI",
  "im_fasting_sample",
  "im_sample_year_preg",
  "SIN_366_preg",
  "COS_366_preg",
  pg_depressed),with=F]

pp <- d[im_participating_pp==1,c(
  "CustomDataR",
  "zscorePP",
  "ppv6_EPDS_D2_9R",
  "im_sample_year_pp",
  "SIN_366_pp",
  "COS_366_pp",
  pp_depressed),with=F
]

ifs <- readRDS(file=file.path("/analyses/data_clean/code_minor/2017/inflammation_data","inflammation_markers_log2_with_lod_sqrt2.RDS"))

n <- names(ifs)
n <- n[stringr::str_detect(n,"_pg$")]
ifs_pg <- ifs[,c("CustomDataR",n),with=F]

n <- names(ifs)
n <- n[stringr::str_detect(n,"_pp$")]
ifs_pp <- ifs[,c("CustomDataR",n),with=F]

dim(pg)
unique(pg$CustomDataR)[!unique(pg$CustomDataR) %in% unique(ifs_pg$CustomDataR)]
pg <- merge(pg,ifs_pg,by="CustomDataR")
dim(pg)

dim(pp)
pp <- merge(pp,ifs_pp,by="CustomDataR")
dim(pp)

pg_ims <- names(ifs_pg)
pg_ims <- pg_ims[stringr::str_detect(pg_ims,"^im_log2_[0-9]")]
pg_ims <- stringr::str_replace_all(pg_ims,"im_log2_","")

pp_ims <- names(ifs_pp)
pp_ims <- pp_ims[stringr::str_detect(pp_ims,"^im_log2_[0-9]")]
pp_ims <- stringr::str_replace_all(pp_ims,"im_log2_","")

pg_pcUnderLOD <- c()
pp_pcUnderLOD <- c()

for(i in 1:length(pg_ims)){
  pg_pcUnderLOD <- c(pg_pcUnderLOD,mean(pg[[sprintf("underLOD_%s",pg_ims[i])]]))
  pp_pcUnderLOD <- c(pp_pcUnderLOD,mean(pp[[sprintf("underLOD_%s",pp_ims[i])]]))
}

sum(pg_pcUnderLOD<0.25)
sum(pp_pcUnderLOD<0.25)

pg_ims <- pg_ims[pg_pcUnderLOD<0.25]
pp_ims <- pp_ims[pp_pcUnderLOD<0.25]

pg_ims <- c(paste0("im_log2_",pg_ims),"zscorePG")
pp_ims <- c(paste0("im_log2_",pp_ims),"zscorePP")

#openxlsx::write.xlsx(pg,file=file.path(RAWmisc::PROJ$HOME,"pg.xlsx"))
#openxlsx::write.xlsx(pp,file=file.path(RAWmisc::PROJ$HOME,"pp.xlsx"))
#
# 

sampleSize <- list()
pg_retval <- pp_retval <- list()
for(depressed in c("All","Depressed","Not-depressed")) for(k in 1:2){
  if(k==1){
    pgorpp <- "pg"
    ims <- pg_ims
    confs <- pg_confs
    dep <- pg_depressed
    data <- pg
  } else if(k==2){
    pgorpp <- "pp"
    ims <- pp_ims
    confs <- pp_confs
    dep <- pp_depressed
    data <- pp
  }
  
  if(depressed=="Depressed"){
    data <- data[get(dep)==1]
  } else if(depressed=="Not-depressed"){
    data <- data[get(dep)==0]
  }
  
  retval <- vector("list",length(ims))
  for(i in 1:length(ims)){
    print(pg_ims[i])
    formula0 <- sprintf("%s ~ %s",ims[i],paste0(confs,collapse="+"))
    if(k==1){
      formula1 <- sprintf("%s ~ %s +SIN_366_preg + COS_366_preg",ims[i],paste0(confs,collapse="+"))
    } else {
      formula1 <- sprintf("%s ~ %s +SIN_366_pp + COS_366_pp",ims[i],paste0(confs,collapse="+"))
    }
    fit0 <- lm(as.formula(formula0),data=data)
    fit1 <- lm(as.formula(formula1),data=data)
    temp <- as.data.frame(coef(summary(fit1)))
    names(temp) <- c("beta","se","t","p")
    temp$var <- row.names(temp)
    temp$pseasonality <- anova(fit1,fit0)$`Pr(>F)`[2]
    
    b1 <- temp$beta[nrow(temp)-1] # sin
    b2 <- temp$beta[nrow(temp)] # cos
    
    temp$amplitude <- sqrt(b1^2 + b2^2)
    p <- atan(b1/b2)*366/2/pi
    if(p>0){
      peak <- p
      trough <- p+366/2
    } else {
      peak <- p+366/2
      trough <- p+366
    }
    if(b1<0){
      g <- peak
      peak <- trough
      trough <- g
    }
    temp$peak <- peak
    temp$trough <- trough
    temp$im <- ims[i]
    
    temp$depressed <- depressed
    temp$meanValueLog2 <- mean(data[[ims[i]]],na.rm=T)
    
    retval[[i]] <- temp
  }
  if(k==1){
    pg_retval[[depressed]] <- retval
  } else {
    pp_retval[[depressed]] <- retval
  }
  
  index <- length(sampleSize)+1
  sampleSize[[index]] <- data.frame(pgorpp,depressed,n=nrow(data))
}

sampleSize <- rbindlist(sampleSize)
setorder(sampleSize,pgorpp,depressed)

openxlsx::write.xlsx(sampleSize,file=file.path(RAWmisc::PROJ$SHARED_TODAY,"samplesize.xlsx"))

for(depressed in c("All","Depressed","Not-depressed")){
  pg_retval[[depressed]] <- rbindlist(pg_retval[[depressed]])
  pp_retval[[depressed]] <- rbindlist(pp_retval[[depressed]])
}
pg_retval <- rbindlist(pg_retval)
pp_retval <- rbindlist(pp_retval)

pg_retval[!im %in% c("zscorePG","zscorePP"),trough_to_peak_change:=100*(2^(2*amplitude)-1)]
pg_retval[im %in% c("zscorePG","zscorePP"),trough_to_peak_change:=2*amplitude]
pg_retval[,trough_to_peak_change:=RAWmisc::Format(trough_to_peak_change,1)]
pg_retval[!im %in% c("zscorePG","zscorePP"),trough_to_peak_change:=sprintf("%s%%",trough_to_peak_change)]
pg_retval[,peak:=RAWmisc::Format(peak,digits=0)]
pg_retval[,trough:=RAWmisc::Format(trough,digits=0)]

pp_retval[!im %in% c("zscorePG","zscorePP"),trough_to_peak_change:=100*(2^(2*amplitude)-1)]
pp_retval[im %in% c("zscorePG","zscorePP"),trough_to_peak_change:=2*amplitude]
pp_retval[,trough_to_peak_change:=RAWmisc::Format(trough_to_peak_change,1)]
pp_retval[!im %in% c("zscorePG","zscorePP"),trough_to_peak_change:=sprintf("%s%%",trough_to_peak_change)]
pp_retval[,peak:=RAWmisc::Format(peak,digits=0)]
pp_retval[,trough:=RAWmisc::Format(trough,digits=0)]

for(k in 1:2){
  if(k==1){
    x <- pg_retval[var=="(Intercept)"]
  } else {
    x <- pp_retval[var=="(Intercept)"]
  }
  
  tab <- x[,c("im","trough_to_peak_change","peak","trough","depressed","meanValueLog2","pseasonality"),with=F]
  tab[,pbonf:=pseasonality*.N/3]
  tab[pbonf>1,pbonf:=1]
  tab[,pbonf:=RAWmisc::Format(pbonf,digits=3)]
  tab[,pseasonality:=RAWmisc::Format(pseasonality,digits=3)]
  
  tab[,meanValueLog2:=RAWmisc::Format(meanValueLog2,digits=1)]
  
  tab[pbonf<"0.050",peak:=sprintf("* %s",peak)]
  tab[pbonf<"0.050",trough:=sprintf("* %s",trough)]
  tab[pbonf<"0.050",trough_to_peak_change:=sprintf("* %s",trough_to_peak_change)]
  
  tab <- dcast.data.table(tab,im~depressed,value.var = c("peak","trough","meanValueLog2","pseasonality","pbonf","trough_to_peak_change"))
  openxlsx::write.xlsx(tab,file=file.path(RAWmisc::PROJ$SHARED_TODAY,sprintf("table%s.xlsx",k+1)))
}



toPlot <- vector("list",2)
for(k in 1:2){
  if(k==1){
    x <- pg_retval[var %in% c("im_sample_year_preg","SIN_366_preg","COS_366_preg")]
    RAWmisc::RecodeDT(x,c(
      "im_sample_year_preg"="year_trend",
      "SIN_366_preg"="sin366",
      "COS_366_preg"="cos366"
        ),
      "var")
    
    details <- pg_retval[var=="(Intercept)"]
  } else {
    x <- pp_retval[var %in% c("im_sample_year_pp","SIN_366_pp","COS_366_pp")]
    RAWmisc::RecodeDT(x,c(
      "im_sample_year_pp"="year_trend",
      "SIN_366_pp"="sin366",
      "COS_366_pp"="cos366"
    ),
    "var")
    
    details <- pp_retval[var=="(Intercept)"]
  }
  
  x[,var:=factor(var,levels=c("sin366","cos366","year_trend"))]
  x[,ci_95:=sprintf("%s, %s",
                    RAWmisc::Format(beta-1.96*se, digits=3),
                    RAWmisc::Format(beta+1.96*se, digits=3)
                    )]
  x <- x[,c(
    "depressed",
    "im",
    "var",
    "beta",
    "se",
    "ci_95",
    "p",
    "trough_to_peak_change",
    "peak",
    "trough",
    "pseasonality"
  )]
  x[var!="sin366",pseasonality:=NA]
  x[var=="sin366",pbonf:=stats::p.adjust(pseasonality,method="bonf"),by=depressed]
  toPlot[[k]] <- copy(x)
  x[,beta:=RAWmisc::Format(beta,digits=3)]
  x[,se:=RAWmisc::Format(se,digits=3)]
  x[,p:=RAWmisc::Format(p,digits=3)]
  x[,pseasonality:=RAWmisc::Format(pseasonality,digits=3)]
  x[,pbonf:=RAWmisc::Format(pbonf,digits=3)]
  
  x[pseasonality=="  NA",pseasonality:=""]
  x[pbonf=="  NA",pbonf:=""]
  
  x[pbonf=="",trough_to_peak_change:=""]
  x[pbonf=="",peak:=""]
  x[pbonf=="",trough:=""]
  
  setorder(x,depressed,im,var)
  
  openxlsx::write.xlsx(x,file=file.path(RAWmisc::PROJ$SHARED_TODAY,sprintf("supp_table%s.xlsx",k)))
}

toPlot <- rbindlist(toPlot)

# PLOTTING
toPlot <- toPlot[var %in% c("sin366","cos366") & !im %in% c("zscorePG","zscorePP"),c("depressed","im","var","beta","pbonf"),with=F]
toPlot[,pbonf:=mean(pbonf,na.rm=T),by=.(depressed,im)]

stack <- dcast.data.table(toPlot,depressed+im+pbonf~var, value.var="beta")
stack[,cos366:=as.numeric(cos366)]
stack[,sin366:=as.numeric(sin366)]

data <- vector("list",length=nrow(stack))
for(i in 1:length(data)){
  data[[i]] <- data.table(Depressed=stack$depressed[i],IF=stack$im[i],day=1:366,pbonf=stack$pbonf[i])
  data[[i]][,y:=stack$cos366[i]*cos(2*pi*day/366) + stack$sin366[i]*sin(2*pi*day/366)]
}

masterData <- rbindlist(data)
data <- rbindlist(data)[pbonf<0.05]

data[,date:=as.Date("2016-12-31")+day]
data[,labels:=""]
data[IF %in% c("zscorePG",
               "im_log2_101_IL_8_pg",
               "im_log2_136_MCP_4_pp",
               "im_log2_172_SIRT2_pg",
               "im_log2_118_AXIN1_pg",
               "im_log2_192_STAMPB_pg"),labels:=IF]
data[,labels:=stringr::str_replace(labels,"im_log2_[0-9][0-9][0-9]_","")]
unique(data$labels)
RAWmisc::RecodeDT(data,switch=c(
  "IL_8_pg"="IL-8 (pregnancy)",
  "AXIN1_pg"="AXIN1 (pregnancy)",
  "MCP_4_pp"="MCP-4 (postpartum)",
  "SIRT2_pg"="SIRT2 (pregnancy)",
  "STAMPB_pg"="STAM-BP (pregnancy)"
), var="labels")
unique(data$labels)
l <- unique(data$labels)
l_pp <- l[stringr::str_detect(l,"\\(postpartum\\)$")]
l_pg <- l[stringr::str_detect(l,"\\(pregnancy\\)$")]
data[,labels:=factor(labels,levels = c("zscorePG",l_pg,l_pp,""))]

saveA4 <- function(q,filename,landscape=T){
  ggsave(filename,plot=q,width=297,height=210, units="mm")
}

q <- ggplot(as.data.frame(data), aes(x=date,y=y,group=IF))
q <- q + geom_line(data=data[labels==""])
q <- q + geom_line(data=data[labels!=""],mapping=aes(colour=labels),lwd=1)
#q <- q + expand_limits(x=as.Date("2016-08-01"))
q <- q + scale_colour_manual("",values=c("#e41a1c", "#377eb8", "#4daf4a", "#ff7f00", "#f781bf"))
#q <- q + scale_colour_brewer("",palette="Set2")
q <- q + scale_x_date("Day/month", labels = scales::date_format("%d/%m"),
                      breaks=as.Date(c("2017-01-01",
                                       "2017-03-01",
                                       "2017-05-01",
                                       "2017-07-01",
                                       "2017-09-01",
                                       "2017-11-01",
                                       "2018-01-01")),
                      minor_breaks=as.Date(c("2017-02-01",
                                             "2017-04-01",
                                             "2017-06-01",
                                             "2017-08-01",
                                             "2017-10-01",
                                             "2017-12-01",
                                             "2018-01-01")))
q <- q + scale_y_continuous("Change in NPX [log2(concentration)]")
q <- q + theme_gray(base_size=16)
q <- q + facet_wrap(~Depressed,ncol=1)
RAWmisc::saveA4(q,filename=file.path(RAWmisc::PROJ$SHARED_TODAY,"figure_sig.png"))



data <- masterData
data[y>1]

data[,date:=as.Date("2016-12-31")+day]

data[,labels:="Not significant PG"]
data[stringr::str_detect(IF,"_pp$"),labels:="Not significant PP"]
data[pbonf<0.05 & stringr::str_detect(IF,"_pg$"),labels:="Significant PG"]
data[pbonf<0.05 & stringr::str_detect(IF,"_pp$"),labels:="Significant PP"]

q <- ggplot(as.data.frame(data), aes(x=date,y=y,group=IF,colour=labels))
q <- q + geom_line(data=data[labels %in% c("Not significant PG","Not significant PP")],mapping=aes(colour=labels),lwd=0.25,alpha=0.5)
q <- q + geom_line(data=data[!labels %in% c("Not significant PG","Not significant PP")],mapping=aes(colour=labels))
#q <- q + expand_limits(x=as.Date("2016-08-01"))
q <- q + scale_colour_manual("",values=c("black", "#377eb8", "#4daf4a", "#ff7f00", "#f781bf"))
#q <- q + scale_colour_brewer("",palette="Set2")
q <- q + scale_x_date("Day/month", labels = scales::date_format("%d/%m"),
                      breaks=as.Date(c("2017-01-01",
                                       "2017-03-01",
                                       "2017-05-01",
                                       "2017-07-01",
                                       "2017-09-01",
                                       "2017-11-01",
                                       "2018-01-01")),
                      minor_breaks=as.Date(c("2017-02-01",
                                             "2017-04-01",
                                             "2017-06-01",
                                             "2017-08-01",
                                             "2017-10-01",
                                             "2017-12-01",
                                             "2018-01-01")))
q <- q + scale_y_continuous("Change in NPX [log2(concentration)]")
q <- q + theme_gray(base_size=16)
q <- q + facet_wrap(~Depressed,ncol=1)
RAWmisc::saveA4(q,filename=file.path(RAWmisc::PROJ$SHARED_TODAY,"figure_all.png"))


