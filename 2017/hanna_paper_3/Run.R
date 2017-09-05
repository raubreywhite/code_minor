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

d <- data.table(haven::read_spss(file.path(RAWmisc::PROJ$RAW,"alla_large_participation_2017_09_01.sav")))
d[is.na(v32_EPDS_D2_9R),im_participating_preg:=0]
d[is.na(v32_SSRI),im_participating_preg:=0]
d[is.na(im_fasting_sample),im_participating_preg:=0]
d[is.na(im_sample_year_preg),im_participating_preg:=0]
d[is.na(SIN_366_preg),im_participating_preg:=0]
d[is.na(COS_366_preg),im_participating_preg:=0]

d[CustomDataR=="1267"]$im_118_AXIN1

sum(d$im_participating_preg==1,na.rm=T)
sum(d$im_participating_pp==1,na.rm=T)


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
  "COS_366_preg")]

pp <- d[im_participating_pp==1,c(
  "CustomDataR",
  "zscorePP",
  "ppv6_EPDS_D2_9R",
  "im_sample_year_pp",
  "SIN_366_pp",
  "COS_366_pp"
)]

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

for(k in 1:2){
  if(k==1){
    ims <- pg_ims
    confs <- pg_confs
    data <- pg
  } else if(k==2){
    ims <- pp_ims
    confs <- pp_confs
    data <- pp
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
    retval[[i]] <- temp
  }
  if(k==1){
    pg_retval <- retval
  } else {
    pp_retval <- retval
  }
}

pg_retval <- rbindlist(pg_retval)
pp_retval <- rbindlist(pp_retval)

for(k in 1:2){
  if(k==1){
    x <- pg_retval[var=="(Intercept)"]
  } else {
    x <- pp_retval[var=="(Intercept)"]
  }
  
  x[,bonf:=pseasonality*(.N)]
  tab <- x[bonf<0.05]
  setorder(tab,pseasonality)
  
  tab <- tab[,c("im","amplitude","peak","trough","bonf"),with=F]
  tab[,amplitude:=RAWmisc::Format(amplitude)]
  tab[,peak:=RAWmisc::Format(peak,digits=0)]
  tab[,trough:=RAWmisc::Format(trough,digits=0)]
  tab[,bonf:=RAWmisc::Format(bonf,digits=3)]
  tab
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
  } else {
    x <- pp_retval[var %in% c("im_sample_year_pp","SIN_366_pp","COS_366_pp")]
    RAWmisc::RecodeDT(x,c(
      "im_sample_year_pp"="year_trend",
      "SIN_366_pp"="sin366",
      "COS_366_pp"="cos366"
    ),
    "var")
  }
  
  x[,var:=factor(var,levels=c("sin366","cos366","year_trend"))]
  x[,ci_95:=sprintf("%s, %s",
                    RAWmisc::Format(beta-1.96*se, digits=3),
                    RAWmisc::Format(beta+1.96*se, digits=3)
                    )]
  x <- x[,c(
    "im",
    "var",
    "beta",
    "se",
    "ci_95",
    "p",
    "pseasonality"
  )]
  x[var!="sin366",pseasonality:=NA]
  x[var=="sin366",pbonf:=stats::p.adjust(pseasonality,method="bonf")]
  toPlot[[k]] <- copy(x)
  x[,beta:=RAWmisc::Format(beta,digits=3)]
  x[,se:=RAWmisc::Format(se,digits=3)]
  x[,p:=RAWmisc::Format(p,digits=3)]
  x[,pseasonality:=RAWmisc::Format(pseasonality,digits=3)]
  x[,pbonf:=RAWmisc::Format(pbonf,digits=3)]
  
  x[pseasonality=="  NA",pseasonality:=""]
  x[pbonf=="  NA",pbonf:=""]
  
  setorder(x,im,var)
  
  openxlsx::write.xlsx(x,file=file.path(RAWmisc::PROJ$SHARED_TODAY,sprintf("supp_table%s.xlsx",k)))
  
}

toPlot <- rbindlist(toPlot)

# PLOTTING
toPlot <- toPlot[var %in% c("sin366","cos366"),c("im","var","beta","pbonf"),with=F]
toPlot[,pbonf:=mean(pbonf,na.rm=T),by=im]

stack <- dcast.data.table(toPlot,im+pbonf~var, value.var="beta")
stack[,cos366:=as.numeric(cos366)]
stack[,sin366:=as.numeric(sin366)]

data <- vector("list",length=nrow(stack))
for(i in 1:length(data)){
  data[[i]] <- data.table(IF=stack$im[i],day=1:366,pbonf=stack$pbonf[i])
  data[[i]][,y:=stack$cos366[i]*cos(2*pi*day/366) + stack$sin366[i]*sin(2*pi*day/366)]
}

masterData <- rbindlist(data)
data <- rbindlist(data)[pbonf<0.05]

data[,date:=as.Date("2016-12-31")+day]
data[,labels:=""]
data[IF %in% c("zscorePG",
               "im_log2_136_MCP_4_pp",
               "im_log2_172_SIRT2_pg",
               "im_log2_118_AXIN1_pg",
               "im_log2_192_STAMPB_pg"),labels:=IF]
data[,labels:=stringr::str_replace(labels,"im_log2_[0-9][0-9][0-9]_","")]
l <- unique(data$labels)
l_pp <- l[stringr::str_detect(l,"_pp$")]
l_pg <- l[stringr::str_detect(l,"_pg$")]
data[,labels:=factor(labels,levels = c("zscorePG",l_pg,l_pp,""))]

saveA4 <- function(q,filename,landscape=T){
  ggsave(filename,plot=q,width=297,height=210, units="mm")
}

q <- ggplot(as.data.frame(data), aes(x=date,y=y,group=IF))
q <- q + geom_line(data=data[labels==""],lwd=1)
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
q <- q + scale_y_continuous("")
q <- q + theme_gray(base_size=16)
#q <- q + ggrepel::geom_text_repel(data=data[day==1],
#                                  mapping=aes(label = labels, colour=labels), size=6,
#                                  segment.size=1,
#                                  nudge_x = -200)
#q <- q + directlabels::geom_dl(aes(label = labels), method = list("first.qp", cex = 4))
#q <- q + theme(legend.position="none")
saveA4(q,filename=file.path(RAWmisc::PROJ$SHARED_TODAY,"figure_sig.png"))



data <- masterData
data[y>1]

data[,date:=as.Date("2016-12-31")+day]

data[,labels:="Not significant PG"]
data[stringr::str_detect(IF,"_pp$"),labels:="Not significant PP"]
data[pbonf<0.05 & stringr::str_detect(IF,"_pg$"),labels:="Significant PG"]
data[pbonf<0.05 & stringr::str_detect(IF,"_pp$"),labels:="Significant PP"]

q <- ggplot(as.data.frame(data), aes(x=date,y=y,group=IF,colour=labels))
q <- q + geom_line(data=data[labels %in% c("Not significant PG","Not significant PP")],mapping=aes(colour=labels),lwd=0.5,alpha=0.5)
q <- q + geom_line(data=data[!labels %in% c("Not significant PG","Not significant PP")],mapping=aes(colour=labels),lwd=0.75)
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
q <- q + scale_y_continuous("")
q <- q + theme_gray(base_size=16)

#q <- q + directlabels::geom_dl(aes(label = labels), method = list("first.qp", cex = 4))
saveA4(q,filename=file.path(RAWmisc::PROJ$SHARED_TODAY,"figure_all.png"))


