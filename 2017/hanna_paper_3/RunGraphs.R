RAWmisc::AllowFileManipulationFromInitialiseProject()

RAWmisc::InitialiseProject(
  HOME = "/git/code_minor/hanna_paper_3/",
  RAW = "/analyses/data_raw/hanna_paper_3/",
  CLEAN = "/analyses/data_clean/hanna_paper_3",
  BAKED = "/analyses/results_baked/hanna_paper_3/",
  FINAL = "/analyses/results_final/hanna_paper_3/",
  SHARED = "/dropbox/clients/hanna/paper_3/richard/")

dir.create(RAWmisc::PROJ$SHARED_TODAY)

library(data.table)
library(ggplot2)

data <- readxl::read_xlsx(file.path(RAWmisc::PROJ$RAW,"Final tables paper 3.xlsx"),sheet="Calculations")
data$`Inflammation factor` <- c(NA,zoo::na.locf(data$`Inflammation factor`))
data <- data.table(data[data$Variable %in% c("sin366","cos366"),
                        c("Inflammation factor","Variable","B coefficient")])

setnames(data,c("IF","var","beta"))

stack <- dcast.data.table(data,IF~var, value.var="beta")
stack[,cos366:=as.numeric(cos366)]
stack[,sin366:=as.numeric(sin366)]

data <- vector("list",length=nrow(stack))
for(i in 1:length(data)){
  data[[i]] <- data.table(IF=stack$IF[i],day=1:366)
  data[[i]][,y:=stack$cos366[i]*cos(2*pi*day/366) + stack$sin366[i]*sin(2*pi*day/366)]
}

data <- rbindlist(data)
data[,date:=as.Date("2016-12-31")+day]
data[,labels:=""]
data[IF %in% c("zscorePG",
               "im_136_MCP4_pp",
               "im_172_SIRT2",
               "im_118_AXIN1",
               "im_192_STAMPB"),labels:=IF]

q <- ggplot(as.data.frame(data), aes(x=date,y=y,group=IF))
q <- q + geom_line(aes(colour=labels),lwd=3)
q <- q + expand_limits(x=as.Date("2016-11-01"))
q <- q + scale_colour_manual("",values=c("black","#e41a1c", "#377eb8", "#4daf4a", "#ff7f00", "#f781bf"))
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
q <- q + theme_gray(base_size=60)
q <- q + ggrepel::geom_text_repel(data=data[day==1],
                                   mapping=aes(label = labels, colour=labels), size=20, segment.size=3, nudge_x = -40)
#q <- q + directlabels::geom_dl(aes(label = labels), method = list("first.qp", cex = 4))
q <- q + theme(legend.position="none")
RAWmisc::png_a4(file.path(RAWmisc::PROJ$SHARED_TODAY,"figure.png"),landscape=TRUE)
print(q)
dev.off()

