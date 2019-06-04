## TODO: test proportional hazards
# https://www.uio.no/studier/emner/matnat/math/STK4080/h16/undervisningsmateriell/lecture12_16.pdf

org::AllowFileManipulationFromInitialiseProject()
org::InitialiseProject(
  HOME = "/git/code_minor/2019/israeli_elections/",
  RAW = "/data/org/data_raw/code_minor/2019/israeli_elections/",
  SHARED = "/dropbox/analyses/results_shared/code_minor/2019/israeli_elections/"
)

library(data.table)
library(ggplot2)

d <- readxl::read_excel(file.path(org::PROJ$RAW,"Background for R-11.xlsx"))
setDT(d)

d[,type:="Above"]
d[Name %in% c(
  "Shas (Deri)",
  "Ra'am/Balad (Ghnaim/Abbas)",
  "Meretz (Zandberg)",
  "URWP (Peretz)"
),type:="Below"]

q <- ggplot(d,aes(x=xlevel,y=poll,colour=coloring,label=Name))
q <- q + annotate("rect",xmin=-Inf,xmax=Inf,ymin=0,ymax=4,fill="black",alpha=0.4)
#q <- q + annotate("text", x = -0.1, y = 4.35, label = "sperregrense",hjust=1)
q <- q + geom_vline(xintercept=0)
q <- q + geom_hline(yintercept=0)
q <- q + geom_hline(yintercept=30)
q <- q + geom_point(mapping=aes(size=poll))
q <- q + ggrepel::geom_label_repel(data=d[type=="Above"],alpha=0.9, nudge_y=1)
q <- q + ggrepel::geom_label_repel(data=d[type=="Below"],alpha=0.9, nudge_y=-1.5)
q <- q + geom_text(mapping=aes(label=poll),colour="black")
q <- q + geom_text(d[Name %in% c("UTJ (Litzman)",
                                 "Hosen (Gantz)")],mapping=aes(label=poll),colour="white")
q <- q + scale_x_continuous("Politisk posisjon",lim=c(-10,10),breaks=NULL)
q <- q + scale_y_continuous("Mandater", breaks=c(4,30,seq(0,25,5),35,40), labels=c("Sperregrense","Minimum for regjeringsdannelse",seq(0,25,5),35,40), minor_breaks=NULL,lim=c(0,41),expand=c(0,0))
q <- q + scale_size(range = c(1, 30))
q <- q + scale_color_manual("",
                            values=c(
                              "brown"="#b15928",
                              "pink"="#fb9a99",
                              "red"="#e31a1c",
                              "red"="#e31a1c",
                              "orange"="#ff7f00",
                              "grey"="#6a3d9a",
                              "green"="#33a02c",
                              "purple"="#bc80bd",
                              "light blue"="#a6cee3",
                              "green"="#33a02c",
                              "navy"="#1f78b4",
                              "navy"="#1f78b4",
                              "light yellow"="#999999",
                              "dark yellow"="black",
                              "light green"="green"
                            ))
q <- q + guides(colour=FALSE, size=FALSE)
q
RAWmisc::saveA4(q,file.path(org::PROJ$SHARED_TODAY,sprintf("graph_%s.png",lubridate::today())),landscape = F)


labs <- d[,.(
  labs=paste0(Name,collapse=", ")
),by=xlevel]

q <- ggplot(d,aes(x=xlevel,y=poll,fill=coloring,label=Name))
q <- q + annotate("rect",xmin=-Inf,xmax=Inf,ymin=0,ymax=4,fill="black",alpha=0.4)
#q <- q + annotate("text", x = -0.1, y = 4.35, label = "sperregrense",hjust=1)
q <- q + geom_vline(xintercept=0)
q <- q + geom_hline(yintercept=0)
q <- q + geom_hline(yintercept=30)
q <- q + geom_col()
#q <- q + ggrepel::geom_label_repel(alpha=0.9, nudge_y=1)
#q <- q + geom_text(mapping=aes(label=poll),colour="black")
#q <- q + geom_text(d[Name %in% c("UTJ (Litzman)",
#                                 "Hosen (Gantz)")],mapping=aes(label=poll),colour="white")
q <- q + scale_x_continuous("Politisk posisjon",lim=c(-10,10),breaks=labs$xlevel,labels=labs$labs, minor_breaks = NULL)
q <- q + scale_y_continuous("Mandater", breaks=c(4,30,seq(0,25,5),35), labels=c("Sperregrense","Minimum for regjeringsdannelse",seq(0,25,5),35), minor_breaks=NULL,lim=c(0,38),expand=c(0,0))
q <- q + scale_size(range = c(1, 30))
q <- q + scale_fill_manual("",
                            values=c(
                              "brown"="#b15928",
                              "pink"="#fb9a99",
                              "red"="#e31a1c",
                              "red"="#e31a1c",
                              "orange"="#ff7f00",
                              "grey"="#6a3d9a",
                              "green"="#33a02c",
                              "purple"="#bc80bd",
                              "light blue"="#a6cee3",
                              "green"="#33a02c",
                              "navy"="#1f78b4",
                              "navy"="#1f78b4",
                              "light yellow"="#999999",
                              "dark yellow"="black"
                            ))
q <- q + guides(fill=FALSE, size=FALSE)
q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))
q
RAWmisc::saveA4(q,file.path(org::PROJ$SHARED_TODAY,sprintf("bargraph_%s.png",lubridate::today())),landscape = F)


