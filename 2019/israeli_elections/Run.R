## TODO: test proportional hazards
# https://www.uio.no/studier/emner/matnat/math/STK4080/h16/undervisningsmateriell/lecture12_16.pdf

org::AllowFileManipulationFromInitialiseProject()
org::InitialiseProject(
  HOME = "/git/code_minor/2019/israeli_elections/",
  RAW = "/Volumes/crypt_data/org/data_raw/code_minor/2019/israeli_elections/",
  SHARED = "/dropbox/analyses/results_shared/code_minor/2019/israeli_elections/"
)

library(data.table)
library(ggplot2)

d <- readxl::read_excel(file.path(org::PROJ$RAW,"Background for R-1.xlsx"))
setDT(d)

q <- ggplot(d,aes(x=xlevel,y=poll,colour=coloring,label=Name))
q <- q + geom_vline(xintercept=0)
q <- q + geom_hline(yintercept=0)
q <- q + geom_point(mapping=aes(size=poll))
q <- q + geom_text(mapping=aes(label=poll),colour="black")
q <- q + geom_text(d[Name %in% c("UTJ (Litzman)",
                                 "Hosen (Gantz)")],mapping=aes(label=poll),colour="white")
q <- q + ggrepel::geom_label_repel(alpha=0.9)
q <- q + scale_x_continuous("Politisk posisjon",lim=c(-10,10),breaks=NULL)
q <- q  + scale_y_continuous("Mandater", breaks=seq(0,40,5), minor_breaks=NULL)
q <- q + expand_limits(y=c(0,32))
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
                              "dark yellow"="black"
                            ))
q <- q + guides(colour=FALSE, size=FALSE)
q
RAWmisc::saveA4(q,file.path(org::PROJ$SHARED_TODAY,"graph.png"),landscape = F)


q <- ggplot(d,aes(x=xlevel,y=poll,fill=coloring,label=Name))
q <- q + geom_vline(xintercept=0)
q <- q + geom_hline(yintercept=0)
q <- q + geom_col()
#q <- q + geom_point(mapping=aes(size=poll))
#q <- q + geom_text(mapping=aes(label=poll),colour="black")
#q <- q + geom_text(d[Name %in% c("UTJ (Litzman)",
#                                 "Hosen (Gantz)")],mapping=aes(label=poll),colour="white")
#q <- q + ggrepel::geom_label_repel(alpha=0.9)
q <- q + scale_x_continuous("Politisk posisjon",lim=c(-10,10),breaks=d$xlevel,labels=d$Name)
q <- q  + scale_y_continuous("Mandater", breaks=seq(0,40,5), minor_breaks=NULL)
q <- q + expand_limits(y=c(0,32))
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
q <- q + guides(colour=FALSE, size=FALSE)
q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1))
q



