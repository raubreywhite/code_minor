Cluster <- function(data, numClusters, fileName){
  names(data) <- gsub("l_im_[0-9][0-9][0-9]_","",names(data))
  tree <- ClustOfVar::hclustvar(data)
  set.seed(4)
  stab <- ClustOfVar::stability(tree, B=50)$matCR[,1:20]
  stab <- apply(stab,2,mean)
  stab <- data.frame("stab"=stab,"clusters"=2:(length(stab)+1))
  
  
  q <- ggplot(stab,aes(x=clusters,y=stab,label=clusters))
  q <- q + geom_vline(xintercept=numClusters,col="red",size=2)
  q <- q + geom_line(lwd=3)
  q <- q + geom_point(size=10)
  q <- q + geom_text(aes(y=stab+0.005),size=10,vjust=0)
  q <- q + scale_x_continuous("Number of clusters")
  q <- q + scale_y_continuous("Mean adjusted Rand criterion (stability of clusters)\n")
  q <- SMAOFormatGGPlot(q)
  SMAOpng(paste0("results/",fileName,"_cluster_stab.png"),landscape=TRUE)
  print(q)
  dev.off()
  
  clusters <- ClustOfVar::cutreevar(tree,numClusters)$cluster
  clusters <- data.frame(clusters)
  clusters$label <- row.names(clusters)
  clusters$clusters <- as.factor(clusters$clusters)
  ggdendro::ggdendrogram(tree,rotate=TRUE)
  treeData <- ggdendro::dendro_data(tree, type="rectangle")
  
  labelData <- ggdendro::label(treeData)
  labelData$hjust <- abs(labelData$x-max(labelData$x)/2)
  labelData$hjust <- labelData$hjust/max(labelData$hjust)
  labelData$angle <- labelData$x-1#abs(labelData$x-max(labelData$x))+1
  labelData$angle <- labelData$angle/(max(labelData$angle)+1)*360#*180
  labelData$a <- 0
  labelData$a[labelData$angle>=0 & labelData$angle<=90] <- 90-labelData$angle[labelData$angle>=0 & labelData$angle<=90]
  labelData$a[labelData$angle>=90 & labelData$angle<=180] <- 90-labelData$angle[labelData$angle>=90 & labelData$angle<=180]
  labelData$a[labelData$angle>=180 & labelData$angle<=270] <- 90-labelData$angle[labelData$angle>=180 & labelData$angle<=270]-180
  labelData$a[labelData$angle>=270 & labelData$angl<=360] <- 270-labelData$angle[labelData$angle>=270 & labelData$angl<=360]
  
  labelData$h <- 0
  labelData$h[labelData$angle>=180 & labelData$angle<=360] <- 1
  labelData <- merge(labelData,clusters,by="label")
  
  
  q <- ggplot(ggdendro::segment(treeData))
  q <- q + geom_segment(aes(x=x, y=y, xend=xend, yend=yend))
  q <- q + geom_text(data=labelData, mapping=aes(x=x,y=y, label=gsub("_","-",gsub("l_im_","",label)), hjust=h, angle=a, colour=clusters))
  q <- q + coord_flip()
  q <- q + scale_y_reverse(expand=c(0.2,0))
  q <- q + scale_x_continuous(lim=c(1,max(labelData$x)+1))
  q <- q + ggdendro::theme_dendro()
  q <- q + coord_polar()
  q <- q + scale_colour_brewer("Cluster",palette="Set1")
  SMAOpng(paste0("results/",fileName,"_cluster.png"),landscape=TRUE)
  print(q)
  dev.off()
}

IMClusters <- function(d){
  data <- d[["data"]]
  namesLog2IMPG <- d[["namesLog2IMPG"]]
  namesLog2IMPP <- d[["namesLog2IMPP"]]
  
  ### principle components
  Cluster(data[data$hasLog2IMPG,namesLog2IMPG], 4, fileName="log2impg")
  Cluster(data[data$hasLog2IMPP,namesLog2IMPP], 2, fileName="log2impp")
}
  
  
  
