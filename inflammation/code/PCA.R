if(FALSE){
  pcIMPG <- pcaMethods::pca(data[data$hasIMPG,namesIMPG], nPcs=2, method="ppca", scale="pareto")
  loadingsIMPG <- data.frame(loadings(pcIMPG))
  loadingsIMPG$im <- rownames(loadingsIMPG)
  loadingsIMPG$rank1 <- rank(-abs(loadingsIMPG$PC1))
  loadingsIMPG$rank2 <- rank(-abs(loadingsIMPG$PC2))
  loadingsIMPG <- loadingsIMPG[loadingsIMPG$rank1<5 | loadingsIMPG$rank2<5,]
  imputed <- pcaMethods::completeObs(pcIMPG)
  
  
  q <- ggplot(loadingsIMPG, aes(x=PC1, y=PC2, label=im))
  q <- q + geom_text()
  q <- q + scale_x_continuous(lim=c(-1,1))
  q <- q + scale_y_continuous(lim=c(-1,1))
  q
  
  
}

#pcIMPG <- pcaMethods::pca(data[data$hasIMPG,dataNamesIMPG], nPcs=3, method="ppca", scale="pareto")
#imputed <- pcaMethods::completeObs(pcIMPG)







