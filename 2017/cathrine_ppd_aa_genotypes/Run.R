# Adds in Richard's own custom repository (allows for you to install 'RAWmisc')
if(!"https://raubreywhite.github.io/drat/" %in% options("repos")$repos){
  options("repos"=c(options("repos")$repos, raubreywhite = "https://raubreywhite.github.io/drat/"))
}

# Load important packages
library(data.table)
library(ggplot2)
library(SKAT)

############################################################
##### SETTING UP VARIABLES THAT WILL BE USED LATER ON IN CLEANING/ANALYSIS

SNPs <- list()
SNPs[["OXT"]] <- c(
  "rs2740210_CA",
  "rs4813625_CA",
  "rs4813627_CA",
  "rs7632287_CA"
)

SNPs[["OXTR"]] <- c(
  "rs1042778_CA",
  "rs2254298_CA",
  "rs2268490_CA",
  "rs2268491_CA",
  "rs237885_CA",
  "rs235887_CA",
  "rs237902_CA",
  "rs4686302_CA",
  "rs53576_CA"
)

# These are covariates
CONFOUNDERS <- c(
  "v32_Alder_CA",
  "v17_fodelseort_R",
  "v17_utbildning_R",
  "v17_arbetardu_R"
)

OUTCOMES <- c(
  "ASQSF_total_anxiety",
  "ASQSF_total_avoidance",
  "v17_EPDS_9R",
  "v32_EPDS_9R",
  "ppv6_EPDS_9R",
  "ppm6_EPDS_9R"
)

SENSITIVITY <- c(
  "v17_dep_anamnes"
)

############################################################
##### CLEANING THE DATA FOR THE NORMAL ANALYSES (i.e. ANALYSES 1 AND 2)
## READ IN DATA
## CATHRINE YOU WILL NEED TO CHANGE THIS
d <- data.table(haven::read_spss("/analyses/data_raw/code_minor/2017/cathrine_ppd_aa_genotypes/pek123_171005.sav"))
nrow(d)
d <- d[filter_OXT_OXTR_CA==1]
nrow(d)

outcomeData <- d[,OUTCOMES,with=F]
outcomeData[,id:=1:.N]
outcomeData <- melt.data.table(outcomeData,id="id")
q <- ggplot(outcomeData, aes(x=log(1+value)))
q <- q + geom_density()
q <- q + facet_wrap(~variable,scales="free")
q <- q + scale_x_continuous("log(1+value)")
ggsave("/dropbox/clients/cathrine/ppd_aa_genotypes/results_richard/histogram.png",
       plot = q, width = 297, height = 210, units = "mm")

# HEATMAP
q <- GGally::ggcorr(cbind(
  d[,SNPs[["OXTR"]],with=F],
  d[,SNPs[["OXT"]],with=F]), 
  hjust = 0.85, label = TRUE) + expand_limits(x=-2)
ggsave("/dropbox/clients/cathrine/ppd_aa_genotypes/results_richard/heatmap.png",
       plot = q, width = 297/1.25, height = 210, units = "mm")

# END HEATMAP


xtabs(~d$ASQSF_total_anxiety)
hist(d$ASQSF_total_anxiety)
xtabs(~d$rs237902_CA)

retval <- list(100)
index <- 1
for(y in OUTCOMES) for(z in names(SNPs)) for(strata in c("All","Depressed","Not-depressed")){
  y.c <- log(1+d[[y]])
  X <- as.matrix(d[,CONFOUNDERS,with=F])
  Z <- as.matrix(d[,SNPs[[z]],with=F])
  
  if(strata=="Depressed"){
    y.c <- y.c[d[[SENSITIVITY]]==1]
    X <- X[d[[SENSITIVITY]]==1,]
    Z <- Z[d[[SENSITIVITY]]==1,]
  } else if(strata=="Not-depressed"){
    y.c <- y.c[d[[SENSITIVITY]]==0]
    X <- X[d[[SENSITIVITY]]==0,]
    Z <- Z[d[[SENSITIVITY]]==0,]
  }
  
  objCrude <-SKAT_Null_Model(y.c ~ 1, out_type="C")
  objAdjusted <-SKAT_Null_Model(y.c ~ X, out_type="C")
  retval[[index]] <- data.frame(
    strata=strata,
    crude_pval_weightedrare=SKAT(Z, objCrude)$p.value,
    crude_pval_commonrare=SKAT_CommonRare(Z, objCrude)$p.value,
    adjusted_pval_weightedrare=SKAT(Z, objAdjusted)$p.value,
    adjusted_pval_commonrare=SKAT_CommonRare(Z, objAdjusted)$p.value,
    logged_outcome=y,
    gene=z)
  index <- index + 1
}
retval <- rbindlist(retval)
retval
openxlsx::write.xlsx(retval, file="/dropbox/clients/cathrine/ppd_aa_genotypes/results_richard/SKAT.xlsx", rowNames=TRUE, colNames=TRUE)



