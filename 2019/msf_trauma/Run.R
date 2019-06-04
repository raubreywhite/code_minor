org::AllowFileManipulationFromInitialiseProject()
org::InitialiseProject(
  HOME = "/git/code_minor/2019/msf_trauma/",
  RAW = "/data/org/data_raw/code_minor/2019/msf_trauma/",
  SHARED = "/box/Phase 2/results/"
)

library(data.table)
library(ggplot2)
library(pbmcapply)

stack <- expand.grid(
  num_people=seq(200,800,100),
  sim_id=1:1000
)

stack_list <- split(stack, seq(nrow(stack)))

res <- pbmclapply(stack_list,
                  function(x) sim(num_people=x$num_people),
                  mc.cores = parallel::detectCores()
)

res <- rbindlist(res, idcol=TRUE)
setnames(res,c("sim","est","se","t","p","var","num_people"))

res <- res[,.(
  est=round(mean(est),2),
  power=mean(p<0.05)
),keyby=.(
  var,
  num_people
)]

res
#xlsx::write.xlsx2(res, fs::path(org::PROJ$SHARED_TODAY,"afghanistan.xlsx"))


x <- sim_data(100)
x[1:10,outcome_end:=NA]

m <- mice(x)
m.out <- mice(x)

# complete
imp.data <- as.list(1:5)
for(i in 1:5){
  imp.data[[i]] <- complete(m.out, action=i)
}

# reshape
imp.data <- lapply(imp.data, melt, id=c("person_id","time_since_injury"))

# analyse
imp.fit <- lapply(imp.data, FUN=function(x){
  #print(names(x))
  kruskal.test(x$value,as.numeric(x$variable))
})
imp.res <- sapply(imp.fit, fixef)

reshap
with(m, kruskal.test(m))