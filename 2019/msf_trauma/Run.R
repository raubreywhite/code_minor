org::AllowFileManipulationFromInitialiseProject()
org::InitialiseProject(
  HOME = "/git/code_minor/2019/msf_trauma/",
  RAW = "/data/org/data_raw/code_minor/2019/msf_trauma/",
  SHARED = "/dropbox/analyses/results_shared/code_minor/2019/msf_trauma/"
)

library(data.table)
library(ggplot2)
library(pbmcapply)

stack <- expand.grid(
  num_people=seq(100,600,50),
  sim_id=1:2000
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
  power=RAWmisc::Format(mean(p<0.05))
),keyby=.(
  var,
  num_people
)]
res[,`N with 30% LTFU`:=round(num_people/0.7)]

res
res <- dcast.data.table(res,num_people+`N with 30% LTFU`~var,value.var = "power")
res[,`(Intercept)`:=NULL]
res[,`outcome_baseline`:=NULL]
setnames(res,"num_people","N with 0% LTFU")
openxlsx::write.xlsx(res, fs::path(org::PROJ$SHARED_TODAY,"power_calc.xlsx"))


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