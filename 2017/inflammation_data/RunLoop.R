RAWmisc::InitialiseProject(
  HOME = "/analyses/code_minor/hanna_paper_3/",
  RAW = "/analyses/data_raw/hanna_paper_3/",
  CLEAN = "/analyses/data_clean/hanna_paper_3",
  BAKED = "/analyses/results_baked/hanna_paper_3/",
  FINAL = "/analyses/results_final/hanna_paper_3/",
  SHARED = "/dropbox/results_shared/hanna_paper_3/")

assign("RUN_ALL", TRUE, envir=globalenv())


#### HANNA RUN FROM HERE
# you will need the following libraries installed:
# data.table
# openxlsx

suppressWarnings(suppressMessages(library(data.table)))

# creating fake dataset
set.seed(43)
n = 50
p = 10
sigma = 1

x = matrix(rnorm(n*p),n,p)

beta = c(3,2,rep(0,p-2))
y = x%*%beta + sigma*rnorm(n)
dataset <- data.frame(y,x)

# look at data
print(dataset)

# create a list to save my results in
results <- list()

# loop through
for(i in c("X1","X2","X3","X4","X5","X6","X7")){
  # creating the formula
  formula <- sprintf("y ~ %s", i)
  
  # look at the formula
  print(formula)
  
  # fitting a linear regression
  fit <- lm(as.formula(formula), data=dataset)
  
  # look at the linear regression
  print(fit)
  print(summary(fit))
  
  # rsquared
  # look at what is available for us to save from "summary(fit)"
  names(summary(fit))
  # getting rsquared
  r2 <- summary(fit)$r.squared
  
  # p-value for model (which is the same as the single variable, since the model only has one variable)
  # look at what is available for us to save from "anova(fit)"
  names(anova(fit))
  # getting p-value for model
  pval <- anova(fit)$`Pr(>F)`[1]
  
  # save results
  singleResults <- data.frame(variable=i, r2=r2, pval=pval)
  results[[i]] <- singleResults
}

results <- rbindlist(results)
results <- as.data.frame(results)
print(results)

# save as excel file
openxlsx::write.xlsx(results, file=file.path(RPROJ$SHARED,"results.xlsx")) #HANNA CHANGE FILE LOCATION

