RAWmisc::InitialiseProject(
  HOME = "/git/code_minor/hanna_paper_3/",
  RAW = "/analyses/data_raw/hanna_paper_3/",
  CLEAN = "/analyses/data_clean/hanna_paper_3",
  BAKED = "/analyses/results_baked/hanna_paper_3/",
  FINAL = "/analyses/results_final/hanna_paper_3/",
  SHARED = "/dropbox/clients/hanna/paper_3/richard/")

# HANNA RUN FROM HERE
library(data.table)
library(ggplot2)

input <- list()

# HANNA EDIT THESE
input[[1]] <- c("sin366"=1, "cos366"=1, "sin183"=1, "cos183"=1, "IF"="INF123")
input[[2]] <- c("sin366"=0.5, "cos366"=0.5, "sin183"=1, "cos183"=1, "IF"="INFd23")

# HANNA STOP EDITING

results <- vector("list",length(input))
for(i in 1:length(results)){
  vals <- input[[i]]
  res <- as.numeric(vals["sin366"])*sin(c(1:366)/366*2*pi) + 
    as.numeric(vals["cos366"])*cos(c(1:366)/366*2*pi) + 
    as.numeric(vals["sin183"])*sin(c(1:366)/183*2*pi) + 
    as.numeric(vals["cos183"])*cos(c(1:366)/183*2*pi)
  res <- data.frame("IF"=vals["IF"],res,"day"=c(1:366))
  results[[i]] <- res
}

results <- rbindlist(results)

q <- ggplot(results, aes(x=day, y=res, colour=IF))
q <- q + geom_line()
q <- q + scale_x_continuous("Day")
q <- q + scale_y_continuous("Effect on IF Marker")
q <- q + scale_color_brewer("",palette="Set1")
q


## EXPORT TO EXCEL
openxlsx::write.xlsx(results, file=file.path("/dropbox/clients/hanna/paper_3/sample_scripts/sample_results/IFs.xlsx"))

## INDIVIDUAL RESULTS
print(results[IF=="INF123"], nrows=1000)