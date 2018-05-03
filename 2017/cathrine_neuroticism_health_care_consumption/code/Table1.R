Table1 <- function(){
  res <- list()
  for(i in c(
    OUTCOMES_BINARY,
    OUTCOMES_CONTINUOUS,
    EXPOSURE,
    CONFOUNDERS_BINARY,
    CONFOUNDERS_CATEGORY
  )){
    res[[i]] <- RAWmisc::SummarizeDispatch(d[[i]],labelLeft=i)
  }
  
  res <- rbindlist(res)
  
  openxlsx::write.xlsx(res, file=file.path(
    RAWmisc::PROJ$SHARED_TODAY,"tables","table_1.xlsx"
  ))
  
}