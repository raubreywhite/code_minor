CleanData <- function(){
  d <- haven::read_sav(file.path(org::PROJ$RAW, "PCOS working file to Richard.sav"))
  setDT(d)
  
  
  nrow(d)
  
  # table 1
  xtabs(~d$group)
  
  # PCOS
  # matched on age, residential area.
  setorder(d,lopnr_fall)
  
  d[,c_time_to_first_pregnancy:=Time_to_first_pregnancy]
  #dF2[is.na(Time_to_first_pregnancy),c_time_to_first_pregnancy:=2018-born-18]
  d[First_pregnancy_all==0 & !is.na(Time_to_first_pregnancy)]
  
  xtabs(~d$First_spontaneous_pregnancy+d$First_pregnancy_all,addNA=T)
  
  
  d[,birth_periods:=factor(birth_periods)]
  d[,Country_of_birth:=factor(Country_of_birth)]
  
  return(d)
}