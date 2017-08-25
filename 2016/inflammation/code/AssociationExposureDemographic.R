describeProp <- function (x, html = TRUE, digits = 1, number_first = TRUE, useNA = c("ifany", 
                                                                                     "no", "always"), useNA.digits = digits, default_ref, percentage_sign = TRUE, 
                          language = "en", ...) 
{
  dot_args <- list(...)
  #return("OK")
  if ("show_missing_digits" %in% names(dot_args)) {
    useNA.digits <- dot_args$show_missing_digits
    dot_args$show_missing_digits <- NULL
    warning("Deprecated: show_missing_digits argument is now useNA.digits as of ver. 1.0")
  }
  if ("show_missing" %in% names(dot_args)) {
    if (missing(useNA)) {
      useNA <- prConvertShowMissing(dot_args$show_missing)
    }
    dot_args$show_missing <- NULL
    warning("Deprecated: show_missing argument is now useNA as of ver. 1.0")
  }
  useNA <- match.arg(useNA)
  default_ref <- prDescGetAndValidateDefaultRef(x, default_ref)
  if ("horizontal_proportions" %in% names(dot_args) || (useNA == 
                                                        "ifany" && any(is.na(x))) || useNA == "always") {
    df_arg_list <- list(x = x, html = html, number_first = number_first, 
                        percentage_sign = percentage_sign, language = language, 
                        digits = digits, useNA = useNA, useNA.digits = useNA.digits)
    for (n in names(dot_args)) {
      if (!n %in% names(df_arg_list)) {
        df_arg_list[[n]] <- dot_args[[n]]
      }
    }
    return(fastDoCall(describeFactors, df_arg_list))
  }
  if (!is.factor(x)) 
    x <- factor(x)
  no <- sum(x == levels(x)[default_ref], na.rm = T)
  percent <- 100 * no/length(x[is.na(x) == FALSE])
  oi_args <- list(x = no, language = language, html = html)
  for (n in names(dot_args)) {
    if (!n %in% names(oi_args)) {
      oi_args[[n]] <- dot_args[[n]]
    }
  }
  no <- fastDoCall(txtInt, oi_args)
  if (percentage_sign == TRUE) 
    percentage_sign <- ifelse(html, "%", "\\%")
  else if (!is.character(percentage_sign)) 
    percentage_sign = ""
  if (number_first) 
    ret <- sprintf(sprintf("%%s (%%.%df%%s)", digits), no, 
                   percent, percentage_sign)
  else ret <- sprintf(sprintf("%%.%df%%s (%%s)", digits), percent, 
                      percentage_sign, no)
  
  return(ret)
}

describeFactors <- function (x, html = TRUE, digits = 1, number_first = TRUE, useNA = c("ifany", 
                                                                     "no", "always"), useNA.digits = digits, horizontal_proportions, 
          percentage_sign = TRUE, language = "en", ...) 
{
  dot_args <- list(...)
  #return("YES")
  if ("show_missing_digits" %in% names(dot_args)) {
    useNA.digits <- dot_args$show_missing_digits
    dot_args$show_missing_digits <- NULL
    warning("Deprecated: show_missing_digits argument is now useNA.digits as of ver. 1.0")
  }
  if ("show_missing" %in% names(dot_args)) {
    if (missing(useNA)) {
      useNA <- prConvertShowMissing(dot_args$show_missing)
    }
    dot_args$show_missing <- NULL
    warning("Deprecated: show_missing argument is now useNA as of ver. 1.0")
  }
  useNA <- match.arg(useNA)
  table_results <- table(x, useNA = useNA)
  if (!missing(horizontal_proportions)) {
    if (is.numeric(horizontal_proportions) == FALSE || (!inherits(horizontal_proportions, 
                                                                  "table") && is.vector(horizontal_proportions) == 
                                                        FALSE)) 
      stop("You have not provided a proper table/vector variable for the function,", 
           " the class you've given is: '", class(horizontal_proportions), 
           "'", ", instead of numeric vector or table")
    if (length(horizontal_proportions) < length(table_results)) 
      stop("There is a length discrepancy between the number of groups in the given sample", 
           " and the reference sample, ", length(horizontal_proportions), 
           " < ", length(table_results), "!")
    if (all(names(table_results) %in% names(horizontal_proportions)) == 
        FALSE) 
      stop("Your results contain results that are not in ", 
           "the reference horizontal_proportions: ", paste(names(table_results)[names(table_results) %in% 
                                                                                  names(horizontal_proportions)], collapse = ", "))
    if (length(horizontal_proportions) > length(table_results)) {
      tmp <- rep(0, times = length(horizontal_proportions))
      names(tmp) <- names(horizontal_proportions)
      for (n in names(tmp)) {
        if (n %in% names(table_results)) {
          tmp[n] <- table_results[n]
        }
      }
      table_results <- tmp
      class(tmp) <- "table"
    }
    percentages <- rep(NA, times = length(horizontal_proportions))
    names(percentages) <- names(horizontal_proportions)
    for (n in names(horizontal_proportions)) {
      if (is.na(n)) 
        percentages[is.na(names(percentages))] <- table_results[is.na(names(table_results))]/horizontal_proportions[is.na(names(horizontal_proportions))] * 
          100
      else percentages[n] <- table_results[n]/horizontal_proportions[n] * 
          100
    }
  }
  else {
    percentages <- table_results/sum(table_results) * 100
  }
  sa_args <- list(X = table_results, FUN = txtInt, language = language, 
                  html = html)
  for (n in names(dot_args)) {
    if (!n %in% names(sa_args)) {
      sa_args[[n]] <- dot_args[[n]]
    }
  }
  values <- fastDoCall(sapply, sa_args)
  if (percentage_sign == TRUE) 
    percentage_sign <- ifelse(html, "%", "\\%")
  else if (is.character(percentage_sign) == FALSE) 
    percentage_sign = ""
  if (number_first) 
    ret <- matrix(sprintf(ifelse(is.na(names(table_results)), 
                                 sprintf("%%s (%%.%df%%s)", useNA.digits), sprintf("%%s (%%.%df%%s)", 
                                                                                   digits)), values, percentages, percentage_sign), 
                  ncol = 1)
  else ret <- matrix(sprintf(ifelse(is.na(names(table_results)), 
                                    sprintf("%%.%df%%s (%%s)", useNA.digits), sprintf("%%.%df%%s (%%s)", 
                                                                                      digits)), percentages, percentage_sign, values), 
                     ncol = 1)
  rn <- names(table_results)
  rn[is.na(rn)] <- "Missing"
  rownames(ret) <- rn
 # ret <- "OK"
  return(ret)
}

AssociationExposureDemographics <- function(d,extraPG=NULL,extraPP=NULL){

  data <- d[["data"]]
  namesOutcome <- d[["namesOutcome"]]
  namesCovariates <- d[["namesCovariates"]]
  namesConIMPG <- d[["namesConIMPG"]]
  namesConIMPP <- d[["namesConIMPP"]]
  namesBinIMPG <- d[["namesBinIMPG"]]
  namesBinIMPP <- d[["namesBinIMPP"]]
  
  for(i in c("meanZPG","meanZPP")){
    if(i=="meanZPG"){
      model <- c(d$modelPG2,extraPG)
    } else if(i=="meanZPP"){
      model <- c(d$modelPP2,extraPP)
    }
    fit <- lm(as.formula(paste(i," ~ ",paste0(model,collapse="+"))), data=data)
    f <- Greg::printCrudeAndAdjustedModel(fit, 
                                          add_references=TRUE, 
                                          reference_zero_effect=1,
                                          ci_lim=c(-50,50),desc_column=F)
    
    x <- data[,c(i,model)]
    tab <- DescRes(x,f,lab=label(data[,i]))
    saveRDS(tab,paste0("results/tableSimpleExposure_",i,".RDS"))
  }
}





