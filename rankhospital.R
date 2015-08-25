rankhospital <- function(state,outcome, num = "best"){
  t1 <- data.frame()
  colName <- "Hospital.30.Day.Death..Mortality..Rates.from."
  care <-  read.csv("outcome-of-care-measures.csv", colClasses = "character")
  pos <- 0
  if(!(state %in% care$State)){
    stop("invalid state")
  }else{
    t1 <- care[care$State==state,]
  } 
  if (outcome == "heart attack"){
    colName <- paste0(colName,"Heart.Attack")
  }else if (outcome == "heart failure"){
    colName <- paste0(colName,"Heart.Failure")
  }else if (outcome == "pneumonia"){
    colName <- paste0(colName,"Pneumonia")
  }else{
    stop("invalid outcome")
  }
  nulls <-  t1[colName]=="Not Available"
  t1 <- t1[!nulls,]
  t1[,colName] <- as.numeric(t1[,colName])
  if (as.numeric(num)){
    pos <- as.numeric(num)
  }else if(num == "best"){
    pos <- 1
  }else if(num == "worst"){
    pos <- nrow(t1)
  }
  return(t1[order(t1[colName]),2][pos])
}