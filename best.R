best <- function(state,outcome){
  t1 <- data.frame()
  colName <- "Hospital.30.Day.Death..Mortality..Rates.from."
  care <-  read.csv("outcome-of-care-measures.csv", colClasses = "character")
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
  return(t1[order(t1[colName]),2][1])
}