best <- function(state,outcome){
  t1 <- data.frame()
  care <-  read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if(!(state %in% care$State)){
    stop("invalid state")
  }else{
    t1 <- care[care$State==state,]
  } 
  if (outcome == "heart attack"){
    nulls <-  t1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack=="Not Available"
    t1 <- t1[!nulls,]
    t1[,11] <- as.numeric(t1[,11])
    return(t1[order(t1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),2][1])
  }else if (outcome == "heart failure"){
    nulls <-  t1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure=="Not Available"
    t1 <- t1[!nulls,]
    t1[,17] <- as.numeric(t1[,17])
    return(t1[order(t1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),2][1])
  }else if (outcome == "pneumonia"){
    nulls <-  t1$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia=="Not Available"
    t1 <- t1[!nulls,]
    t1$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(t1$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    return(t1[order(t1$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),2][1])
  }else{
    stop("invalid outcome")
  }
}