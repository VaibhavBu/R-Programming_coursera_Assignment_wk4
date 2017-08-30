## Finding the  best hospital in State based on Rank, Return NA when passed number for num is more than hospitals in state

setwd("C:/Users/Vaibhav/Desktop/Vaibhav_Test/R/Input_Files/rprog%2Fdata%2FProgAssignment3-data")

rankhospital <- function(state, outcome, num = "best") {
  ## Read columns correspondingto outcome values HospitalName, State, HeartAttack, HearFailure, Pneumonia
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")[,c(2,7,11,17,23)]
  
  ## Check that state and outcome are valid
  if(! ( state %in% levels(factor(data$State)) ) ) {
    stop("invalid state")
  }
  
  if(! (outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia") ) {
    stop("invalid outcome")
  }
  
  ## Only accept best or worst for num as ip value of rank
  if(class(num) == "character"){
    if (! (num == "best" || num == "worst")){
      stop("invalid number")
    }
  }
  
  ####  Return hospital name in that state with the given rank 30-day death rate
  
  # filter row for state 
  data = data[data$State==state,]
  # remove additional State column 
  data = data[,c(1,3,4,5)]
  
  # Create dataset corresponding to outcome,  HospitalName and Deaths by outcome
  if(outcome == "heart attack") {
    data = data[,c(1,2)]
  } else if(outcome == "heart failure") {
    data = data[,c(1,3)]
  } else if(outcome == "pneumonia") {
    data = data[,c(1,4)]
  }
    # Rename Variable name Deaths
  names(data)[2] = "Deaths"
   # Change class of Deaths to Numeric for further calculations
  data[, 2] = suppressWarnings( as.numeric(data[, 2]) )
  
 # Now remove all  NA value rows
  data = data[!is.na(data$Deaths),]
  
  # if passed value for num > numrows(hospitals in state) then return NA
  if(class(num) == "numeric" && num > nrow(data)){
    return (NA)
  }
  
  # Order by Deaths and then HospitalName
  data = data[order(data$Deaths, data$Hospital.Name),]
  
  # Return Op value based on ip rank for num
  if(class(num) == "character") {
    if(num == "best") {
      return (data$Hospital.Name[1])
    }
    else if(num == "worst") {
      return (data$Hospital.Name[nrow(data)])
    }
  }
  else {
    return (data$Hospital.Name[num])
  }
}