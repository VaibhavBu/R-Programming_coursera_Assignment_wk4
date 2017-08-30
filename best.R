## This function returns the Hospital with minimum death , sorthed alphabetically 
setwd("C:/Users/Vaibhav/Desktop/Vaibhav_Test/R/Input_Files/rprog%2Fdata%2FProgAssignment3-data")

best <- function(state, outcome) {
  ## Read outcome data: COLS: HospitalName, State, HeartAttack, HearFailure, Pneumonia
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")[,c(2,7,11,17,23)]
  
  ## Check that state and outcome are valid  
  if(! ( state %in% levels(factor(data$State)) ) ) {
    stop("invalid state")
  }
  
  if(! (outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia") ) {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death rate
  
  
  # Remove row by state 
  data = data[data$State==state,]
  # Remove additional column State
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
  
  # Order by Minimum Deaths and then HospitalName
  data = data[order(data$Deaths, data$Hospital.Name),]
  
  # Return Best Hospital
  return (data$Hospital.Name[1])
}