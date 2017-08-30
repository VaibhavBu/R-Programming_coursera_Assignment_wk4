## Function to give Rankwise hospital in states , Remove hospitals those are not having corresponding rank for hospital

setwd("C:/Users/Vaibhav/Desktop/Vaibhav_Test/R/Input_Files/rprog%2Fdata%2FProgAssignment3-data")


rankall <- function(outcome, num = "best") {
  ## Read columns correspondingto outcome values HospitalName, State, HeartAttack, HearFailure, Pneumonia
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")[,c(2,7,11,17,23)]
  
  ## Check that state and outcome are valid  
  if(! (outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia") ) {
    stop("invalid outcome")
  }
  
  ## Check only best or worst passed for num
  if(class(num) == "character"){
    if (! (num == "best" || num == "worst")){
      stop("invalid number")
    }
  }
  
  #### Function to give Rankwise hospital in states 
  
# Create dataset corresponding to outcome,  HospitalName and Deaths by outcome
  if(outcome == "heart attack") {
    data = data[,c(1,2,3)]
  } else if(outcome == "heart failure") {
    data = data[,c(1,2,4)]
  } else if(outcome == "pneumonia") {
    data = data[,c(1,2,5)]
  }
  # Rename Variable name Deaths
  names(data)[3] = "Deaths"
  # Change class of Deaths to Numeric for further calculations
  data[, 3] = suppressWarnings( as.numeric(data[, 3]) )
  
  # Now remove all  NA value rows
  data = data[!is.na(data$Deaths),]
  data = data[order("State", "Deaths", "Hospital.Name"),]
  
  ## Split groups based on state
  state_death = split(data, data$State)
  
  
  rankHsptl <- function(x, num) {
    if (num=="best") {
      head(x, 1)
    } else if (num=="worst") {
      tail(x, 1)
    } else {
      x[num]
    }
  }
  
  result <- lapply(state_death, rankHsptl, num)
  result <- data.frame(result)
  result <- result[,c(1,2)]
  data.frame(hospital = unlist(result), state = names(result), row.names = names(result))
}
 