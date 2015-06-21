##18Jun2015
## R-Programming - Assignment 3

## Function "best" to find name of the hospital with the best(lowest) mortality rate in a given state (first argument as two characters) and for a given health condition(second argument)
### Hospitals that do not have data on a particular outcome should be excluded from the set of hospitals when deciding the rankings.


best <- function(state,outcome){
  care <- read.csv("./input/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv",colClasses="character")
  df <- care

  #Creating a histogram of 30-day death rates from heart attacks
  #df[, 11] <- as.numeric(df[, 11])
  #hist(df[, 11])

  hospData <- read.csv("./input/rprog-data-ProgAssignment3-data/hospital-data.csv",colClasses="character")

  #Creating a vector of valid states from the hospital data
  validStates <- data.frame(table(hospData$State))
  validStates <- validStates$Var1

  #Creating a vector of valid health conditions from care data
  validOutcomes <- c("heart attack","heart failure","pneumonia")

  #checking if the state and outcome are valid and stopping out if they are invalid
    if(!is.element(state,validStates)){
      stop("invalid state")
    }

if(!is.element(outcome,validOutcomes)){
  stop("invalid outcome")
}

#Finding the best(lowest mortality) hospital
## Subsetting only the hospital that match our given state
df2 <- df[df$State == state,]

#Expanding the column name to find the min mortality
healthCondition <- NULL
if (outcome == "heart attack") {
  healthCondition <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
} else if (outcome == "heart failure") {
  healthCondition <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
} else {
  healthCondition <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
}

#Again subsetting to a vector containing only the required health outcome and removing hospital entries not having data
df3 <- df2[,healthCondition]
df4 <- df3[!df3=="Not Available"]
df4 <- as.numeric(as.character(df4))

#Find the min and match it with the hospital name
#All mortality rates are of the format "xx.x" , When converting to numeric to find min the extranaeous "0" after the decimal is removed and any string matching will fail, so we use sprintf to retain the format

lowerMortality <- sprintf("%.1f", min(df4))

#Filtering using the minimum mortality rate and where the column name is same as our health condition
bestHospitals <- df2[df2[,healthCondition]== lowerMortality,]


#Sorting the hospital names so we can pick the first one in the alphabetic order
hospitalNames <- sort(bestHospitals[,"Hospital.Name"])

return(as.character(hospitalNames[1]))

}