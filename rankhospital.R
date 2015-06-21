##21Jun2015
## Function - rankhosptial :: A function to rank the hospitals based on the given "health condition".
## takes three arguments -
### 2-character abbreviated name of a state (state),
### an outcome (outcome),
### and the ranking of a hospital in that state for that outcome (num)

### returns a character vector containing the name(s) of the hospital for the given "outcome" and "ranking" in the given state

rankhospital <- function(state,outcome,num="best"){

  df <- read.csv("./input/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv",colClasses="character")

  #Creating a vector of valid states from the hospital data
  validStates <- data.frame(table(df$State))
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
  careData <- df[df$State == state,]

  #Expanding the column name to find the min mortality
  healthCondition <- NULL
  if (outcome == "heart attack") {
    healthCondition <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if (outcome == "heart failure") {
    healthCondition <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else {
    healthCondition <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  }

  #Make a dataframe with only the required columns "health condition and hospital name"
  rankedHosp <- careData[,c(healthCondition,"Hospital.Name")]

  #Remove the hospitals having no data
  rankedHosp <- rankedHosp[!rankedHosp[,healthCondition]=="Not Available",]

  #For ordering the data - the health condiition(as number) and then the corresponding hospital names as characters
  rankedHosp <- rankedHosp[ order( as.numeric(rankedHosp[,healthCondition]) , as.character(rankedHosp[,"Hospital.Name"]) ),]

  #Returning the ordered list of hospitals based on the "num"er requested in the function call
  if (num == "best") {
    return(head(as.character(rankedHosp[,"Hospital.Name"]), n = 1))
  } else if (num == "worst") {
    return(tail(as.character(rankedHosp[,"Hospital.Name"]), n = 1))
  } else if ( num > length(table(careData$Hospital.Name)) ){
    return(NA)
  }
  return(as.character(rankedHosp[,"Hospital.Name"][num]))

}