## 21Jun2015 - CoursEra - R Programming
## Function "rankall" - To rank all hospitals in the given dataset for the given health condition

### Take two arguments "health condition" and ranking "num"ber

rankall <- function(outcome,num = "best") {

  careData <- read.csv("./input/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv",colClasses="character")

  #Creating a vector of valid health conditions from care data
  validOutcomes <- c("heart attack","heart failure","pneumonia")

  #checking if the outcome are valid and stopping out if they are invalid
  if(!is.element(outcome,validOutcomes)){
    stop("invalid outcome")
  }

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
  rankedHosp <- careData[,c("State",healthCondition,"Hospital.Name")]

  #convert the outcome to number to make it easy for ordering
  rankedHosp[,healthCondition] <- suppressWarnings(as.numeric(rankedHosp[,healthCondition]))

  #Remove the hospitals having no data
  rankedHosp <- rankedHosp[!is.na(rankedHosp[healthCondition]),]

  #For ordering the data - First by state, then the health condiition(as number) and then the corresponding hospital names
  rankedHosp <- rankedHosp[ order( rankedHosp[,"State"], rankedHosp[,healthCondition], rankedHosp[,"Hospital.Name"]), ]

  # aggregate by state, choosing the row that corresponds to the given rank num
  rankedHospByState <- aggregate(rankedHosp, by=list(rankedHosp$State), function(x) {
    if (!is.numeric(num)) {
      if (num == "best") {
        num <- 1
      } else if (num == "worst") {
        num <- length(x)
      } else {
        stop("invalid num")
      }
    }
    x[num]
  })

  #ordering the columns as requested
  outDF <- rankedHospByState[,c(4,2)]

  #setting up the header row as recommended in the question
  ## Need to remove the condition column after ranking.
  names(outDF) <- c("hospital","state")


  return(outDF)
}