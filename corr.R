## Miztiik
## 08Jun2013
## John Hopkins R Programming Week02 Assignment

## Final function of the quiz here - https://class.coursera.org/rprog-015/assignment/view?assignment_id=3
## To find correlation between the pollutants "Sulphate" & "nitrate" from monitors which meet the minimum threshold
## ( i,e., have enough observations greater than or equal to the threshold without any "NA")

corr <- function(directory="specdata", threshold = 0) {

  #Lets source the "complete.R" function to find the monitors with total of non na observations
  source("complete.R")

  #Set the directory path to our data folder
  dir <- paste("./input",directory,sep="/")

  #Find the monitor IDs which meet the threshold
  df <- complete()

  df <- subset(df,nobs>threshold)

  #setting the files names for 3 character filenames
  dir <- paste("./input",directory,sep="/")
  file.names <- sprintf("%03d.csv",df$id)
  file.names <- paste(dir,file.names,sep="/")

  sapply(file.names,
         function(fname) {
           tmp <- na.omit(read.csv(fname))
           cor(tmp$sulfate,tmp$nitrate)
         },USE.NAMES=FALSE
        )
}