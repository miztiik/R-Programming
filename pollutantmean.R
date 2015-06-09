## Miztiik
## 08Jun2013
## John Hopkins R Programming Week02 Assignment
## "pollutantmean.R" a function to find the mean for either suplphate or nitrate particulate across all monitors

pollutantmean <- function(directory="specdata", pollutant, id = 1:332) {

  dir <- paste("./input",directory,sep="/")
  #setting the files names for 3 character filenames
  file.names <- sprintf("%03d.csv",id)
  file.names <- paste(dir,file.names,sep="/")

  #create a dummy output file
  out.file <- ""
  #recursive read into a single file
  out.file <- do.call("rbind", lapply(file.names, read.csv, header = T))

  write.table(out.file, file = "allData.csv",sep=",",row.names=F)

  df <- read.csv("allData.csv")
  #Calculate the mean for the given pollutant
  pollutantMean <- mean(df[,pollutant],na.rm=T)

  #print(pollutantMean)

}