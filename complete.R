## A function to return completely observed sets(not sure what exactly that means)
## My presumptions is to return all observations without any "na"
## https://class.coursera.org/rprog-015/assignment/view?assignment_id=3

complete <- function(directory="specdata", id = 1:332) {

dir <- paste("./input",directory,sep="/")
#setting the files names for 3 character filenames
file.names <- sprintf("%03d.csv",id)
file.names <- paste(dir,file.names,sep="/")

rowNames <- length(id)

#Lets create a dataframe with the 3 columns as requested in the quizz
## Col 1 - the Monitor ID
## Col 2 - No. of observations without any "NA" - I am using a combination of sapply and "na.omit" instead of usuall "complete.cases" - found this simpler to understand
## Col 3 - Not exactly a column but row names - quick hack to have an row index from the length of id's
df <- data.frame(
                 id = id,
        nobs =sapply(file.names, function(fname) nrow(na.omit(read.csv(fname))) ),
        row.names = 1:rowNames

         )
df
}