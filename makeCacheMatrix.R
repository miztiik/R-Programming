## Function to cache mean when the contents of a vector are not changing.
## Part of R Programming excercise
## https://class.coursera.org/rprog-015/human_grading/view/courses/973496/assessments/3/submissions

## The first function, makeVector creates a special "vector", which is really a list containing a function to
# set the value of the vector
# get the value of the vector
# set the value of the mean
# get the value of the mean

##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  #Setup an null inverse matrix
  inv <- NULL

  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setInverse=setinverse, getInverse=getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function( x , ... ){

  # Getting the inverse of the matrix
  inv <- x$getInverse()

  #check if it has value already then return cached inverse
  if(!is.null(inv)) {
    message("We have cached data, fetching it...")
    return(inv)
  }

  #calculating inverse when it is not already cached using the template functions built by "makeCacheMatrix"
  data <- x$get()
  invData <- solve(data , ...)
  # Cache it for future re-use
  x$setInverse(invData)
  # return the inversed Matrix
  return(invData)
}