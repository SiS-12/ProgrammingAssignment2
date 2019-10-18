## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function



makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  ### the value given back needs to be "reset"
  set <- function(y) {
    x <<- y    #set function sets x on a free value, so the argument can't be missing 
    m <<- NULL # reset the value given back in the calling env.
  }
  get <- function() x   # to get the original matrix
  setSolve <- function(solve) {m <<- solve}   # set the calculation to m in the calling environment
  getSolve <- function() m   ##keeps the calculation
  list(set = set, get = get,  #returns a list with 4 functions
       setSolve = setSolve,
       getSolve = getSolve)
}

cacheSolve <- function(x,...) {
  m <- x$getSolve()            # get the output if already calculated
  if (!is.null(m)) {          # if it is calculated
    message("getting cached data")  ##return the message
    return(m)                       ## and give back the calculated --- stop processing further on
  }
  data <- x$get()             ##else (not yet calculated) get the original matrix
  m <- solve(data,...)       ## calculate
  x$setSolve(m)             ##set the output to the list  (initiated by the makeCacheMatrix-function)
  m                        ##return the output
}
