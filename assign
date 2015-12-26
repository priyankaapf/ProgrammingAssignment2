# Function 1

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
  x <<- y
  inverse <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inverse <<- inverse
  getinverse <- function() inverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  }

## Function 2

cacheSolve <- function(x = matrix, ...) {

  inv <- x$getinverse()
  if (!is.null(inv)){
  message("Getting cached inverted matrix")
  return(inv)
  }

  data <- x$get()     
  if (!is.matrix(data)){
  return("Data is not a matrix!")
  }
  
  inv <- solve(data,...)   
 x$setinverse(inv)  
  inv   
 }
##############
