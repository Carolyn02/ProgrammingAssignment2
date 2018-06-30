## The following functions make it possible to cache possible time-consuming computatuions. 
## The following functions are for caching the inverse of a matrix.

## The 'makeCacheMatrix'creates a list that contains a function to set the value of the matrix, 
## get the value for the matrix, set the value for the inverse of the matrix, 
## and get the value for the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  }
  


## The 'cacheSolve' function computes the inverse of the matrix. 
## It first checks to see if the inverse has has already been computed. 
## If it has been, the 'cacheSolve' retrieves the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.NULL(inv)){
    message("Getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <-  solve(data, ...)
  
  x$setinverse(inv)
  inv
}
