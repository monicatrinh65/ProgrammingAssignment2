## A set of functions that cache the inverse of a matrix

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  ## set the matrix 
  set <- function(matrix) {
    m <<- matrix 
    i <<- NULL
  }
  
  ## get the value of the matrix 
  get <- function() m
  
  ## set the inverse 
  setinverse <- function(inverse) {
    i <<- inverse
  }
  
  ## get the inverse
  getinverse <- function() i 
  
  list(set=set, get=get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  
  ## if the inverse is already set, return the inverse
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## get the matrix 
  data <- x$get()
  
  ## calculate the inverse of the matrix
  i <- solve(data)
  
  ## set the inverse 
  x$setinverse(i)
  
  ## return the inversed matrix
  i
}
