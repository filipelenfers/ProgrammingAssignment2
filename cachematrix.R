## Put comments here that give an overall description of what your
## functions do

## Function to create a object with a matrix and the capcity to cache the inverse of this matrix. 
## The matrix passed as parameter should be square.
## Methods on the created object:
## - get(): return the base matrix
## - set(): change the base matrix
## - getInverse(): return the inverse of matrix
## - setInverse(): change/define the inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
