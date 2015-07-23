## These functions allows to cache inverse matrices associated to the original matrix.
## Avoid calculating more than once the inverse of a matrix by storings its inverse once calculated. 

## makeCacheMatrix
## Creates and returns a list containig four functions to:
## 1. set the value of the matrix.
## 2. get the value of the matrix.
## 3. set the value of the inverse matrix.
## 4. get the value of the inverse matrix.
## Each time the function is called an environment containing the matrix and its inverse 
## is created.

makeCacheMatrix <- function(x = matrix()) {
  
    inv <- NULL

    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
  
    get <- function() x
    
    setInverse <- function(inverse)inv <<- inverse
    
    getInverse <- function() inv
    
    # Build list 
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## cacheSolve
## Get the inverse of a matrix. If the inverse if already in the environment created when calling 
## makeCacheMatrix returns it. Otherwise calculates it and caches it.

cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  
  ## Check if inverse is already cached
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Calculate and cache inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
