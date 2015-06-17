
## Caching the Inverse of a Matrix
## The following pair of functions cache the inverse of a matrix.
## 1. makeCacheMatrix: This function creates a special "matrix"
##    object that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special
##    "matrix" returned by makeCacheMatrix above. If the inverse
##    has already been calculated (and the matrix has not changed),
##    then the cachesolve should retrieve the inverse from the cac


#################################################################
## makeCacheMatrix
#################################################################
## The makeCacheMatrix function creates a
## special "matrix" object that can cache its inverse.
## The special “matrix” is really a list containing functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize value of inverse to NULL
  inv <- NULL

  ## function to set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }

  ## function to get the value of the matrix
  get <- function() {
    return(x)
  }

  ## function to set the value of the inverse
  setInverse <- function(inverse) {
    inv <<- inverse
  }

  ## function to get the value of the inverse
  getInverse <- function() {
    return(inv)
  }

  # The result is a list of the four functions.
  result <- list(set = set,
                 get = get,
                 setInverse = setInverse,
                 getInverse = getInverse)
  return(result)
}

#################################################################
## cacheSolve
#################################################################
## The cacheSolve function computes the inverse of
## the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated
## (and the matrix has not changed), then
## cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  data <- x$get()

  ## Check if the inverse has already been calculated
  ## (and the matrix has not changed), then
  ## retrieve the inverse from the cache.

  check <- !is.null(inv)
  ## If we assume that
  ## (i) only the makeCacheMatrix and cacheSolve
  ## functions are used to create and modify the matrix, and
  ## (ii) the x$setInverse function is accessed only within
  ## cacheSolve
  ## then:
  ## the only way to change the matrix is via the set
  ## function, which also sets the inverse to null. Therefore
  ## if the inverse is not null, it implies that there has been
  ## no change in the matrix.
  ## To put it another way, if the inverse is not null, it implies:
  ## (a) either makeCacheMatrix was called and then x$setInverse
  ## was called from cacheSolve,
  ## (b) or set was called and then x$setInverse
  ## was called from cacheSolve.
  ## Either case, we have an object with inv which is
  ## the true inverse of the matrix from x$get
  if(check) {
    message("getting cached data")
    return(inv)
  }

  ## The inverse has not been calculated. So lets do it.
  inv <- solve(data, ...)
  x$setInverse(inv)

  return(inv)
}
