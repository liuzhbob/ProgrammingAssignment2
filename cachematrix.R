## A pair of functions that cache the inverse of a matrix

## This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setMatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getMatrix <- function() x
  cacheInverse <- function(cache) {
    m <<- cache
  }
  getInverse <- function() m
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


## This function computes the inverse of the special matrix returned by the above. If the inverse already
## calculated and the matrix has not changed, then the cachesolve should retriedve the inverse from
## the cache.
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getMatrix()
  m <- cache(data)
  x$cacheInverse(m)
  inverse
}
