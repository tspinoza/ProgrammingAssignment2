## The functions below are used to find the inverse of a matrix x
## As high compute resources required to caclulate inverse of a matrix
## the functions work together to store inverse of matrix and return inverse
## for inverse matrix already computed from cache
## makeCacheMatrix needs to be called first; assumes that matrix supplied is always invertible.

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## initialise a variable 'n' used to save inverse matrix as cache
  n <- NULL
  ## function to set new value to x and clear cache, for changes in x
  set  <- function(y) {
    x <<- y
    n <<- NULL
  }
  ## function get to obtain matrix x for inverse calculation
  get <- function() x
  ## function setInverse to assign inverse matrix of x to n
  setInverse <-function(inverse) n <<- inverse
  ## function getInverse to obtain cached inverse matrix of x
  getInverse <- function() n
  ## returns list of functions
  list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special matrix returned by the makeCacheMatrix function.
## If the inverse is already calculate, stored in cache, and the matrix has not changed then
## the cache solve function retrieves the inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## checks for cache value for inverse of n, if present, returns n
  n <- x$getInverse()
  if (!is.null(n)){
    message("getting cached data")
    return(n)
  }
  ## if no cached value for inverse of x, solves for inverse of x, stores as n in cache
  data <- x$get()
  n <- solve(data, ...)
  x$setInverse(n)
  return(n)
}
