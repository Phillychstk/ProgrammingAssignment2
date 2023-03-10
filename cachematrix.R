## These will set up to find the inverse of the special matrix.

## setting invrs which is for range to null

makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  setinvrs <- function(inverse) invrs <<- invrs
  getinvrs <- function() invrs
  list(set = set,
       get = get,
       setinvrs = setinvrs,
       getinvrs = getinvrs)

}


## this function computes the inverse of the "matrix" if not already input by the previous function

cachemean <- function(x, ...) {
  invrs <- x$getinvrs()
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  dta <- x$get()
  invrs <- solve(dta, ...)
  x$setinvrs(invrs)
  invrs
}