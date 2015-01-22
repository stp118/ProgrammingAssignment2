## The following two functions work together to store the inverse
## of a matrix in a cache for later use (instead of computing the
## inverse everytime it is needed.)

## This function provides access to a matrix x and its inverse I.
## Access is established by four sub-functions set, get, setI and getI,
## which are returned as a list.
makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setI <- function(inv) I <<- inv
  getI <- function() I
  list(set = set, get = get, setI = setI, getI = getI)
}

## This function reads the inverse I of a given matrix from a cache, if
## the inverse was stored there and if the matrix has not been changed.
## Otherwise it computes the inverse.
## In either case the inverse is returned.
cacheSolve <- function(x, ...) {
  I <- x$getI()
  if(!is.null(I)) {
    message("Loading inverse from cache...")
    return(I)
  }
  tmp <- x$get()
  message("Calculating inverse...")
  inv <- solve(tmp, diag(dim(tmp)[1]), ...)
  x$setI(inv)
  inv
}
