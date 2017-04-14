## these two functions create a special matrix, then calculates the inverse
## of the special matrix, and caches the inverse.

## this function creates a special matrix and returns a list of functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y=matrix) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## this function returns the inverse of the special matrix
## it either calculates the inverse or returns the cached inverse
## if there is one.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
