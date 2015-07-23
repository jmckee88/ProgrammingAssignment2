##Following are two functions, "makeCacheMatrix" and "cacheSolve", which store a matrix and cache its inverse.


## function makeCacheMatrix creates a special vector which consists of four functions: set, get, setsolve, getsolve.
## "set" sets the value of the matrix, "get" gets the value of the matrix, "setsolve" sets the value of the inverse
## of the matrix, and "getsolve" gets the value of the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Function "cacheSolve" which calculates the inverse of the matrix. However, first it checks to see if the inverse
## has already been calculated and cached, and if so, retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}

