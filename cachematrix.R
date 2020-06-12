## The following functions can compute and cache the inverse of a matrix.
## functions do

## this first function create a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## computes the inverse of the “matrix” returned by makeCacheMatrix(). If the 
## inverse has already been calculated and the matrix has not changed, 
##it’ll retrieves the inverse from the cache directly

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
      return(i)
  }
  data <- x$get()
  i = solve(data, ...)
  x$setInverse(i)
  return(i)
}
