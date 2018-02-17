## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
	      x <<- y
      m <<- NULL
        }
    get <- function() x
    setinverse <- function(mean) m <<- mean
      getinverse <- function() m
      list(set = set, get = get,
	          setinverse = setinverse,
		  getinverse = getinverse)
}

# If the matrix has already been solved used the cached value
# otherwise solve the matrix. In both cases the solved matrix
# is returned to the caller
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
