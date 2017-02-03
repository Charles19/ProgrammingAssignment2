# The first function creates a special "Matrix" attached to its inverse.
# The second function calculates the inverse if this one wasn't already in the cache



# This function returns a list containing the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inverse <<- inv
  getinv <- function() inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# This function returns the inverse. If this one is already calculated,
# then it is just returned. Otherwise, or if the matrix is different, then the 
# matrix is re-calculated.

cacheSolve <- function(x, ...) {
  inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinv(inverse)
  inverse
}

