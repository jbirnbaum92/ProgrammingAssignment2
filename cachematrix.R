## Matrix inversion is a costly computation an there is benefit to caching it rather than computing repeatedly.
## The First function creates an object that can cache its inverse.
## The second computes the inverse returned by the function above. If the inverse has been calculated already, it will simply be
## recovered from the cache.

## This function creates a special matrix object to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
  }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## This function computes the inverse or returns the inverse from the cache if already computed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
      message("getting cached data")
      return(inv)
  }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
