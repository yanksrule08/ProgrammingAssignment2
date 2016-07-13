## These functions calculate the inverse of a matrix and cache the result, 
## and if the inverse of a matrix that has been calculated and cached before is 
## calculated again, the function returns the cached inverse instead of recalaculating.

## The function makeCacheMatrix calculates the inverse of a matrix and caches it.

makeCacheMatrix <- function(x = numeric()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## The funcion cacheSolve first serches for a cached inverse of a matrix and return the 
## cached inverse, and if none exists, the inverse is calculated.

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
