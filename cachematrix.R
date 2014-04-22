## Put comments here that give an overall description of what your
## functions do

## creates a special matrix 

makeCacheMatrix <- function(x = matrix()) {

    invrs <- NULL
    set <- function(y) {
      x <- y
      invrs <- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invrs <- inverse
    getinverse <- function() invrs
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
}


## checks if the mean is already present in the cache and return it or else
##calculates the mean.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invrs <- x$getinverse()
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  mat <- x$get()
  invrs <- solve(mat, ...)
  x$setinverse(invrs)
  invrs
  
}
