# Load makeCacheMatrix and CacheSolve below and use as follows: 
  # x<-matrix(rnorm(9),3,3)
  # v<-makeCacheMatrix(x)
  # v$get()
  # cacheSolve(v)
  # cacheSolve(v)       #second cachSolve gives cached data

makeCacheMatrix <- function(x = matrix()) {
       inv <- NULL
       set <- function(y) {
             x <<- y
             inv <<- NULL
}
       get <- function() x
       setinv <- function(inverse) inv <<- inverse
       getinv <- function() inv
       list(set = set, get = get,
                       setinv = setinv,
                       getinv = getinv)
}


cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
