# Load makeCacheMatrix and CacheSolve below and use as follows: 
  # x<-matrix(rnorm(9),3,3)
  # v<-makeCacheMatrix(x)
  # v$get()
  # cacheSolve(v)
  # cacheSolve(v)       #second cachSolve gives cached data


# What the function does
# 1 Set the value of the matrix
# 2 Get the value of the matrix
# 3 Set the value of the inverse
# 4 Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
# Define the inverse
       inv <- NULL
# Define inverse as function of y, which sets the inverse equal to cached matrix x
       set <- function(y) {
             x <<- y
             inv <<- NULL
}
# Defines get value step
       get <- function() x
# Calculates inverse in cache  
       setinv <- function(inverse) inv <<- inverse
# Gets inverse from Cache    
       getinv <- function() inv
 # Returning values for matrix inverse 
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
