## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##              If the inverse has already been calculated (and the matrix has not changed), 
##              then the cachesolve should retrieve the inverse from the cache.

## see above

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## see above

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

#example
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
aMatrix <- makeCacheMatrix(m1)
aMatrix$get()
aMatrix$getinverse()

cacheSolve(aMatrix)
aMatrix$getinverse()


