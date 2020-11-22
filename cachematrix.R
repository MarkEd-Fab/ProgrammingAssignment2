## makeCacheMatrix() and cacheSolve are two functions that works together
## to get the inverse of an invertible matrix 

## On one hand, makeCacheMatrix() creates a list of function to 
## set the value, get the value, set the inverse and
## retrieve the computed inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(solve) m <<- solve
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## cacheSolve(), on the other hand calculates the
## inverse of the matrix created with the makeCacheMatrix function.
## However, it first checks if the inverse of the matrix has been
## computed already. If it was, it retrieve the cached inverse of the matrix,
## skipping the computation. Otherwise, it performs the computation

cacheSolve <- function(x, ...) {
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m
}
