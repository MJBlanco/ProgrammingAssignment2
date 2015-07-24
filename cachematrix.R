## This set of formulas will be used to calculate the inverse 
## of a square matrix and caching its result to avoid unnecessary
## computing

## Through this function a special matrix is created that
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
          x <<- y
          s <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Through this function the calculation of the inverse of the matrix
## created before is returned.
## If the matrix has not been changed and its inverse has been previously calculated, 
## then this formula returns the previuosly calculated result and a message.

cacheSolve <- function(x, ...) {
        s <- x$getinverse()
        if(!is.null(s)) {
          message("getting cached data")
          return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}
