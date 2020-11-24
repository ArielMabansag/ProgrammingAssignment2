##Week 3 Assignment R Programming
##This function creates a matrix object that cache its inverse
CacheMatrix <- function(x = matrix()) {
        n <- NULL
        set <- function(y) {
                x <<- y
                n <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) n <<- inverse
        getInverse <- function() n
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function calculates the inverse of the matrix returned by CacheMatrix.
## If the inverse has been calculated, the cacheSolve will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        n <- x$getinverse()
        if(!is.null(n)) {
          message("getting cached data")
          return(n)
        }
        data <- x$get()
        n <- solve(data, ...)
        x$setinverse(n)
        n
}

