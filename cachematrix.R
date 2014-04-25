##  These two functions work together in order to cache a matrix and to 
## pull the cached matrix out without having to do a computation should 
## you try to find the inverse of the matrix again. 


## The first function establishes a list of functions for the "cache matrix",
## which is the matrix that is used to cache the matrix in the second function.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function will either retrieve the cached matrix if it is in the cache,
## or it will compute the inverse of the matrix without pulling it from the cache.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
                        
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
