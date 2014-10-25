## These functions are designed to return the inverse of a matrix
## As this is quite a lot of resources, we'll cache that reverse and return that
## if the matrix hasn't changed

## The makeCacheMatrix creates a list of functions to get and set the values of
## matrix and the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    r <- NULL
    set <- function(y) {
        x <<- y
        r <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) r <<- inverse
    getinverse <- function() r
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of a matrix in case it hasn't been
## calculated before

cacheSolve <- function(x, ...) {
    r <- x$getinverse()
    if(!is.null(r)) {
        message("getting cached data")
        return(r)
    }
    data <- x$get()
    r <- solve(data, ...)
    x$setinverse(r)
        ## Return a matrix that is the inverse of 'x'
    r
}
