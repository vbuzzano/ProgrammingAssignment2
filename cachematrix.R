# Cache Matrix related functions.
# - makeCacheMatrix to create a new Cache Matrix
# - cacheSolve to solve the a matrix inverse

## Create and return a new Matrix Object
## This Matrix can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ix <- NULL
    set <- function(m) {
        x  <<- m
        ix <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) ix <<- inv
    getInverse <- function() ix
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function to inverse a Matrix, and Cache the result.
## Return the inverse of x, or a previous cached inversed result.

cacheSolve <- function(x, ...) {
    ix <- x$getInverse()
    if(!is.null(ix)) {
        message("getting cached data")
        return(ix)
    }
    data <- x$get()
    ix <- solve(data, ...)
    x$setInverse(ix)
    ix
}
