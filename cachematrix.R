# Cache Matrix related functions.
# - makeCacheMatrix to create a new Cache Matrix
# - cacheSolve to solve the a matrix inverse

## Create a special "matrix" object that can cache its inverse.

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


## Computes and return the inverse of a special "matrix" object. If the inverse has already
## been calculated (and the "matrix" has not changed), return the inverse 
## from the cache.

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
