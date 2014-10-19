## Functions used to cache a matrix and its inverse. 

## Creates a special "cache matrix" which is really a list containing 4 functions to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() { x }
    setinverse <- function(inverse) { inv <<- inverse }
    getinverse <- function() { inv }
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Computes the inverse of a cache matrix in two steps:
## - first checking if the inverse has already been computed and return it
## - otherwise, compute the inverse, cache it in the dedicated slot of
##   the cache matrix and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    ## Access cached inverse
    inv <- x$getinverse()
    if (!is.null(inv)) { ## inverse has already been computed and is cached
        message("getting cached inverse")
    } else { ## inverse has never been computed before
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv)
    }
    return(inv) ## either cached or locally computed solution
}

## Test functions
mat <- diag(1:4)
x <- makeCacheMatrix(mat)
## First call to cacheSolve, inverse has not been cached yet
cacheSolve(x)
## Second call to cacheSolve, inverse has not already been cached
cacheSolve(x)
