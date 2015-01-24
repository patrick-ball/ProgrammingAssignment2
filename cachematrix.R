## This set of functions will be used to check for, create, store, and retrieve
## the cached inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    ## This function takes a matrix as input and returns a list of four
    ## functions used to store and retrieve the cached inverse of the matrix
    
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() {
        x
    }
    setinv <- function(inv) {
        i <<- inv
    }
    getinv <- function() {
        i
    }
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


cacheSolve <- function(x, xCache, ...) {
    ## This function accepts a matrix and the list returned by makeCacheMatrix()
    ## This function then checks to see if x matches the matrix
    ## originally passed to makeCacheMatrix().
    ## If so, the caches inverse is returned.
    ## If not, this function updates the cached data and returns the inverse
    
    if(identical(x, xCache$get)) {
        message("retrieving cached inverse")
        return(xCache$getinv)
    }
    xCache$set(x)
    i <- solve(x, ...)
    x$setinv(i)
    i
}
