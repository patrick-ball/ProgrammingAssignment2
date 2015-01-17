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


cacheSolve <- function(x, ...) {
    ## This function accepts the list returned by makeCacheMatrix()
    ## This function then attempts to retrieve the cached inverse of the matrix
    ## originally passed to makeCacheMatrix().
    ## If that cached inversed exists, it is returned.
    ## If not, this function solves, caches, and returns the inverse.
    
    i <- x$getinv()
    if(!is.null(i)) {
        message("retrieving cached inverse")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}