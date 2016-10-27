## This module provides a mechanism for caching the results of functions so that 
## two or more calls to the function cacheSolve with the same inputs will return a cached response.


## This function creates and returns a list of four functions, set, get, setSolvedMatrix and getSolvedMatrix.  
## set stores a matrix whose inverse is required
## get returns the stored matrix whose inverse is required
## setSolvedMatrix stores the solved matrix
## getSolvedMatrix returns the solved matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolvedMatrix <- function(solvedMatrix) m <<- solvedMatrix
    getSolvedMatrix <- function() m
    list(
        set = set,
        get = get,
        setSolvedMatrix = setSolvedMatrix,
        getSolvedMatrix = getSolvedMatrix
    )
}


## This function uses the four functions of the above makeCacheMatrix object to cache the value of a solved matrix.
## If a solved matrix is available, it is immediately returned.
## If a solved matrix is not available, it is calculated, stored in the makeCacheMatrix object and then the solved matrix is returned.
cacheSolve <- function(x, ...) {
    m <- x$getSolvedMatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolvedMatrix(m)
    m
}
