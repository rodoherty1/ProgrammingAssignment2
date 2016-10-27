## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
