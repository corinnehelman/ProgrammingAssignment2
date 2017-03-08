

## This function calculates the inverse of a square matrix

## testing the code
##testmatrix <- makeCacheMatrix(matrix(1:4, 2, 2))
##
##testmatrix$get()
##       [,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> cachesolve(testmatrix)
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) inv <<- solve
    getsolve <- function() inv
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}
# this function returns the inverse of the inputed matrix
cachesolve <- function(x, ...) {
    inv <- x$getsolve()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setsolve(inv)
    inv
}

