## makeCacheMatrix creates a matrix whose inverse is stored when calculated through the cacheSolve function.

## cacheSolve calculates the inverse of a given cacheMatrix (created with the makeCacheMatrix function).
## When calculated, the inverse is stored at the chacheMatrix. If the matrix doesn't change, the value
## from the inverse matrix will be gotten from the cached value.


## Parameter: Matrix whose inverse will be cached.
## Returns a function list that represents the cacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) { ## If the matrix is changed, the inverse value is set to null 
        x <<- y          ## so next time we try to cacheSolve, the inverse is recalculated
        inverse <<- NULL
    }
    get <- function() x ## Returns the matrix stored at the cacheMatrix.
    setInverse <- function(inv) inverse <<- inv ## Stores the inverse matrix.
    computeInverse <- function(matrix) solve(matrix) ## Computes the inverse of the stored matrix.
    getInverse <- function () inverse ## Returns the stored value from the matrix.
    list(set = set, get = get, setInverse = setInverse, computeInverse = computeInverse, getInverse = getInverse)
}
## NOTE: In the example given (vector mean), the mean function is called directly from cacheSolve.
## Here, the solve function that calculates the inverse is included in the cacheMatrix.


## Parameter: cacheMatrix.
## Returns the inverse of the given cacheMatrix.

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) { ## If the inverse is already calculated, it's taken from the cached value
        message("getting cached data")
        return(inverse)
    }
    ## If the inverse isn't calculated (first attempt to get the inverse or the matrix has changed)
    matrix <- x$get()
    inverse <- x$computeInverse(matrix)
    x$setInverse(inverse)
    x$getInverse()
}
