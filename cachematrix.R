## This pair of functions cache the inverse of a matrix
## Note that the matrix is assumed to be Invertable

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    I <- NULL              # I represents the inverse matrix and is set to null when the "matrix" object is created
    set <- function(y) {   # function to set the "matrix" object and set the inverse to null
        x <<- y
        I <<- NULL
    }
    get <- function() x    # function to return the matrix
    setInverse <- function() I <<- solve(x)  ## function to solve the matrix and cache the inverse matrix
    getInverse <- function() I               ## function to return the inverse
    list( set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated, then the inverse will be retrieved from the cache

cacheSolve <- function(x, ...) {
    I <- x$getInverse()                  ## get the inverse matrix, if cached
    if(!is.null(I)) {                    ## if inverse has been cached, return it
        message("getting cached data")
        return(I)
    }
    x$setInverse()                       ## otherwise solve it and cache the inverse
    x$getInverse()                       ## and return inverse matrix
}
