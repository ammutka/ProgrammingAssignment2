## These functions are for caching the inverse of a matrix

## function that makes a matrix object that can cache its inverse
makeCacheMatrix <- function(x) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) inv <<- matrix
        getmatrix <- function() inv
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

## function that computes the inverse of a matrix object created by the
## makeCacheMatrix function, or that returns a cached inverse if the
## calculation has been done previously
cacheSolve <- function(x, ...) {
        inv <- x$getmatrix()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setmatrix(inv)
        ## Return a matrix that is the inverse of 'x'
	inv
}
