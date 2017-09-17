
## These two functions: makeCacheMatrix and cacheSolve will produce an
## inverse matrix of a given matrix

## makeCacheMatrix will take a matrix object and find and store its 
## inverse, and return a list of the matrix information 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    if (det(x) == 0) {
        message("matrix is non-invertible")
        return(inv)
    }
    get <- function() x
    setInverse <- function(solve) inv <<- solve
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve takes a list object from makeCacheMatrix and returns the 
## matrix's inverse or cached data of the matrix's inverse if it already
## was calculated 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
