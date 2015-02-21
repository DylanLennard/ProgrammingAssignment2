## The two functions together make and cache the inverse of a matrix, 
## and check to see if one already exists.  

## makeCacheMatrix generates a list with values assigned to it
## These values are the actual matrix, and the inverse if it's been calculated. 
## Also, this function assigns the matrix and its inverse to the global environment
## So that they are accessible outside of the function. 

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    setmatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getmatrix <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(setmatrix = setmatrix, getmatrix = getmatrix,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Cache solve looks at a list provided as input, and determines if there is
## or is not an inverse already stored. IF there is, it is returned, if not,
## it is calculated, and then returned.  

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$getmatrix()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
