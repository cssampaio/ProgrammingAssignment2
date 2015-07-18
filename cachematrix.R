## Caching the Inverse of a Matrix

## makeCacheMatrix: cache the matrix inverse
## input: invertible matrix
## output: list of functions  to
##          1. set the value of the matrix
##          2. get the value of the matrix
##          3. set the value of the inverse
##          4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}


## cacheSolve: compute the inverse of a matrix
## input: output of makeCacheMatrix
## output: matrix inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
