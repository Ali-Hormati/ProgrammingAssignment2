
## This work is going to make a special version of a matrix, which is a list of four functions, to be able to store a 
## cache of the inverse of the matrix and not re-calculate it if the matrix has not been changed.

## The function makeCacheMatrix accepts a matrix input and makes a list of four functions to set, get,
## getSolve and setSolve. If the inverse is already computed and the matrix has not changed, it will 
## return the cached value
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(val) inv <<- val
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'. If the matrix is made from makeCacheMatrix, the inverse is not recomputed
## and is restored from the cache.
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
