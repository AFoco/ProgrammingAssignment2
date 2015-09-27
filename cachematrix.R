## These functions create an object that can calculate and store the
## value of the inverse of an invertible matrix.


## This function create a 'special' matrix that can cache
## the value of its inverse.
## It defines a list of functions that operate (get or set)
## on the matrix and its cached inverse.

makeCacheMatrix <- function(x = matrix()) {
        cachedInverse <- NULL
        set <- function(y) {
                x <<- y
                cachedInverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) cachedInverse <<- inverse
        getinverse <- function() cachedInverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## This function calculates the inverse of the special "matrix" created
## with the makeCacheMatrix() function.
## It first checks to see if the inverse has already been calculated. If so,
## it gets the inverse from the cache and skips the computation. Otherwise,
## it calculates the inverse of the matrix and sets the value in the cache
## via the setinverse() function.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}

