## The following functions use the cache to make matrix inversion less costly.

## The makeCacheMatrix function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix function above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}
