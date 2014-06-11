## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here).
##` This pair of function cache the inverse of a matrix.
## AT THE END OF THIS SCRIPT THERE ARE THE INSTRUCTIONS FOR TEST THE TWO FUNCTIONS

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m  # Return a matrix that is the inverse of 'x'
}

## How test it:

## matr <- matrix( c(0, 2, 2, 0 ), 2, 2)
## m <- makeCacheMatrix()
## m$set(matr)
## m$get()
## cacheSolve(m)
## cacheSolve(m) # obtain the message
## m$set( matrix( rnorm(4), 2, 2) ) # another try
## m$get()
## cacheSolve(m)
## cacheSolve(m) # obtain the message