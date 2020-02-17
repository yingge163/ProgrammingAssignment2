## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than computing it repeatedly (there
## are also alternatives to matrix inversion that we will not discuss here). So
## I write a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## set the value of the inverse of matrix to NULL
    s <- NULL
    
    ## set the value of the matrix
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    ## get the value of the matrix
    get <- function() x
    
    ## set the value of the inverse of matrix
    setsolve <- function(solve) s <<- solve
    
    ## get the value of the inverse of matrix
    getsolve <- function() s
    
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)

}


## cacheSolve function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## get before inverse value
    s <- x$getsolve()
    
    ## if we have solve value in cache  return the value  
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    
    # otherwise we solve the inverse value of matrix
    data <- x$get()
    s <- solve(data)
    x$setsolve(s)
    s
    
}
