## The below functions create a matrix and then compute the inverse
## of that matrix storing it in cache

## This function creates a matrix object that can cache its inverse 
## based on user supplied parameters  
## e.g x <- makeCacheMatrix(matrix(c(1:4), nrow=2, ncol=2))

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setinverse <- function(solve) {
        m <<- solve
    }
    
    getinverse <- function() {
        m
    }
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the matrix object from the 
## makeCacheMatrix function, e.g cacheSolve(x). If it's already been 
## computed it's retrieved from cache.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
