## Functions for getting the inverse of a matrix with caching

## Function for get/set the matrix, set and store it's inverse
makeCacheMatrix <- function( m = matrix() ) {
        
        i <- NULL
        
        get <- function() m
        
        set <- function( matrix ) {
                m <<- matrix
                i <<- NULL
        }
        
        setInverse <- function(inverse)  i <<- inverse
        
        
        getInverse <- function() i
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

#returns the inverse matrix, by calculation or the cached version
cacheSolve <- function(x, ...) {
        
        m <- x$getInverse()
        
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        
        library(MASS)
        m <- ginv(data) %*% data
        
        x$setInverse(m)
        
        m
}

Sys.time()
set.seed(1)
mat1 <- matrix(rnorm(9), 3,3)
mat <- makeCacheMatrix(mat1)
cacheSolve(mat)
Sys.time()
