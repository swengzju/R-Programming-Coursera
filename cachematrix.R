## This program is used to create a special matrix that stores a square 
## matrix and cache's its inverse matrix

## a list of 4 functions in makeCacheMatrix:
## the first one returns the content of a matrix, 
## the second sets the content of the matrix, 
## the third returns the stored inversion of the matrix, 
## the fourth one sets the the content of the inverted matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## retrieve the inverse of the original matrix from the cache, or, 
## calculate it and store it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
