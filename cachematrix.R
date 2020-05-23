## Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than computing it
#repeatedly. This function is created to cache the inverse of a matrix.


#1.  set the value of the "matrix"
#2.  get the value of the "matrix"
#3.  set the inverse of the "matrix"
#4.  get the inverse of the "matrix"


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        setmatrix <- function(y) {
                x <<- y
                i <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, 
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
# the function above. If the inverse has already been calculated (and the matrix has not changed)
# then `cacheSolve` retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$getmatrix()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}


