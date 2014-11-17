## A pair of functions enabling the calculation and caching of the
## inverse of a matrix, thus providing repeated access to the inverse 
## without paying the time of recomputing with each access.


## This function creates an object that caches the inverse of a matrix. 
## Methods are provided for the storage and retrival of both the matrix 
## and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
		## Store the matrix in x. Invalidate the inverse cache in i.
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function returns the inverse of the matrix stored in the
## makeCacheMatrix function. If the inverse was previously calculated, 
## and if the matrix has not changed, then cacheSolve will retrieve 
## the inverse from the cached value stored in makeCacheMatrix.
## Othewise it calculates and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'.
        i <- x$getinverse()
        if(!is.null(i)) {
		## A cached value is available.
                message("getting cached data")
                return(i)
        }
	## No cached value is available. Calculate the inverse.
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
