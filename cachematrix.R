## A pair of functions that cache the inverse of a matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(X = matrix()) {
	i <- NULL
	set <- function(y) {
		X <<- y
		i <<- NULL
	}
	get <- function() X
	setinverse <- function(solve) i <<- solve
	getinverse <- function() i
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(X, ...) {
        i <- X$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- X$get()
        i <- solve(data, ...)
        X$setinverse(i)
        i
}


