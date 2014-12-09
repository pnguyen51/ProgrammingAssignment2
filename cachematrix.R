
#Functions for caching the inverse of a matrix


# Creates a list of functions to set the value of the matrix, get the value of the matrix,
# set the value of the inverse, and get the value of the inverse
# Assume that the input matrix x is invertible.
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL

    set <- function(y) {
            x <<- y
            m <<- NULL
    }
    get <- function() x

    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m

    list(set = set, get = get,
         setInverse = setInverse, getInverse = getInverse)
}


# Calculates the inverse using the output list from the function above.
# If the inverse was already calculated, the cached value is returned by getInverse. Otherwise,
# the inverse is calculated by solve and set using setInverse.

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
            message("getting cached data")
            return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    m
}


