## These functions are used to calculate the inverse of a matrix and cache it.
## Assignment 2 0f rprog-030 - Jul 2016

## This function creates a special "matrix" object whih is the inverse of the argument passed in and caches it

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	setter <- function(y) {
		x <<- y
		m <<- NULL
	}
	getter <- function() x
	setInverse <- function(solve) m <<- solve
	getInverse <- function() {
	 return(m)
	}
	list(set = setter, get = getter,
	setinverse = setInverse,
	getinverse = getInverse)
}

## This function computes the inverse of the special "matrix" and 
## if the same has already been calculated , then the function
## retrieves the inverse from the cache 

cacheSolve <- function(x, ...) {
		m <- x$getinverse()
		if(!is.null(m)) {
			message("From cached data")
			return(m)
		}
		data <- x$get()
		print(data)
		m <- solve(data, ...)
		x$setinverse(m)
		m
}