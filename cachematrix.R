## This file contains two functions that allow for matrix inversion
## We have created a special matrix object that is able to cache 
## the inverse of the matrix. This prevents the cost associated with
## repeated matrix inversion calculations.


## Function creates a special "matrix" which is able to cache its 
## inverse.
## There are four functions that are part of makeCacheMatrix:
## 1. get - get the underlying matrix
## 2. set - set the underlying matrix
## 3. getinverse - get the inverse of the matrix
## 4. setinverse - set the inverse of the matrix
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


## Function takes a matrix and uses the solve function to get
## the inverse of the matrix. Since, it is expensive to do 
## this operation. It relies on a special matrix created using
## makeCacheMatrix. This enables the function to cache its 
## results. If the cached results exist, it returns that results
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  m <-x$getinverse()
	  if(!is.null(m)) {
		message("getting cached data")
		return(m)
	  }
	  data <- x$get()
	  m <- solve(data, ...)
    x$setinverse(m)
	  m
}
