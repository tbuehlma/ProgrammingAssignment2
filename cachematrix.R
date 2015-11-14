## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
## makeCacheMatrix takes as input a matrix and provides the ability to cache
## the value for faster retrieval later on. 
makeCacheMatrix <- function(x = matrix()) {
	iM <- NULL
	set <- function(y) { #assign matrix 
		x <<- y
		iM <<- NULL # and invalidate inverse value
	}
	get <- function() x # return matrix
	setinverse <- function (inverse) iM <<- inverse
	getinverse <- function () iM
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		iM <- x$getinverse() # get the cached inverse value
		if (!is.null(iM)) { # if it is not NULL, we are using the cached value
			message("getting cached data")
		}
		else { # otherwise get the data and compute inverse of matrix then (re-)cache it
			data <- x$get()
			iM <- solve(data, ...)
			x$setinverse(iM)
		}
		iM
}
