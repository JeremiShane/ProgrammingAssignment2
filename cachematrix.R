# Caching the Inverse of a Matrix 
## assignment is to write a pair of functions that cache the inverse of a matrix
## 1. makeCacheMatrix creates a special "matrix" object that can cache its inverse
## 2. cacheSolve computes the inverse of the special "matrix" object, or retrieves the cached inverse
##
### the inverse of A is A^-1 only when
### A * A^-1 = A^-1 * A = I (the identity matrix)

## Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y=c(1,1,1,0), d=2) {
		## for inverse to work the matrix must be square, and nonsingular
		## a square matrix is singular if and only if its determinant is 0
		x <<- matrix(y, d, d)
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) inv <<- solve
	getinverse <- function() inv
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) ## maybe should be matrix
}


## Check if "matrix" object inverse is already cached, if so retrieve it, else computes the inverse

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
## Return a matrix that is the inverse of 'x'
}
