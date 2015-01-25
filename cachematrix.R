## Put comments here that give an overall description of what your
## functions do

## Create an object to store a matrix and cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
    ## Initially set the inverse to NULL
	inv <- NULL
	## Set the matrix value and clear the inverse
	set <- function(y_mat) {
	        ## Store the data for the matrix in the parent environment
	        x <<- y_mat
			inv <<- NULL
	}
	## Get the current matrix data
	get <- function() x
	## Set the inverse of the matrix (calculated elsewhere
	setinv <- function(inv_mat) inv <<- inv_mat
	## Get the inverse of the matrix
	getinv <- function() inv
	list(set = set, get = get,
	     setinv = setinv, getinv = getinv)
}


## This function will return an inverted matrix. 
## If the matrix has already had an inverse created,
## it will return a cached copy rather than recreating 
## the inverse.

cacheSolve <- function(x, ...) {
    ## Get the stored inverse of 'x'
    inv <- x$getinv()
	## If the returned inverse is not null, use that
    if(!is.null(inv)) {
        ## Tell them we are using a cached inverse
		message("getting cached data")
		## Return the cached inverse and exit
		return(inv)
    }

	## Get the stored matrix data
	mat <- x$get()
	## Use the solve function to calculate the inverse
	inv <- solve(mat)
	## Cache the calculated inverse
	x$setinv(inv)
	## Return the inverse
	inv
}
